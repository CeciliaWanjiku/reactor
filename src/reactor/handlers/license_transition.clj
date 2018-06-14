(ns reactor.handlers.license-transition
  (:require [blueprints.models.account :as account]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.license-transition :as license-transition]
            [blueprints.models.unit :as unit]
            [blueprints.models.event :as event]
            [clostache.parser :as stache]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [markdown.core :as md]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [reactor.utils.tipe :as tipe]
            [toolbelt.datomic :as td]
            [toolbelt.date :as date]
            [taoensso.timbre :as timbre]
            [blueprints.models.property :as property]
            [toolbelt.core :as tb]
            [clj-time.core :as t]
            [clj-time.coerce :as c]))


(defn- member-url [hostname account-id]
  (format "%s/accounts/%s" hostname account-id))


(defn- make-friendly-unit-name
  [unit]
  (let [code        (unit/code unit)
        property    (property/name (unit/property unit))
        unit-number (subs code (inc (clojure.string/last-index-of code "-")))]
    (str property " #" unit-number)))


(def ^:private property-channel
  {"52gilbert"   "#52-gilbert"
   "2072mission" "#2072-mission"
   "6nottingham" "#6-nottingham"
   "414bryant"   "#414-byant"})


(defn- notification-channel [property]
  (let [code (property/code property)]
    (get property-channel code slack/crm)))


;; slack notification -> staff
(defmethod dispatch/report :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition (license-transition/by-uuid (->db deps) transition-uuid)
        license    (license-transition/current-license transition)
        member     (member-license/account license)
        unit       (member-license/unit license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " is moving out!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's move out in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit) true)
        (sm/field "Move-out date" (date/short (license-transition/date transition)) true)
        (when-let [a (:asana/task transition)]
          (sm/field "Asana Move-out Task" a true))))))))


(def move-out-after-30-days-email-document-id
  "The Tipe document id for the email sent to members who are moving out when
  their move-out date is beyond 30 days from the date they gave notice"
  "5b219382fbc48500130b9e56")


(def move-out-before-30-days-email-document-id
  "The Tipe document id for the email set to members who are moving out when their
  move-out date is within 30 days of the date they gave notice"
  "5b216533401e390013aeeefa")


(defn prepare-move-out-after-30-days-email
  [document member admin transition]
  (tb/transform-when-key-exists document
    {:subject (fn [subject] (stache/render subject {:name (account/first-name member)}))
     :body    (fn [body] (-> (stache/render body {:name                  (account/first-name member)
                                                 :community-team-member (account/first-name admin)
                                                 :move-out-date         (date/short (license-transition/date transition))})
                            (md/md-to-html-string)))}))

(defn thirty-days-after-notice
  [transition]
  (-> (license-transition/notice-date transition)
      (c/to-date-time)
      (t/plus (t/days 30))
      (c/to-date)))


(defn prepare-move-out-before-30-days-email
  [document member admin transition]
  (let [license (license-transition/current-license transition)
        tz      (member-license/time-zone license)]
    (tb/transform-when-key-exists document
      {:subject (fn [subject] (stache/render subject {:name (account/first-name member)}))
       :body    (fn [body] (-> (stache/render
                               body {:name                  (account/first-name member)
                                     :community-team-member (account/first-name admin)
                                     :move-out-date         (date/short (date/tz-corrected (license-transition/date transition) tz))
                                     :notice-date           (date/short (date/tz-corrected (license-transition/notice-date transition) tz))
                                     :notice-date-plus-30   (date/short (date/tz-corrected (thirty-days-after-notice transition) tz))})
                              (md/md-to-html-string)))})))


(defn find-transition-creator
  "Given an active license, provide the account of the admin who created said license."
  [db license]
  (->> (d/q '[:find ?creator .
              :in $ ?member-license
              :where
              [?t :license-transition/current-license ?member-license ?tx]
              [_ :source/account ?creator ?tx]]
            db (td/id license))
       (d/entity db)))


(defn moveout-within-30-days-notice?
  [transition]
  (let [notice-date   (license-transition/notice-date transition)
        move-out-date (license-transition/date transition)
        interval      (t/in-days (t/interval (c/to-date-time notice-date) (c/to-date-time move-out-date)))]
    (< interval 30)))


;; email notification -> member
(defmethod dispatch/notify :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition  (license-transition/by-uuid (->db deps) transition-uuid)
        license     (license-transition/current-license transition)
        member      (member-license/account license)
        admin       (find-transition-creator (->db deps) license)
        admin-email (account/email admin)
        document    (tipe/fetch-document (->tipe deps) (if (moveout-within-30-days-notice? transition)
                                                         move-out-before-30-days-email-document-id
                                                         move-out-after-30-days-email-document-id))
        content     (if (moveout-within-30-days-notice? transition)
                      (prepare-move-out-before-30-days-email document member admin transition)
                      (prepare-move-out-after-30-days-email document member admin transition))]
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     (tb/assoc-when
      {:uuuid (event/uuid event)}
      :cc (when (not= admin-email "admin@test.com") admin-email)))))


(defmethod dispatch/job :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition (license-transition/by-uuid (->db deps) transition-uuid)]
    [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                      :triggered-by event})
     (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                      :triggered-by event})]))


(defmethod dispatch/report :transition/move-out-updated
  [deps event {:keys [transition-id]  :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (license-transition/current-license transition)
        member     (member-license/account license)
        unit       (member-license/unit license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str "Updated Move-out Info for " (account/short-name member))
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's move out in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit) true)
        (sm/field "Move-out date" (date/short (license-transition/date transition)) true)
        (when-let [a (:asana/task transition)]
          (sm/field "Asana Move-out Task" a true))))))))


(defmethod dispatch/job :transition/move-out-updated
  [deps event {:keys [transition-id] :as params}]

  [(event/report (event/key event) {:params {:transition-id transition-id}
                                    :triggered-by event})])


(defmethod dispatch/report :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :cahnnel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " has renewed their license!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's renewal in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit) true)
        (sm/field "New License Term" (member-license/term new-license) true)
        (sm/field "New License Rate" (member-license/rate new-license) true)
        (sm/field "Renewal date" (date/short (license-transition/date transition)) true)))))))


(def renewal-created-email-document-id
  "The Tipe document id for the renewal-created email template"
  "5b216e297ec99e0013175bbd")


(defn prepare-renewal-created-email
  [document account new-license]
  (tb/transform-when-key-exists document
    {:body (fn [body]
             (-> (stache/render body {:name (account/first-name account)
                                      :date (date/short (member-license/starts new-license))
                                      :term (str (member-license/term new-license))
                                      :rate (str (member-license/rate new-license))})
                 (md/md-to-html-string)))}))


(defmethod dispatch/notify :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        document        (tipe/fetch-document (->tipe deps) renewal-created-email-document-id)
        content         (prepare-renewal-created-email document member new-license)]
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     {:uuuid (event/uuid event)})))


(defmethod dispatch/job :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})])


;; Intra-community Transfer notifications =======================================


(defmethod dispatch/report :transition/intra-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        old-unit        (member-license/unit current-license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :cahnnel (notification-channel (unit/property old-unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " is transferring to a new unit!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's transfer in the Admin Dashboard.")
       (sm/fields
        (sm/field "Old Unit" (make-friendly-unit-name old-unit) true)
        (sm/field "New Unit" (make-friendly-unit-name (member-license/unit new-license)) true)
        (sm/field "New License Term" (member-license/term new-license) true)
        (sm/field "New License Rate" (member-license/rate new-license) true)
        (sm/field "Old Unit Move-out Date" (date/short (license-transition/date transition)) true)
        (sm/field "New Unit Move-in Date" (date/short (member-license/commencement new-license)) true)))))))


(def intra-xfer-created-document-id
  "The Tipe document id for the intra-xfer-created email template"
  "5b2183eba83135001307db90")


(defn prepare-intra-xfer-created-email
  [document member unit new-license current-license]
  (tb/transform-when-key-exists document
    {:subject (fn [subject] (stache/render subject {:name (account/first-name member)}))
     :body    (fn [body] (-> (stache/render body {:name          (account/first-name member)
                                                 :new-unit      (make-friendly-unit-name (member-license/unit new-license))
                                                 :old-unit      (make-friendly-unit-name unit)
                                                 :move-out-date (date/short (member-license/ends current-license))
                                                 :move-in-date  (date/short (member-license/starts new-license))
                                                 :term          (str (member-license/term new-license))
                                                 :rate          (str (member-license/rate new-license))})
                            (md/md-to-html-string)))}))



(defmethod dispatch/notify :transition/intra-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)
        document        (tipe/fetch-document (->tipe deps) intra-xfer-created-document-id)
        content         (prepare-intra-xfer-created-email document member unit new-license current-license)]
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :transition/intra-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})])




;; Inter-community Transfer notifications =======================================


(defmethod dispatch/report :transition/inter-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        old-unit        (member-license/unit current-license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :cahnnel (notification-channel (unit/property old-unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " is transferring to a new community!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's transfer in the Admin Dashboard.")
       (sm/fields
        (sm/field "Old Unit" (make-friendly-unit-name old-unit) true)
        (sm/field "New Unit" (make-friendly-unit-name (member-license/unit new-license)) true)
        (sm/field "New License Term" (member-license/term new-license) true)
        (sm/field "New License Rate" (member-license/rate new-license) true)
        (sm/field "Old Unit Move-out Date" (date/short (license-transition/date transition)) true)
        (sm/field "New Unit Move-in Date" (date/short (member-license/commencement new-license)) true)))))))


(def inter-xfer-created-document-id
  "The Tipe document id for the inter-xfer-created email template"
  "5b217d5775c33d0013c15145")

(defn prepare-inter-xfer-created-email
  [document member unit new-license current-license]
  (tb/transform-when-key-exists document
    {:subject (fn [subject] (stache/render subject {:name (account/first-name member)}))
     :body (fn [body] (-> (stache/render body {:name (account/first-name member)
                                              :new-unit (make-friendly-unit-name (member-license/unit new-license))
                                              :old-unit (make-friendly-unit-name unit)
                                              :move-out-date (date/short (member-license/ends current-license))
                                              :move-in-date (date/short (member-license/starts new-license))
                                              :term (str (member-license/term new-license))
                                              :rate (str (member-license/rate new-license))})
                         (md/md-to-html-string)))}))


(defmethod dispatch/notify :transition/inter-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)
        document        (tipe/fetch-document (->tipe deps) inter-xfer-created-document-id)
        content         (prepare-inter-xfer-created-email document member unit new-license current-license)]
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :transition/inter-xfer-created
  [deps event {:keys [transition-uuid] :as params}]
  [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})])


;; Month-to-month transition notifications =======================================


(defmethod dispatch/report :transition/month-to-month-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :cahnnel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " has been renewed for a month-to-month license!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's renewal in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit) true)
        (sm/field "New License Term" (member-license/term new-license) true)
        (sm/field "New License Rate" (member-license/rate new-license) true)
        (sm/field "Renewal date" (date/short (license-transition/date transition)) true)))))))


(def month-to-month-created-document-id
  "The Tipe document id for the month-to-month notice email"
  "5b2185ce17bbd50013fce7fa")


(defn prepare-month-to-month-created-email
  [document account license]
  (tb/transform-when-key-exists document
    {:subject (fn [subject] (stache/render subject {:name (account/first-name account)}))
     :body (fn [body] (-> (stache/render body {:name (account/first-name account)
                                              :date (date/short (member-license/starts license))
                                              :term (str (member-license/term license))
                                              :rate (str (member-license/rate license))})
                         (md/md-to-html-string)))}))


(defmethod dispatch/notify :transition/month-to-month-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)
        document        (tipe/fetch-document (->tipe deps) month-to-month-created-document-id)
        content         (prepare-month-to-month-created-email document member new-license)]

    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     {:uuid (event/uuid event)})))

(defmethod dispatch/job :transition/month-to-month-created
  [deps event {:keys [transition-uuid] :as params}]
  [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})])
