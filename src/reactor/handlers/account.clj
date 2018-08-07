(ns reactor.handlers.account
  (:require [blueprints.models.account :as account]
            [blueprints.models.approval :as approval]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.license :as license]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.property :as property]
            [blueprints.models.unit :as unit]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :as string]
            [customs.auth :as auth]
            [datomic.api :as d]
            [hubspot.contact :as contact]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [ring.util.codec :refer [form-encode]]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]
            [teller.payment :as tpayment]
            [teller.customer :as tcustomer]
            [teller.property :as tproperty]
            [reactor.utils.tipe :as tipe]
            [reactor.config :as config]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn- unit-link [hostname unit]
  (let [property (unit/property unit)
        url      (format "%s/communities/%s/units/%s"
                         hostname (:db/id property) (:db/id unit))]
    (sm/link url (:unit/name unit))))


(defn- property-link [hostname property]
  (let [url (format "%s/communities/%s" hostname (:db/id property))]
    (sm/link url (property/name property))))


;; =============================================================================
;; Account Creation
;; =============================================================================


(defmethod dispatch/notify :account/create [deps event {:keys [email]}]
  (let [account (account/by-email (->db deps) email)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Activate Your Account")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Thanks for signing up!")
      (mm/p (format "<a href='%s/signup/activate?email=%s&hash=%s'>Click here to activate your account</a> and apply for a home."
                    (->public-hostname deps)
                    (form-encode (account/email account))
                    (account/activation-hash account)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defn- create-account
  [{:keys [email first-name middle-name last-name password]}]
  (assert (some? email) "`email` is required.")
  (assert (some? first-name) "`first-name` is required.")
  (assert (some? last-name) "`last-name` is required.")
  (assert (some? password) "`password` is required.")
  (tb/assoc-when
   {:db/id                   (d/tempid :db.part/starcity)
    :account/email           (string/trim email)
    :account/first-name      (-> first-name string/trim string/capitalize)
    :account/last-name       (-> last-name string/trim string/capitalize)
    :account/password        password
    :account/activation-hash (auth/make-activation-hash email)
    :account/activated       false
    :account/role            :account.role/applicant}
   :account/middle-name (when-some [x middle-name]
                          (-> x string/trim string/capitalize))))


(defmethod dispatch/job :account/create [_ event params]
  [(create-account params)
   (event/notify :account/create
                 {:params       (select-keys params [:email :first-name :last-name])
                  :triggered-by event})])


;; ==============================================================================
;; promotion ====================================================================
;; ==============================================================================


;; create first-month's rent ====================================================


(defn- prorated-amount [start rate]
  (let [start          (c/to-date-time start)
        days-in-month  (t/day (t/last-day-of-the-month start))
        ;; We inc the days-remaining so that the move-in day is included in the calculation
        days-remaining (inc (- days-in-month (t/day start)))]
    (tb/round (* (/ rate days-in-month) days-remaining) 2)))


(defmethod dispatch/job ::create-first-months-rent [deps event params]
  (let [account   (d/entity (->db deps) (:account-id params))
        community (account/current-property (->db deps) account)
        property  (tproperty/by-community (->teller deps) community)
        customer  (tcustomer/by-account (->teller deps) account)
        license   (member-license/active (->db deps) account)
        tz        (member-license/time-zone license)
        pstart    (member-license/starts license)
        amount    (prorated-amount pstart (member-license/rate license))]
    (tpayment/create! customer amount :payment.type/rent
                      {:period   [pstart (date/end-of-month pstart tz)]
                       :status   :payment.status/due
                       :due      pstart
                       :property property})
    nil))


;; hubspot close date ===========================================================


(defmethod dispatch/job ::set-hubspot-close-date [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        contact (-> account account/email contact/fetch)]
    (if-let [error (:error contact)]
      (timbre/warn ::set-hubspot-close-date-error {:account (account/email account)
                                                   :data    error})
      (let [now (date/tz-uncorrected (java.util.Date.)
                                     (t/time-zone-for-id "America/Los_Angeles"))]
        (contact/update! (:canonical-vid contact)
                         {:closedate (.getTime now)})))))


;; helping hands order summary =================================================


(defn- fmt-order [index order]
  (let [{:keys [name desc price quantity billed]} (order/clientize order)
        price (if-some [p price] (str "$" price "/ea") "Quote")]
    (cond
      (some? quantity)
      (format "%d. [ `%s` | _%s_ | *x%s* ] _%s_ (billed %s)"
              (inc index)
              (string/upper-case name)
              price
              (int quantity)
              desc
              (clojure.core/name billed))

      :otherwise
      (format "%d. [ `%s` | _%s_ ] _%s_ (billed %s)"
              (inc index)
              (string/upper-case name)
              price
              desc
              (clojure.core/name billed)))))


(defmethod dispatch/report :account.promoted/order-summary [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        orders  (order/orders (->db deps) account)]
    (when-not (empty? orders)
      (slack/send
       (->slack deps)
       {:uuid    (:event/uuid event)
        :channel slack/crm}
       (sm/msg
        (sm/info
         (sm/title (format "%s ordered premium services:" (account/full-name account))
                   (account-link (->dashboard-hostname deps) account))
         (sm/text (->> (map-indexed fmt-order orders) (interpose "\n") (apply str)))
         (sm/fields
          (sm/field "Account" (account-link (->dashboard-hostname deps) account)))))))))


;; report ======================================================================


(defmethod dispatch/report :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        license (member-license/active (->db deps) account)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/text (format "*%s* is now a member!" (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account-link (->dashboard-hostname deps) account) true)
        (sm/field "Unit" (unit-link (->dashboard-hostname deps) (member-license/unit license)) true)))))))


;; notify ======================================================================


(defn- ^:private promotion-email-subject [property]
  (mail/subject (format "Welcome to %s!" (property/name property))))


(defn- promotion-email-body [hostname account]
  (mm/msg
   (mm/greet (account/first-name account))
   (mm/p "Thank you for signing your membership agreement and submitting
      payment for your security deposit. <b>Now you're officially a Starcity
      member!</b> We're looking forward to having you join the community and
      can't wait to get to know you better.")
   (mm/p (format "The next step to getting settled in is to log into your <a
      href='%s'>member dashboard</a>. This is where you'll be able to pay
      your rent payments and the remainder of your security deposit (if
      applicable). You can choose to <a href='%s/profile/payments/sources'>enable
      autopay</a> or make individual rent payments going forward." hostname hostname))
   (mm/p "Please let us know if you have any questions about the move-in
      process or need assistance navigating the dashboard.")
   mail/community-sig))


(defn- send-promotion-email [deps account event]
  (let [property (-> (member-license/active (->db deps) account) member-license/property)]
    (mailer/send (->mailer deps) (account/email account)
                 (promotion-email-subject property)
                 (promotion-email-body (->dashboard-hostname deps) account)
                 {:from mail/from-community
                  :uuid (event/uuid event)})))


(defmethod dispatch/notify :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))]
    (send-promotion-email deps account event)))


(defmethod dispatch/job :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        license (member-license/active (->db deps) account)]
    (if-not (some? license)
      (throw (ex-info "Member has no active license!" {:account (account/email account)}))
      [(event/notify :account/promoted
                     {:params params :triggered-by event})

       (event/report :account/promoted
                     {:params params :triggered-by event})

       (event/report :account.promoted/order-summary
                     {:params params :triggered-by event})
       (event/job ::create-first-months-rent
                  {:params params :triggered-by event})
       (event/job ::set-hubspot-close-date
                  {:params params :triggered-by event})])))


;; =============================================================================
;; Approve
;; =============================================================================


(defmethod dispatch/report :account/approved [deps event {account-id :account-id}]
  (let [approvee (d/entity (->db deps) account-id)
        approval (approval/by-account approvee)
        approver (approval/approver approval)
        unit     (approval/unit approval)
        license  (approval/license approval)
        property (unit/property unit)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/text
        (format "*%s* has approved *%s* for membership! _Don't forget to send an email to let %s know that they've been approved._"
                (account/full-name approver)
                (account/full-name approvee)
                (account/first-name approvee)))
       (sm/fields
        (sm/field "Email" (account/email approvee) true)
        (sm/field "Property" (property-link (->dashboard-hostname deps) property) true)
        (sm/field "Unit" (unit-link (->dashboard-hostname deps) unit) true)
        (sm/field "Move-in" (date/short-date (approval/move-in approval)) true)
        (sm/field "Term" (format "%s months" (license/term license)) true)))))))


(defmethod dispatch/job :account/approved [deps event {account-id :account-id :as params}]
  (let [account (d/entity (->db deps) account-id)]
    [(event/report (event/key event) {:params params})
     (assoc (events/revoke-session account-id) :event/triggered-by (td/id event))]))


;; =============================================================================
;; Reset Password
;; =============================================================================


(defmethod dispatch/notify :account/reset-password
  [deps event {:keys [account-id new-password]}]
  (let [account (d/entity (->db deps) account-id)
        link    (format "%s/login?email=%s&next=/account"
                        (->public-hostname deps) (form-encode (account/email account)))]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Password Reset")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "As requested, we have reset your password. Your temporary password is:")
      (mm/p (str "<b>" new-password "</b>"))
      (mm/p
       (format "After logging in <a href='%s'>here</a>, please change your password to something more memorable by clicking on <b>My Account</b> in the upper right-hand corner of the page." link))
      (mm/p "If this was not you, please contact us at <a href='mailto:team@starcity.com'>team@starcity.com</a>.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :account/reset-password [deps event {account-id :account-id}]
  (let [account                (d/entity (->db deps) account-id)
        [new-password tx-data] (auth/reset-password account)]
    [tx-data
     (event/notify (event/key event) {:params       {:new-password new-password
                                                     :account-id   account-id}
                                      :triggered-by event})]))


;; ==============================================================================
;; playground ===================================================================
;; ==============================================================================


(defmethod dispatch/notify ::send-slack-notification
  [deps event {account-id :account-id}]
  (let [account (d/entity (->db deps) account-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/text
        (format "Changed %s's first name to Cece!" (account/email account))))))))


(defmethod dispatch/job ::change-first-name-to-cece
  [deps event {account-id :account-id :as params}]
  (let [account (d/entity (->db deps) account-id)]
    (assert (not= (account/first-name account) "Cece")
            "Name is already Cece!")
    [[:db/add account-id :account/first-name "Cece"]
     (event/notify ::send-slack-notification {:params       params
                                              :triggered-by event})]))


;; ==============================================================================
;; ceo welcome email ============================================================
;; ==============================================================================


;; - When should the message go out?
;;   - one day after move-in
;;   - TODO: How do we tell when someone moved in?
;; - TODO: Name and email of person that moved in
;; - DONE: need email copy
;; - DONE: Should be sent from jon@starcity.com


(def ^:private ceo-welcome-email-document-id
  "5b439a6bc7e97000137f4897")


;; (defn prepare-renewal-email
;;   [document account license]
;;   (tb/transform-when-key-exists document
;;     {:body (fn [body]
;;              (-> (stache/render body {:name   (account/first-name account)
;;                                       :ends   (date/short (member-license/ends license))
;;                                       :sender "Starcity Community"})
;;                  (md/md-to-html-string)))}))


(defmethod dispatch/notify ::send-ceo-welcome-email
  [deps event {:keys [account-id] :as params}]
  (let [content (tipe/fetch-document (->tipe deps) ceo-welcome-email-document-id)]
    (mailer/send
     (->mailer deps)
     "todo@todo.com"
     (mail/subject (:subject content))
     (mm/msg (:body content) (:signature content))
     {:uuid (event/uuid event)
      :from (:from content)})))


(defn- all-active-licenses
  [db]
  (map
   (fn [entity-id]
     (d/entity db entity-id))
   (d/q '[:find [?ml ...]
          :where
          [?ml :member-license/status :member-license.status/active]]
        db)))


(defn- commenced-yesterday?
  [member-license as-of]
  (let [commencement (member-license/commencement member-license)
        tz           (member-license/time-zone member-license)]
    (= 1
       (t/in-days
        (t/interval
         (c/to-date-time
          (-> (date/beginning-of-day commencement tz)
              (date/tz-uncorrected tz)))
         (c/to-date-time
          (-> (date/beginning-of-day as-of tz)
              (date/tz-uncorrected tz))))))))


(defn- only-one-license? [member-license]
  (= 1 (count (:account/licenses (member-license/account member-license)))))


(defn- license->welcome-email-event [member-license]
  )


(defmethod dispatch/job :account/send-welcome-emails
  [deps event {t :t}]
  (->> (all-active-licenses (->db deps))
       (filter #(and (commenced-yesterday? % t)
                     (only-one-license? %)))
       (map license->welcome-email-event)))


(comment

  (do
    (def conn reactor.datomic/conn))


  (let [account (d/entity (d/db conn) [:account/email "member@test.com"])]
    (d/touch (member-license/active (d/db conn) account)))


  (let [account (d/entity (d/db conn) [:account/email "member@test.com"])]
    (d/transact conn [(event/job ::send-ceo-welcome-email
                                 {:params {:account-id (td/id account)}})]))


#_(defmethod dispatch/notify ::send-renewal-reminder
    [deps event {:keys [license-id days]}]
    (let [license     (d/entity (->db deps) license-id)
          account     (member-license/account license)
          document-id (get-renewal-reminder-email-document-id license)
          document    (tipe/fetch-document (->tipe deps) document-id)
          content     (prepare-renewal-email document account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (:subject content))
     (mm/msg (:body content) (or (:signature content) (mail/noreply-sig)))
     {:uuid (event/uuid event)
      :from (or (:from content) (mail/from-community))
      :bcc  (when (config/production? config) mail/community-address)})))



  ;; TOPIC: job, report, notify
  ;; - job: could do anything
  ;; - notify: sending emails to members
  ;; - report: sending slack messages to us

(let [account (d/entity (d/db conn) [:account/email "member@test.com"])]
    (d/transact conn [(event/job ::change-first-name-to-cece
                                 {:params {:account-id (td/id account)}})]))

  (d/transact conn [[:db/add [:account/email "member@test.com"] :account/first-name "Ethan"]])


  (d/transact conn [{:db/id        285873023223289
                     :event/status :event.status/pending}])

  )
