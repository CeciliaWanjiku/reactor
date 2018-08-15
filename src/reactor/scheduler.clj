(ns reactor.scheduler
  (:require [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [hara.io.scheduler :as sch]
            [mount.core :refer [defstate]]
            [reactor.config :as config :refer [config]]
            [reactor.datomic :refer [conn]]
            [reactor.hubspot-sync :as hubspot-sync]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]))

(defn last-day-of-month?
  [date]
  (let [date' (c/to-date-time date)]
    (not= (t/month date') (t/month (t/plus date' (t/days 1))))))


(defn- create-scheduler [conn]
  (let [params {:conn conn}]
    (sch/scheduler
     (tb/assoc-when
      ;; process any license transitions this month, then create rent payments as appropro
      {:conduct-monthly-operations
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(event/job :ops/first-of-month {:params {:t t}})]))
        ;; first of every month
        :params   params
        :schedule "0 0 0 * 1 * *"}

       :end-of-month-operations
       {:handler  (fn [t {conn :conn}]
                    (when (last-day-of-month? t)
                      (d/transact-async conn [(event/job :ops/end-of-month {:params {:t t}})])))
        :params   params
        :schedule "0 0 0 * 28-31 * *"}


       :daily-operations
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(event/job :ops/daily {:params {:t t}})]))
        :params   params
        ;; 12am every day
        :schedule "0 0 0 * * * *"}


       :teller-operations
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(event/job :teller/daily {:params {:t t}})]))
        :params   params
        ;; 12am every day
        :schedule "0 0 0 * * * *"}

       :daily-events
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(events/process-daily-tasks t)
                                            (event/job :account/send-welcome-emails {:params {:t t}})]))
        :params   params
        ;; 9am every day
        :schedule "0 0 9 * * * *"}}

      :hubspot-application-sync
      (when (config/production? config)
        {:handler  (fn [t {conn :conn}]
                     (hubspot-sync/sync-applications conn))
         :params   params
         :schedule "0 0 8-19 * * * *"}))
     {}
     {:clock {:timezone "PST"}})))


(defstate scheduler
  :start (do
           (timbre/info ::starting)
           (sch/start! (create-scheduler conn)))
  :stop (do
          (timbre/info ::stopping)
          (sch/stop! scheduler)))


(comment


  (let [t1 #inst "2017-10-03"
        t2 #inst "2017-10-04"]
    (sch/simulate (sch/scheduler
                   {:print-task {:handler  (fn [t] (println "TIME:" t))
                                 :schedule "0 0 9 * * * *"}})
                  {:start t1 :end t2}))


  (let [t1 #inst "2018-08-05"
        t2 #inst "2018-08-06"]
    (sch/simulate (sch/scheduler
                   {:print-task {:handler  (fn [t] (println "TIME:" t))
                                 :schedule "0 0 9 * * * *"}
                    :do-daily-ops {:handler (fn [t {conn :conn}]
                                              (d/transact-async conn [(event/job :ops/daily {:params {:t t}})]))
                                   :schedule "0 0 0 * * * *"}})
                  {:start t1 :end t2}))


  )
