(in-package alarm)

;; An alternate option (especially for separating behavior by model) could be to
;; have a behavior directory which would contain individual files defining the
;; behavior for each model.

(defbehavior clock
  (publish-current-time
    (when waiting 1 second)
    (then (will publish :current-time)
          (will be completed within 5 milliseconds))))


(defbehavior alarm
  (trigger-alarm
    (given have received :alarm-time)
    (when receiving :current-time)
    (then will publish :status)))

(defbehavior alarm
  (deactivate-alarm
    (when receiving :acknowledgement)
    (then will publish :status)))


(defbehavior alarm-controller
  "The alarm time can be set as desired by the user."
  (set-alarm-time
   "Set the alarm time for a time in the future"
   (given (the alarm is not set)
          (the :time-zone is cst))
   (when the user sets the :alarm-time to <time>)
   (then (will publish :alarm-time)
         (the hours field of the :alarm-time will be <hours>)
         (the minutes field of the :alarm-time will be <minutes>)
         (the seconds field of the :alarm-time will be <seconds>)
         (the :time-zone field of the alarm-time will be cst))
   (examples
    (:time "7am" "7 am" "7:00" "0715" "12:00" "1200" "1300" "1pm" "1 pm" "14:15:16" "2:15:16")
    (:hours 7 7 7 7 12 12 13 13 13 14 14)
    (:minutes 0 0 0 15 0 0 0 0 0 15 15)
    (:seconds 0 0 0 0 0 0 0 0 0 16 16))))


(defbehavior alarm-controller
  (notify-user-of-alarm
    (when receiving :alarm-status)
    (then will interact with user)))

(defbehavior alarm-controller
  (acknowledge-alarm
    (when interacting with user)
    (then will publish :acknowledgement)))
