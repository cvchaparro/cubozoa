(in-package alarm)

;; An alternate option (especially for larger systems) could be to have a model
;; directory which would contain individual files defining each model of the
;; system.

(defmodel clock
  (with-inputs
    (time-to-set :type zoned-time))

  (with-outputs
    (current-time :type zoned-time)))

(defmodel alarm
  (with-inputs
    (current-time :type zoned-time)
    (alarm-time :type zoned-time)
    (acknowledgement :type alarm-acknowledgement))

  (with-outputs
    (status :type alarm-status)))

(defmodel alarm-controller
  (with-inputs
    (status :type alarm-status))

  (with-outputs
    (alarm-time :type zoned-time)
    (acknowledgement :type alarm-acknowledgement)))

(defmodel alarm-clock
  (with-components
    (clock :type clock)
    (alarm :type alarm)
    (controller :type alarm-controller))

  (with-links
    ;; Approach 1
    (current-time (clock -> alarm))
    (alarm-time (controller -> alarm))
    (acknowledgement (controller -> alarm))
    (status (alarm -> controller))

    ;; Approach 2
    ((current-time clock) -> (current-time alarm))
    ((alarm-time controller) -> (alarm-time alarm))
    ((acknowledgement controller) -> (acknowledgement alarm))
    ((status alarm) -> (status controller))

    ;; Approach 3
    (clock.current-time -> alarm.current-time)
    (controller.alarm-time -> alarm.alarm-time)
    (controller.acknowledgement -> alarm.acknowledgement)
    (alarm.status -> controller.status)))
