(in-package alarm)

(defdata alarm-acknowledgement
  ((id :type integer)
   (acknowledged-p :type boolean))
  (:documentation "Some descriptive string for the alark-acknowledgement data type."))

(defdata alarm-status
  ((id :type integer)
   (active :type boolean)))

;; Note that we're extending an alarm-status type. This is akin to inheritence
;; but strictly for data types.
(defdata (noisy-alarm-status :extends alarm-status)
  (sound :type file-stream))
