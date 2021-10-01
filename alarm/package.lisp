;; Naturally, this would generaly be in another file/module but for
;; simplicity-sake I just included it here.
(defpackage time)

(in-package time)

(defenum time-zone
  :est :cst :mst :pst)

(defdata time
  ((hour :type integer)
   (minute :type integer)
   (second :type integer)))

(defdata (zoned-time :extends time)
  ((zone :type time-zone)))

(defpackage alarm
  (:import-from #:time
                #:time-zone
                #:time
                #:zoned-time))
