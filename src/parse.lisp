(in-package #:cubozoa)

(defmethod parse :around (type filespec &rest args)
  (let ((type (standard-keyword (pathname-type filespec))))
    (apply #'call-next-method type filespec args)))

(defmethod parse (type filespec &rest args)
  (apply #'parse type filespec args))
