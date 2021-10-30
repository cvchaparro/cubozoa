(in-package #:cubozoa)

(defmethod parse :around (filespec type &rest args)
  (let ((type (standard-keyword (pathname-type filespec))))
    (apply #'call-next-method filespec type args)))

(defmethod parse (filespec type &rest args)
  (apply #'parse filespec type args))
