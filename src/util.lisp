(in-package #:cubozoa)

(defun standard-keyword (x)
  "Return X as a standard keyword."
  (funcall (compose #'make-keyword #'string-upcase) x))

(defun standard-symbol (x)
  "Return X as a standard symbol."
  (intern (string-upcase x)))
