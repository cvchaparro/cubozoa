(in-package #:cubozoa)

(defun standard-keyword (x)
  "Return X as a standard keyword."
  (funcall (compose #'make-keyword #'string-upcase) x))

(defun standard-symbol (x)
  "Return X as a standard symbol."
  (intern (string-upcase x)))

(defun project-pathspec (pathspec)
  "Return the full path of PATHSPEC relative to the current project."
  (asdf:system-relative-pathname :cubozoa pathspec))
