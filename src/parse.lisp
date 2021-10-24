(in-package #:cubozoa)

(defvar *aac-spec* nil
  "The complete specification for the Architecture-as-Code language.")

(defmacro with-spec-from-file ((filespec &rest args) &body body)
  "Execute `BODY' with the specification from `FILESPEC' bound to `*AAC-SPEC*'."
  `(let ((*aac-spec* (parse ,filespec t ,@args)))
     ,@body))

(defgeneric parse (filespec type &rest args)
  (:documentation "Parse the Architecture-as-Code specification from `FILESPEC'."))

(defmethod parse :around (filespec type &rest args)
  (let ((type (make-keyword (string-upcase (pathname-type filespec)))))
    (apply #'call-next-method filespec type args)))

(defmethod parse (filespec type &rest args)
  (apply #'parse filespec type args))

;; TODO: This ultimately doesn't belong here, but for testing it's okay
(defmethod parse (filespec (type (eql :yaml)) &rest args)
  (let ((model (apply #'cl-yaml:parse filespec args)))
    (if (listp model) (rest model) model)))
