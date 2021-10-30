(in-package #:cubozoa)

(defvar *aac-spec* nil
  "The complete specification for the Architecture-as-Code language.")

(defvar *spec-filespec* nil
  "The file containing the Architecture-as-Code language specification.")

(defgeneric parse (filespec type &rest args)
  (:documentation "Parse the Architecture-as-Code specification from `FILESPEC'."))
