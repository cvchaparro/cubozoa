(in-package #:cubozoa)

(defgeneric parse (filespec type &rest args)
  (:documentation "Parse the Architecture-as-Code specification from `FILESPEC'."))
