(defpackage #:cubozoa
  (:use #:cl #:alexandria #:iterate)
  (:export
   ;; api.lisp
   #:*aac-spec*
   #:*spec-filespec*
   #:parse

   ;; load.lisp
   #:with-spec-from-file
   #:with-spec-filespec
   #:load-aac-spec

   ;; util.lisp
   #:standard-keyword
   #:standard-symbol
   #:project-pathspec))
