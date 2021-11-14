(defpackage #:cubozoa-standard
  (:use #:cl #:cubozoa #:alexandria)
  (:import-from #:cubozoa
                #:%name
                #:%required
                #:build
                #:data
                #:data-item
                #:enum
                #:enum-item))

(in-package #:cubozoa-standard)

(defmethod parse ((type (eql :lisp)) filespec &rest args)
  (apply #'load filespec args))

(defmacro defenum (name &rest keys)
  `(build :enum (list :name ',name :items ',keys)))

(defmacro defdata (name items)
  (with-gensyms (xs required name-if-required data-form->plist item)
    `(labels ((,data-form->plist (,item) (cons :name ,item))
              (,name-if-required (,item) (and (%required ,item) (%name ,item)))
              (,required (,xs)
                (remove-if #'null (mapcar #',name-if-required ,xs))))
       (let ((,xs (mapcar #',data-form->plist ',items)))
         (build :data (list :name ',name :items ,xs :required (,required ,xs)))))))
