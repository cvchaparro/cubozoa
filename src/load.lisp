(in-package #:cubozoa)

(defmacro with-spec-filespec (filespec &body body)
  `(let ((*spec-filespec* ,filespec))
     ,@body))

(defmacro with-spec-from-file ((filespec &rest args) &body body)
  "Execute `BODY' with the specification from `FILESPEC' bound to `*AAC-SPEC*'."
  `(let ((*aac-spec* (parse ,filespec t ,@args)))
     ,@body))

;; TODO: Make this more generic
;; For example, the assumption that everything will be loaded into a hash-table
;; is not a good assumption since other parsers might return it using a
;; different data structure.
;; TODO: Convert this to a generic function and specialize it for each type of
;; plugin
(defun load-aac-spec (&rest args)
  "Load the Architecture-as-Code specification."
  ;; KLUDGE: Find a better way to pass the args list to `with-spec-from-file'
  ;; without having to specifically `eval'
  (eval
   `(with-spec-from-file (,*spec-filespec* ,@args)
      (loop for model in *aac-spec*
            append (let ((keys (hash-table-keys model))
                         (vals (hash-table-values model)))
                     (setf keys (mapcar #'standard-keyword keys))
                     (flatten (mapcar (compose #'generate-class #'build) keys vals)))))))

(defun build (type value)
  "Build an object of the specified TYPE from the provided VALUE."
  (let ((class (intern (string type) 'cubozoa))
        (item-class (intern (format nil "~a-ITEM" type) 'cubozoa))
        (data-p (eq type :data)))
    (labels ((build-item (x)
               (apply #'make-instance item-class :name (%name x)
                      (if data-p (list :type (%type x))))))
      (apply #'make-instance class
             :name (%name value) :items (mapcar #'build-item (%items value))
             (if data-p (list :required (%required value)))))))

(defun generate-class (value)
  "Generate a class definition from the parsed value."
  (mapcar (compose #'eval #'generate)
          ;; KLUDGE: Come to a conclusion on whether `VALUE' should always be a
          ;; hash-table, or if we want to allow various types of objects.
          (if (hash-table-p value) (hash-table-values value) (list value))))

(defun generate (value)
  "Generate a new class that represents a type of model."
  (labels ((required-p (item)
             (and (subtypep (class-of value) 'data)
                  (not (null (member (%name item) (%required value) :test #'string=))))))
    (let ((name (standard-symbol (format nil "<~a>" (%name value))))
          (items (%items value))
          (documentation-fmt "The ~(~a~) of the ~(~a~)."))
      `(defclass ,name ()
         ,(loop for item in items
                collect
                (generate-slot-def name item
                                   :required-p (required-p item)
                                   :documentation-fmt documentation-fmt))
         (:documentation
          ,(format nil "The representation of a ~a model." name))))))

(defun generate-slot-def (model item &key (required-p t) documentation-fmt)
  "Generate a slot definition for `MODEL' from `NAME'.

The `REQUIRED' keyword argument is used to set the `:INITFORM'. If `REQUIRED' is
`T' and the user creates a class with this slot and it is unbound an error
condition will be raised. If `REQUIRED' is any value other than `T', then it is
used as the value of `:INITFORM'.

The `DOCUMENTATION-FMT' keyword argument is a format string that is used as
documentation for the slot. The format string provided by `DOCUMENTATION-FMT'
should accept the slot's name and the model's name (in that order) when writing
the documentation the string."
  (let* ((name  (%name item))
         (%name (standard-symbol (format nil "%~a" name))))
   `(,%name
     :accessor ,%name
     :initarg  ,(standard-keyword name)
     :initform
     ,(if (eq required-p t)
          `(error ,(format nil "~a is required for ~a models!" %name model))
          required-p)
     ,@(when documentation-fmt
         `(:documentation ,(format nil documentation-fmt name model))))))
