(in-package #:cubozoa)

(defvar *spec-filename* (asdf:system-relative-pathname :cubozoa "models/yaml/aac/aac.yaml")
  "The file containing the Architecture-as-Code language specification.")

(defun build (type value)
  "Build an object of the specified TYPE from the provided VALUE."
  (let ((class (intern (string type)))
        (item-class (intern (format nil "~a-ITEM" type)))
        (data-p (eq type :data)))
    (labels ((build-item (x)
               (apply #'make-instance item-class :name (%name x)
                      (if data-p (list :type (%type x))))))
      (apply #'make-instance class
             :name (%name value) :items (mapcar #'build-item (%items value))
             (if data-p (list :required (%required value)))))))

;; TODO: Make this more generic
;; For example, the assumption that everything will be loaded into a hash-table
;; is not a good assumption since other parsers might return it using a
;; different data structure.
(defun load-aac-spec ()
  "Load the Architecture-as-Code specification."
  (with-spec-from-file (*spec-filename* :multi-document-p t)
    (loop for table in *aac-spec*
          append (let ((keys (hash-table-keys table))
                       (vals (hash-table-values table)))
                   (setf keys (mapcar #'standard-keyword keys))
                   (mapcar #'build keys vals)))))

(defun standard-keyword (x)
  "Return X as a standard keyword."
  (funcall (compose #'make-keyword #'string-upcase) x))

(defmethod %name ((str string)) (print str) str)

(defmethod %name     ((table hash-table)) (print table) (gethash "name" table))
(defmethod %items    ((table hash-table)) (print table) (gethash "items" table))
(defmethod %required ((table hash-table)) (print table) (gethash "required" table))
(defmethod %type     ((table hash-table)) (print table) (gethash "type" table))

(defclass data ()
  ((%name
    :initform (error "%NAME is required for a data model!")
    :initarg :name
    :accessor %name
    :type     string
    :documentation "The name of the data model.")
   (%items
    :initform (error "%ITEMS is required for a data model!")
    :initarg :items
    :accessor %items
    :type     list
    :documentation "The list of items for the data model.")
   (%required
    :initform nil
    :initarg :required
    :accessor %required
    :documentation "The list of required items for the data model."))
  (:documentation "The representation of a data model."))

(defclass enum ()
  ((%name
    :initform (error "%NAME is required for a enum model!")
    :initarg :name
    :accessor %name
    :type     string
    :documentation "The name of the enum model.")
   (%items
    :initform (error "%ITEMS is required for a enum model!")
    :initarg :items
    :accessor %items
    :type     list
    :documentation "The list of items for the enum model."))
  (:documentation "The representation of an enumeration model."))

(defclass item () ()
  (:documentation "The representation of an item in a model's ITEMS list."))

(defclass data-item (item)
  ((%name
    :initform (error "%NAME is required for a data item!")
    :initarg :name
    :accessor %name
    :type     string
    :documentation "The name of the data item.")
   (%type
    :initform (error "%TYPE is required for a data item!")
    :initarg :type
    :accessor %type
    :type     string
    :documentation "The type of the data item."))
  (:documentation "The representation of a data item."))

(defclass enum-item (item)
  ((%name
    :initform (error "%NAME is required for a enum item!")
    :initarg :name
    :accessor %name
    :type     string
    :documentation "The name of the enum item."))
  (:documentation "The representation of a enum item."))
