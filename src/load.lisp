(in-package #:cubozoa)

(defvar *spec-filename* (asdf:system-relative-pathname :cubozoa "models/yaml/aac/aac.yaml")
  "The file containing the Architecture-as-Code language specification.")

;; TODO: Make this more generic
;; For example, the assumption that everything will be loaded into a hash-table
;; is not a good assumption since other parsers might return it using a
;; different data structure.
(defun load-aac-spec ()
  (with-spec-from-file (*spec-filename* :multi-document-p t)
    (flatten
     (loop for table in *aac-spec*
           collect
           (let ((keys (hash-table-keys table))
                 (vals (hash-table-values table)))
             (setf keys (mapcar (compose #'make-keyword #'string-upcase) keys))
             (mapcar (lambda (key val)
                       (format t "~a: ~a~%" key (%name val))
                       (case key
                         (:data (make-data val))
                         (:enum (make-enum val))))
                     keys vals))))))

(defun make-data (&rest args)
  (let ((table (first args)))
    (make-instance 'data :name (%name table)
                         :items (mapcar #'make-data-item (%items table))
                         :required (%required table))))

(defun make-enum (&rest args)
  (let ((table (first args)))
    (make-instance 'enum :name (%name table)
                         :items (mapcar #'make-enum-item (%items table)))))

(defun make-data-item (&rest args)
  (let ((table (first args)))
    (make-instance 'data-item :name (%name table) :type (%type table))))

(defun make-enum-item (&rest args)
  (make-instance 'enum-item :name (first args)))

(defmethod %name     ((table hash-table)) (gethash "name" table))
(defmethod %items    ((table hash-table)) (gethash "items" table))
(defmethod %required ((table hash-table)) (gethash "required" table))
(defmethod %type     ((table hash-table)) (gethash "type" table))

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
