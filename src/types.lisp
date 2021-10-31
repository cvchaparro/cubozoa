(in-package #:cubozoa)

;; TODO: See if we can get rid of the `DATA' and `ENUM' classes in favor of
;; generating them like we do with all the other classes. I'm not sure yet, but
;; we may still need the `*ITEM' classes.
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

(defmethod %name ((str string)) str)
(defmethod %name ((sym symbol)) sym)

(defmethod %name     ((table hash-table)) (gethash "name" table))
(defmethod %items    ((table hash-table)) (gethash "items" table))
(defmethod %required ((table hash-table)) (or (gethash "required" table)
                                              (gethash "required-p" table)))
(defmethod %type     ((table hash-table)) (gethash "type" table))

(defmethod %name     ((xs list)) (getf xs :name))
(defmethod %items    ((xs list)) (getf xs :items))
(defmethod %required ((xs list)) (or (getf xs :required)
                                     (getf xs :required-p)))
(defmethod %type     ((xs list)) (getf xs :type))
