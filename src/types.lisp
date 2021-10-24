(in-package #:cubozoa)

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
