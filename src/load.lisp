(in-package #:cubozoa)

(defvar *spec-filename* (asdf:system-relative-pathname
                         :cubozoa "models/yaml/aac/aac.yaml")
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

(defmethod %name ((str string)) str)

(defmethod %name     ((table hash-table)) (gethash "name" table))
(defmethod %items    ((table hash-table)) (gethash "items" table))
(defmethod %required ((table hash-table)) (gethash "required" table))
(defmethod %type     ((table hash-table)) (gethash "type" table))
