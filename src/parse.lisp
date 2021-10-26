(in-package #:cubozoa)

(defvar *aac-spec* nil
  "The complete specification for the Architecture-as-Code language.")

(defmacro with-spec-from-file ((filespec &rest args) &body body)
  "Execute `BODY' with the specification from `FILESPEC' bound to `*AAC-SPEC*'."
  `(let ((*aac-spec* (parse ,filespec t ,@args)))
     ,@body))

(defmethod parse :around (filespec type &rest args)
  (let ((type (standard-keyword (pathname-type filespec))))
    (apply #'call-next-method filespec type args)))

(defmethod parse (filespec type &rest args)
  (apply #'parse filespec type args))

;; TODO: This ultimately doesn't belong here, but for testing it's okay
(defmethod parse (filespec (type (eql :yaml)) &rest args)
  (let* ((model (apply #'cl-yaml:parse filespec args))
         (models (if (listp model) (rest model) (list model))))
    (flatten (union models (maybe-import-models filespec models)))))

(defun maybe-import-models (filespec models)
  "If `MODELS' has any imports, parse and load them."
  (labels ((parse* (file)
             (let ((pathspec (make-pathname
                              :name (pathname-name file)
                              :type (pathname-type file)
                              :directory (uiop:merge-pathname-directory-components
                                          (remove-if (lambda (part) (string= part "."))
                                                     (pathname-directory file))
                                          (pathname-directory filespec)))))
               (parse pathspec t :multi-document-p t))))
    (ctypecase models
      (list       (mapcar (lambda (m) (maybe-import-models filespec m)) models))
      (hash-table (let ((imports (gethash "import" models)))
                    (remhash "import" models)
                    (if imports (mapcar #'parse* imports) models))))))
