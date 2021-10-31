(defpackage #:cubozoa-yaml
  (:use #:cl #:cubozoa #:alexandria))

(in-package #:cubozoa-yaml)

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

(with-spec-filespec (project-pathspec "models/yaml/aac/aac.yaml")
  (load-aac-spec :multi-document-p t))
