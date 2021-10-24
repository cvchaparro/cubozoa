(defsystem #:cubozoa
  :description "The Lispy verison of Jellyfish/AaC."
  :author "Cameron V Chaparro <cameron@cameronchaparro.com>"
  :version "0.0.1"
  :depends-on (#:alexandria #:cl-yaml)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "types")
                 (:file "parse")
                 (:file "load")))))
