(defsystem #:cubozoa
  :description "The Lispy verison of Jellyfish/AaC."
  :author "Cameron V Chaparro <cameron@cameronchaparro.com>"
  :version "0.0.1"
  :depends-on (#:alexandria #:iterate)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "api")
                 (:file "util")
                 (:file "types")
                 (:file "parse")
                 (:file "load")))))
