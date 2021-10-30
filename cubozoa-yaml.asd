(defsystem #:cubozoa-yaml
  :description "Cubozoa system for working with YAML-defined models."
  :author "Cameron V Chaparro <cameron@cameronchaparro.com>"
  :version "0.0.1"
  :depends-on (#:alexandria #:cl-yaml #:cubozoa)
  :serial t
  :components ((:module "src"
                :pathname "src/impl/"
                :components
                ((:file "yaml")))))
