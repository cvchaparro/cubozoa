(defsystem #:cubozoa-standard
  :description "Cubozoa system for working with standard-defined models."
  :author "Cameron V Chaparro <cameron@cameronchaparro.com>"
  :version "0.0.1"
  :depends-on (#:alexandria #:cubozoa)
  :serial t
  :components ((:module "src"
                :pathname "src/impl/"
                :components
                ((:file "standard")))))
