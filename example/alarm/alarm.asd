(defsystem #:cubozoa-example-alarm
  :description "An example system modeled with cubozoa."
  :version "0.0.1"
  :defsystem-depends-on (#:cubozoa)
  :pathname "example/"
  :components
  ((:module "alarm"
    :components ((:file "versions")
                 (:file "package")
                 (:file "data")
                 (:file "models")
                 (:file "behavior")
                 (:module "impl"
                  :components ((:file "configuration")))
                 (:module "behavior"
                  :components ((:static-file "alarm-controller.set-alarm-time.feature")))
                 (:module "data"
                  :components ((:static-file "alarm-status.proto")))))))
