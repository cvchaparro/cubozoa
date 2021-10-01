(in-package alarm)

;; As with everything else, each deployment configuration could be in a separate
;; file but I didn't do that to reduce the number of files we have to deal with
;; in this example.

;; Use `defconfig' if you want to name the configuration (to refer to it elsewhere).
(defconfig digital-clock
  (:use-model (clock :version "1.0.0"))
  ...)

;; If needed, we can refine specific characteristics about models (i.e. data)
;; in deployment configurations.
(with-config (alarm-controller :version "1.0.0")
  ;; Note we're refining the type of status to use, here,
  (refine-data status
               :type noisy-alarm-status
               :validator #'notification-file-exists?)

  (refine-data alarm-time
               :type zoned-time)

  (refine-data acknowledgement
               :type alarm-acknowledgement))

(defconfig digital-alarm-clock
  ;; The model for which we're providing a configuration
  (:use-model
   (alarm-clock :version "1.0.0"))

  ;; Sub-configurations to include in this configuration
  (:use-configs
   (digital-clock :version "1.0.0")
   (alarm-controller :version "1.0.0"))

  ;; Potentially, we could specify that `data' uses protobuf here so that it
  ;; doesn't have to be specified in each component implementation. This could
  ;; potentially overwrite lower-level implementation claims.
  (configure-data
   :implementation :proto3
   :directory "data"
   :extension "proto")

  ;; Some of the options to `configure-behavior' allow refining where to search
  ;; for/generate feature files.
  ;;
  ;; The `:implementation' option to `configure-behavior' defines whether we
  ;; hook into Cucumber (as in this case) for user-acceptance testing.
  ;; Alternatively, someone else might want to use Robot Framework for their
  ;; user-acceptance testing, in which case they'd switch the implementation
  ;; here to `:robot'.
  ;;
  ;; The `:directory' option determines in which directory to search
  ;; for/generate files. The `:extension' option determines which file extension
  ;; to use for feature files.
  ;;
  ;; It may also be useful to allow refining the filename, but the default could
  ;; be formatted as `model-name.behavior-name' - for example, one behavior
  ;; could be represented as `alarm-controller.set-alarm-time'.
  (configure-behavior
   :implementation :cucumber
   :directory "features"
   :extension "feature")

  (configure-links :implementation :grpc))
