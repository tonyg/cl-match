;;(cl:in-package :cl)

(asdf:defsystem :pcl-unit-test
  :depends-on (:standard-cl)
  :components (
    (:file "pcl-unit-test")
))