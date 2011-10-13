;; asdf system def for standard-cl

(defsystem :standard-cl
  :components (
    (module "std"
      :serial t
      :components (
        (:file "package")
        (:file "base")
        (:module :content
           :components (
             (:file "standard")
             (:file "2fix")
             (:file "func")))
        (:file "syntax")
        ;;(:file "asdf")
))))