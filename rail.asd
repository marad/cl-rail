;;;; rail.asd

(asdf:defsystem #:rail
  :name "rail"
  :version "0.0.1"
  :description "Library implementing functions for railway oriented programming."
  :author "Marcin Radoszewski"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "rail")))

(asdf:defsystem #:rail-test
  :depends-on (:rail :fiasco)
  :serial t
  :components ((:file "test")))
