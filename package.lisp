;;;; package.lisp

(defpackage #:rail
  (:use #:cl)
  (:export :succeed
           :ok
           :fail
           :either
           :merge-messages
           :bind
           :flat-map
           :fapply
           :lift
           :fmap
           :success-side-effect
           :success-tee
           :failure-side-effect
           :failure-tee
           :map-success
           :map-messages
           :get-or-default
           :get-or-default-f
           :fail-if-nil
           :success?
           :fail?))

