(in-package #:cl-user)

(fiasco:define-test-package #:rail-test
  (:use #:rail))

(in-package #:rail-test)

(defgeneric branch= (a b)
  (:documentation "Checks wether two branches are the same"))

(defmethod branch= ((a success) (b success))
  (and (equalp (branch-value a) (branch-value b))
       (equalp (branch-messages a) (branch-messages b))))

(defmethod branch= ((a failure) (b failure))
  (equalp (branch-messages a) (branch-messages b)))

(defmethod branch= (a b) nil)

(deftest succeed-creates-valid-instance ()
  (let ((inst (succeed 42 '(:message))))
    (is (success? inst))
    (is (equal (rail:branch-value inst) 42))
    (is (equal (rail:branch-messages inst) '(:message)))))

(deftest fail-creates-valid-instance ()
  (let ((inst (fail '(:message))))
    (is (fail? inst))
    (is (equal (branch-messages inst) '(:message)))))

(deftest test-either ()
  (is (branch= (succeed 43 '(:msg))
               (either (lambda (v ms) (succeed (1+ v) ms))
                       #'fail
                       (succeed 42 '(:msg)))))
  (is (branch= (fail '(:foo :bar))
               (either #'succeed
                       (lambda (ms) (fail (cons :foo ms)))
                       (fail '(:bar))))))

(deftest test-merge ()
  (is (branch= (succeed 42 '(:foo :bar))
               (merge-messages '(:foo) (succeed 42 '(:bar)))))
  (is (branch= (fail '(:foo :bar))
               (merge-messages '(:foo) (fail '(:bar))))))


(deftest test-bind ()
  (is (branch= (succeed 43 '(:msg))
               (bind (lambda (v) (succeed (1+ v) '(:msg)))
                     (succeed 42))))
  (is (branch= (fail '(:foo))
               (bind (lambda (v) (succeed v))
                     (fail '(:foo))))))

(deftest test-fapply ()
  (let ((f (succeed #'1+ '(:foo))))
    (is (branch= (fapply f (succeed 42))
                 (succeed 43 '(:foo))))
    (is (branch= (fapply f (fail '(:bar)))
                 (fail '(:foo :bar))))))


(deftest test-lift ()
  (is (branch= (lift #'1+ (succeed 42 '(:foo)))
               (succeed 43 '(:foo))))
  (is (branch= (lift #'1+ (fail '(:foo)))
               (fail '(:foo)))))

(deftest test-map-success ()
  (is (branch= (map-success (lambda (v ms) (succeed (1+ v) ms))
                            (succeed 42 '(:foo)))
               (succeed 43 '(:foo))))
  (is (branch= (map-success (lambda (v ms) (succeed (1+ v) ms))
                            (fail '(:foo)))
               (fail '(:foo)))))

(deftest test-map-messages ()
  (is (branch= (map-messages (lambda (ms) (cons :foo ms))
                             (succeed 42 '(:bar)))
               (succeed 42 '(:foo :bar))))
  (is (branch= (map-messages (lambda (ms) (cons :foo ms))
                             (fail '(:bar)))
               (fail '(:foo :bar)))))


(deftest test-get-or-default ()
  (is (equal (get-or-default 24 (succeed 42))
             42))
  (is (equal (get-or-default 24 (fail '()))
             24)))


(deftest test-get-or-default-f ()
  (is (equal (get-or-default-f (lambda () 24) (succeed 42))
             42))
  (is (equal (get-or-default-f (lambda () 24) (fail '()))
             24)))

(deftest test-fail-if-nil ()
  (is (branch= (fail-if-nil :was-nil 42)
               (succeed 42)))
  (is (branch= (fail-if-nil :was-nil nil)
               (fail '(:was-nil)))))
