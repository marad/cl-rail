;;;; cl-rail.lisp

(in-package #:rail)

(defclass branch ()
  ((name :accessor branch-name
         :initform nil
         :initarg :name)
   (messages :accessor branch-messages
             :initform nil
             :initarg :messages)))

(defclass success (branch)
  ((name :initform :success)
   (value :accessor branch-value
          :initform nil
          :initarg :value)))

(defclass failure (branch)
  ((name :initform :failure)))

(defmethod print-object ((s success) out)
  (format out "Success {~a ~a}" (branch-value s) (branch-messages s)))

(defmethod print-object ((f failure) out)
  (format out "Failure {~a}" (branch-messages f)))

(defun succeed (value &optional messages)
  "Create a success branch. Optionally with messages."
  (make-instance 'success :value value :messages messages))

(defun fail (messages)
  "Create fail branch."
  (make-instance 'failure :messages messages))

(defgeneric branch-copy (branch &key messages)
  (:documentation "Copies given branch and gives the ability to update it's contents"))

(defmethod branch-copy ((branch success) &key (value nil value-supplied-p) (messages nil messages-supplied-p))
  (succeed (if value-supplied-p value (branch-value branch))
           (if messages-supplied-p messages (branch-messages branch))))

(defmethod branch-copy ((branch failure) &key (messages nil messages-supplied-p))
  (fail (if messages-supplied-p messages (branch-messages branch))))

(defgeneric either (f-success f-failure branch)
  (:documentation "Applies f-success if on success branch of f-failure if on failure branch.
   f-success takes value and a list of messages and should return a branch.
   f-failure takes list of messages and returns a branch."))

(defmethod either ((f-success function) (f-failure function) (branch success))
  (funcall f-success (branch-value branch) (branch-messages branch)))

(defmethod either ((f-success function) (f-failure function) (branch failure))
  (funcall f-failure (branch-messages branch)))

(defun merge-messages (msgs branch)
  "Merges additional messages to the branch"
  (branch-copy branch
               :messages (concatenate 'list msgs (branch-messages branch))))

(defun bind (f branch)
  "Given function f (value -> branch) applies it on success branch"
  (either (lambda (v ms)
            (merge-messages ms
                            (funcall f v)))
          #'fail
          branch))

(defun flat-map (f branch) "Alias for bind" (bind f branch))

(defgeneric fapply (f-branch v-branch)
  (:documentation "Applies function stored as a value in f-branch to value stored in v-branch."))

(defmethod fapply ((f-branch success) (v-branch success))
  (succeed (funcall (branch-value f-branch) (branch-value v-branch))))

(defmethod fapply ((f-branch branch) (v-branch branch))
  (fail (concatenate 'list (branch-messages f-branch) (branch-messages v-branch))))

(defun lift (f branch)
  "Given a function f applies it to value stored in branch"
  (fapply (succeed f) branch))

(defun fmap (f branch)
  "Alias for lift."
  (lift f branch))

(defgeneric success-side-effect (f branch)
  (:documentation "Given a function f : (value, messages) -> any and a branch
   applies the function to the success branch and returns result unchanged"))

(defmethod success-side-effect ((f function) (b success))
  (funcall f (branch-value b) (branch-messages b))
  b)

(defmethod success-side-effect ((f function) (b branch)) b)

(defun success-tee (f b)
  "Alias for success-side-effect"
  (success-side-effect f b))


(defgeneric failure-side-effect (f branch)
  (:documentation "Given a function f : (messages) -> any and a branch
   applies the function to the success branch and returns result unchanged"))

(defmethod failure-side-effect ((f function) (b failure))
  (funcall f (branch-messages b))
  b)

(defmethod failure-side-effect ((f function) (b branch)) b)

(defun failure-tee (f b)
  "Alias for failure-side-effect"
  (failure-side-effect f b))

(defun map-success (f branch)
  "Given a function (f : (value messages) -> branch) transforms success branch
   and returns a resulting branch"
  (either f #'fail branch))

(defun map-messages (f branch)
  "Given a function (f : (messages) -> messages) applies function to branch messages"
  (branch-copy branch :messages (funcall f (branch-messages branch))))


(defgeneric get-or-default (default branch)
  (:documentation "Returns the value from success branch or default for failure branch."))
(defmethod get-or-default (default (b success)) (branch-value b))
(defmethod get-or-default (default (b failure)) default)

(defgeneric get-or-default-f (default-f branch)
  (:documentation "Returns the value from success branch or computed value from default-f function for failure"))
(defmethod get-or-default-f ((default-f function) (b success)) (branch-value b))
(defmethod get-or-default-f ((default-f function) (b failure)) (funcall default-f))

(defun fail-if-nil (fail-message value)
  "Creates branch from a value. If value is nil then fails with given message"
  (if (null value)
      (fail (list fail-message))
      (succeed value)))

(defun success? (branch)
  (eq (type-of branch) 'success))

(defun fail? (branch)
  (eq (type-of branch) 'failure))
