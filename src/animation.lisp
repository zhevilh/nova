(in-package :nova)
(cl-annot:enable-annot-syntax)

(defclass animation ()
  ((start-state :initarg :state-state :reader animation-start-state)
   (operations :initarg :operations :reader animation-operations)))

(defclass animation-operation ()
  ((duration :initarg :duration :reader operation-duration)
   (function :initarg :function :reader operation-function)))

(defgeneric execute-operation (operation state time-elapsed))

(defclass move-operation (animation-operation)
  ((destination :initarg :destination :reader destination)))

@export
(defun move-to (x y duration &optional (function #'identity))
  (make-instance 'move-operation
                 :destination (point x y)
                 :duration duration
                 :function function))

(defun operation-progress (operation time-elapsed)
  (clamp (/ time-elapsed (operation-duration operation)) 0 1))

(defun animate (start-state &rest operations)
  (loop with state = start-state
        for operation in operations
        do (setf state (funcall operation state))
        finally (return state)))
