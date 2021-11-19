(in-package :nova)
(cl-annot:enable-annot-syntax)

@export
(defclass action-scene (base-scene)
  ((action-queue :initform nil :accessor action-queue)
   (flash :initform nil :accessor action-flash)))

(defclass flash ()
  ((timer :initform (timer) :reader flash-timer)
   (color :initarg :color :reader flash-color)
   (area :initarg :area :reader flash-area)
   (function :initarg :function :reader flash-function)
   (duration :initarg :duration :reader flash-duration)))

(defclass action ()
  ((key :initform nil :accessor action-key)
   (timer :initform (timer) :reader action-timer)
   (action :initarg :action :reader action)))

(defclass delay-action (action)
  ((delay :initarg :delay :reader delay)))

(defclass continuous-action (action)
  ((duration :initarg :duration :reader duration)))

(defgeneric handle-action (action)
  (:method ((a delay-action))
    (when (>= (timer-time (action-timer a)) (delay a))
      (funcall (action a))
      t))
  (:method ((a continuous-action))
    (funcall (action a) (/ (timer-time (action-timer a)) (duration a)))
    (>= (timer-time (action-timer a)) (duration a))))

(defun handle-flash (scene)
  (when (and (action-flash scene)
             (>= (timer-time (flash-timer (action-flash scene)))
                 (flash-duration (action-flash scene))))
    (setf (action-flash scene) nil)))

(defmethod idle-scene :before ((s action-scene))
  (dolist (a (action-queue s))
    (when (handle-action a)
      (remove! a (action-queue s)))))

(defparameter *action-key* nil)

(defun add-action (action-scene action)
  (when *action-key*
    (setf (action-key action) *action-key*)
    (remove! *action-key* (action-queue action-scene) :key #'action-key))
  (push action (action-queue action-scene)))

(defun make-delay-action (delay action-fn)
  (make-instance 'delay-action
                 :delay delay
                 :action action-fn))

(defun make-continuous-action (duration action-fn)
  (make-instance 'continuous-action
                 :duration duration
                 :action action-fn))

@export
(defun key-action (key)
  (setf *action-key* key))

@export
(defmacro delay-action (action-scene delay &body body)
  `(let ((*action-key* nil))
     (add-action ,action-scene (make-delay-action ,delay (lambda () ,@body)))))

@export
(defmacro continuous-action (action-scene duration (&key completion) &body body)
  (let ((completion-sym (or completion (gensym))))
    `(let ((*action-key* nil))
       (add-action ,action-scene
                   (make-continuous-action
                    ,duration
                    (lambda (,completion-sym)
                      ,.(when (not completion)
                          `((declare (ignore ,completion-sym))))
                      ,@body))))))

@export
(defun flash (action-scene duration
              &key area (color +white+) function intensity)
  (in-place (or function (lambda (x) (sin (* pi x)))))
  (when intensity
    (setf function (compose (curry #'* intensity) function)))
  (setf (action-flash action-scene)
        (make-instance 'flash
                       :color color
                       :area (or area (make-area (point 0 0) (window-size)))
                       :function function
                       :duration duration)))

(defun flash-alpha (flash)
  (clamp (round (* (funcall (flash-function flash)
                            (/ (timer-time (flash-timer flash))
                               (flash-duration flash)))
                   255))
         0 255))

(defmethod draw-scene :after ((s action-scene))
  (with-access ((f action-flash)) s
    (when f
      (clear-area (flash-area f)
                  :color (alpha (flash-alpha f) (flash-color f))
                  :blend-mode :blend)
      (handle-flash s))))
