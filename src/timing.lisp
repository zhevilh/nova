(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defparameter *timer* (timer))

@export
(defun reset-time ()
  (setf *timer* (timer)))

@export
(defun get-time ()
  (timer-time *timer*))

@export
(defun freeze-time ()
  (in-place (pause-timer *timer*)))

@export
(defun time-frozen? ()
  (timer-started? *timer*))

@export
(defun unfreeze-time ()
  (in-place (resume-timer *timer*)))

@export
(defun offset-time (offset)
  (in-place (push-timer *timer* offset)))

@export-class
(define-class timing ()
  timer
  duration
  loop-type
  output-function)

@export
(defun timing (duration loop-type &key (timer (timer)) output)
  (make-instance 'timing
                 :timer timer
		 :duration duration
		 :loop-type loop-type
		 :output-function output))

@export
(defun timing-elements (elements duration loop-type &key (timer (timer)))
  (let* ((n (length elements))
	 (array (make-array n :initial-contents elements)))
    (timing duration loop-type
            :timer timer
	    :output (lambda (r) (aref array (min (1- n) (floor (* n r))))))))

@export
(defun run-timing (timing)
  (with-slots (timer duration loop-type output-function) timing
    (let ((time (timer-time timer)))
      (-> (timing-function time duration loop-type)
	  (if output-function
	      (funcall output-function %)
	      %)))))

@export
(defun timing-function (time duration loop-type)
  (ecase loop-type
    (:no-loop
     (/ (min duration time) duration))
    (:loop
      (/ (mod time duration) duration))
    (:loop-back
     (let* ((total-duration (* 2 duration))
            (time (mod time total-duration)))
       (/ (if (> time duration)
              (- total-duration time)
              time)
          duration)))))

@export
(defun reset-timing (timing &optional (offset 0))
  (with-new (timer) timing
    (setf timer (timer))
    (in-place (push-timer timer offset))))

@export
(defun timing-progress (timing)
  (with-slots (timer duration loop-type) timing
    (-> (timer-time timer)
        (ecase loop-type
          (:no-loop %)
          ((:loop :loop-back) (mod % duration))))))

@export
(defun pause-timing (timing)
  (with-new (timer) timing
    (in-place (pause-timer timer))))

@export
(defun resume-timing (timing)
  (with-new (timer) timing
    (in-place (resume-timer timer))))

(define-class sequence-action ()
  action
  duration)

(define-class sequencer (:mutable? t)
  (action-end-time 0)
  sequence)

@export
(defun sequencer () (make-instance 'sequencer))

@export
(defmacro sequence-action (duration &body form)
  `(make-instance 'sequence-action
                  :duration ,duration
                  :action (lambda ()
                            ,@form)))

@export
(defun bind-sequence (&rest sequences)
  (-> (mapcar #'ensure-list sequences)
      (apply #'append %)))

@export
(defun sequencer-append! (sequencer &rest sequences)
  (with-slots (sequence) sequencer
    (setf sequence (apply #'bind-sequence (.-> sequencer sequence) sequences))))

@export
(defun sequencer-override! (sequencer &rest sequences)
  (with-slots (sequence action-end-time) sequencer
    (setf sequence (bind-sequence sequences)
          action-end-time 0)))

@export
(defun tick-sequencer! (sequencer)
  (with-slots (action-end-time sequence) sequencer
    (when (and sequence (>= (get-time) action-end-time))
      (let ((actions (loop for sa = (pop sequence)
                           collect sa
                           until (or (not sequence) (> (.-> sa duration) 0)))))
        (mapcar (compose #'funcall #'sequence-action-action) actions)
        (setf action-end-time
              (+ (get-time) (.-> (car (reverse actions)) duration)))))))

@export
(defun sequencer-idle? (sequencer)
  (and (not (.-> sequencer sequence))
       (let ((idle-since (- (get-time) (.-> sequencer action-end-time))))
         (when (> idle-since 0) idle-since))))

@export
(defun skip-sequence-action! (sequencer)
  (setf (.-> sequencer action-end-time) 0))
