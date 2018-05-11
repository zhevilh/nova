(in-package #:nova)
(cl-annot:enable-annot-syntax)

@export
(defun get-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

@export-class
(define-class timing ()
  (start-time (get-time))
  duration
  loop-type
  output-function)

@export
(defun timing (duration loop-type &optional output-function)
  (make-instance 'timing
		 :duration duration
		 :loop-type loop-type
		 :output-function output-function))

@export
(defun timing-elements (elements duration loop-type)
  (let* ((n (length elements))
	 (array (make-array n :initial-contents elements)))
    (timing duration loop-type
	    (lambda (r) (aref array (min (1- n) (floor (* n r))))))))

@export
(defun run-timing (timing)
  (with-slots (start-time duration loop-type output-function) timing
    (let ((time (- (get-time) start-time)))
      (-> (ecase loop-type
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
		  duration))))
	  (if output-function
	      (funcall output-function %)
	      %)))))

@export
(defun reset-timing (timing)
  (with-new (start-time duration) timing
    (setf start-time (get-time))))

(define-class sequence-action ()
  action
  priority
  duration)

(define-class sequencer ()
  (action-priority -1)
  (action-end-time 0)
  queue)

@export
(defun sequencer () (make-instance 'sequencer))

@export
(defun sequence-action (sequencer duration action &key (priority 0))
  (with-new (queue) sequencer
	    (let ((sa (make-instance 'sequence-action
			     :duration duration
        :priority priority
        :action action)))
      (setf queue
	    (if (eq :override priority)
	 (list sa)
  (-> queue
      (remove-if (lambda (p) (< p priority)) % :key #'sequence-action-priority)
      (append % (list sa))))))))

@export
(defmacro sequence-action! (sequencer duration action &key (priority 0))
  `(setf ,sequencer (sequence-action ,sequencer ,duration ,action :key ,priority)))

@export
(defmacro do-sequence-action (sequencer duration action-form &key (priority 0))
  `(sequence-action ,sequencer ,duration (lambda () ,action-form)
		    :priority ,priority))

@export
(defmacro do-sequence-action! (sequencer duration action-form &key (priority 0))
  `(setf ,sequencer
	 (do-sequence-action ,sequencer ,duration ,action-form :priority ,priority)))

@export
(defun tick-sequencer (sequencer)
  (with-slots (action-end-time action-priority queue) sequencer
    (if (and queue
	     (with-slots (priority) (car queue)
        (or (eq :override priority)
	    (> priority action-priority)
	    (>= (get-time) action-end-time))))
	(tick-sequencer
  (with-new (action-end-time action-priority queue) sequencer
	    (let ((sa (pop queue)))
      (with-slots (duration priority action) sa
        (funcall action)
        (setf action-end-time (+ (get-time) duration)
	      action-priority priority)))))
	sequencer)))

@export
(defmacro tick-sequencer! (sequencer)
  `(setf ,sequencer (tick-sequencer ,sequencer)))

@export
(defun sequencer-idle? (sequencer)
  (and (not (sequencer-queue sequencer))
       (>= (get-time) (sequencer-action-end-time sequencer))))


;; Testing
(defmacro run-sequencer-test (&rest sequence-actions-definitions)
  (with-gensyms (sequencer)
    `(let ((,sequencer (sequencer)) %)
       ,.(mapcar
	  (lambda (d)
     (destructuring-bind (priority duration action) d
       `(setf ,sequencer (do-sequence-action ,sequencer ,duration ,action :priority ,priority))))
   sequence-actions-definitions)
       (tick-sequencer ,sequencer)
       %)))

(lisp-unit:define-test sequencer-basic-tests
  (lisp-unit:assert-eq 1 (run-sequencer-test (0 0 (setf % 1))))
  (lisp-unit:assert-eq 1 (run-sequencer-test (0 1 (setf % 1))
          (0 0 (setf % 2))))
  (lisp-unit:assert-eq 2 (run-sequencer-test (0 0 (setf % 1))
					     (0 0 (setf % 2))))
  (let (x)
    (lisp-unit:assert-eq 3 (run-sequencer-test (0 0 (setf % 1))
					       (0 0 (setf x t % 2))
					       (0 0 (setf % 3))))
    (lisp-unit:assert-true x)))

(lisp-unit:define-test sequencer-priority-tests
    (lisp-unit:assert-eq 1 (run-sequencer-test (1 1 (setf % 1))
					       (0 0 (setf % 2))))
  (lisp-unit:assert-eq 2 (run-sequencer-test (0 1 (setf % 1))
					     (1 0 (setf % 2))))
  (lisp-unit:assert-eq 3 (run-sequencer-test (2 1 (setf % 1))
					     (2 2 (setf % 2))
					     (:override 0 (setf % 3))))
  (let (x)
    (lisp-unit:assert-eq 3 (run-sequencer-test (3 0 (setf % 1))
					       (1 1 (progn (setf % 2)
							   (setf x t)))
					       (2 0 (setf % 3))))
    (lisp-unit:assert-false x)))
