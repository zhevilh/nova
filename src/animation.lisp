(in-package :nova)
(annot:enable-annot-syntax)

(defvar *animate-functions* (make-hash-table))

(defmacro def-animate-function (key duration-var current-time-var &body body)
  `(setf (gethash ,key *animate-functions*)
	 (lambda (,duration-var ,current-time-var) ,@body)))

@export
(def-animate-function :linear d ct
  (/ ct d))

(defun resolve-animate-function (function)
  (if (keywordp function)
      (gethash function *animate-functions*)
      function))

@export
(defun animate (sprites duration &key (loop-type :no-loop) (function :linear))
  "sprite list -> int -> (int -> sprite)"
  (let ((function (resolve-animate-function function))
	(start-time (sdl2:get-ticks))
	(n (length sprites)))
    (flet ((get-sprite (current-time)
	     (elt sprites
		  (floor (* n (funcall function duration current-time))))))
      (case loop-type
	(:no-loop
	 (let ((last-elt (last sprites)))
	   (lambda (current-time)
	     (if (> current-time duration)
		 last-elt
		 (get-sprite (- current-time start-time))))))
	(:loop
	 (lambda (current-time)
	   (get-sprite (mod (- current-time start-time) duration))))
	(:loop-back
	 (let ((total-duration (* 2 duration)))
	   (lambda (current-time)
	     (let ((current-time (mod (- current-time start-time) total-duration)))
	       (if (> current-time duration)
		   (get-sprite (- total-duration current-time))
		   (get-sprite current-time))))))))))

(defmacro animate-helper (duration function completion-ratio-sym &body body)
  (with-gensyms (start-time current-time)
    `(let ((,start-time (sdl2:get-ticks)))
       (lambda (,current-time)
	 (let* ((,current-time (- ,current-time ,start-time))
		(,completion-ratio-sym (if (< ,current-time ,duration)
					   (funcall (resolve-animate-function ,function) ,duration ,current-time)
					   1)))
	   ,@body)))))

@export
(defun animate-move (translation-x translation-y duration &key (base-point (point 0 0)) (function :linear))
  "Point p => int -> int -> int -&> p -&> animate-function -> (int -> p)"
  (animate-helper duration function completion-ratio
    (with-new (x y) base-point
      (incf x (floor (* translation-x completion-ratio)))
      (incf y (floor (* translation-y completion-ratio))))))

@export
(defun animate-size (base-size target-size duration &key (function :linear))
  "Size a => a -> size -> (int -> a)"
  (animate-helper duration function completion-ratio
    (with-new (w h) base-size
      (incf w (floor (* (w target-size) completion-ratio)))
      (incf h (floor (* (h target-size) completion-ratio))))))
