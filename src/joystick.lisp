(in-package #:nova)
(cl-annot:enable-annot-syntax)

(defvar *open-joysticks* nil)

(defmacro with-joysticks (&body body)
  `(let ((*open-joysticks* nil))
     (unwind-protect
	  (progn
	    (sdl2::sdl-joystick-event-state sdl2-ffi:+sdl-enable+)
	    ,@body)
       (dolist (joystick *open-joysticks*)
	 (sdl2:joystick-close joystick)))))

@export
(defun open-joystick (index)
  (let ((joystick (sdl2:joystick-open index)))
    (push joystick *open-joysticks*)
    joystick))

@export
(defun open-all-joysticks ()
  (loop for i from 0 to (1- (get-joystick-count))
     collect (open-joystick i)))

@export
(defun get-open-joysticks ()
  *open-joysticks*)

@export
(defun joystick-name (joystick)
  (sdl2:joystick-name joystick))

@export
(defun get-joystick-count ()
  (sdl2:joystick-count))
