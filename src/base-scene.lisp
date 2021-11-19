(in-package :nova)
(cl-annot:enable-annot-syntax)

(defvar *logical-resolution-stack* nil)

@export
(defclass base-scene ()
  ((logical-resolution :initform nil :accessor logical-resolution)))

(defmethod unload-scene :after ((s base-scene))
  (when (logical-resolution s)
    (pop *logical-resolution-stack*))
  (when-> (car *logical-resolution-stack*)
          (with-access (w h) %
            (sdl2::sdl-render-set-logical-size *renderer* w h))))

@export
(defun set-logical-resolution (scene width height)
  (push
   (setf (logical-resolution scene) (size width height))
   *logical-resolution-stack*)
  (sdl2::sdl-render-set-logical-size *renderer* width height))
