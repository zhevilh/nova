(in-package #:nova)
(cl-annot:enable-annot-syntax)

@export
(defgeneric handle-event (scene event-type event))
(defmethod handle-event (scene event-type event)
  (declare (ignore scene event-type event)))

(defmacro define-event (event-type scene-method-arg properties &body body)
  (with-gensyms (event)
    `(defmethod handle-event (,scene-method-arg
			      (event-type (eql ,event-type))
			      ,event)
       (let* (,@(sdl2::unpack-event-params event event-type properties))
	 ,@body))))

(defmacro defmacro-key-event (macro-name event-type)
  `(defmacro ,macro-name ((scene-method-arg scancode-arg) &body body)
     (with-gensyms (keysym)
       `(define-event ,,event-type ,scene-method-arg ((:keysym ,keysym))
	  (let ((,scancode-arg (sdl2:scancode ,keysym)))
	    ,@body)))))

(defmacro defmacro-event (macro-name event-type)
  `(defmacro ,macro-name ((scene-method-arg &rest properties) &body body)
     `(define-event ,,event-type ,scene-method-arg ,properties
	,@body)))

@export
(defmacro-key-event define-keydown :keydown)
@export
(defmacro-key-event define-keyup :keyup)

@export
(defmacro-event define-mousemotion :mousemotion)
@export
(defmacro-event define-mousebuttondown :mousebuttondown)
@export
(defmacro-event define-mousebuttonup :mousebuttonup)
@export
(defmacro-event define-mousewheel :mousewheel)

@export
(defmacro-event define-joybuttondown :joybuttondown)
@export
(defmacro-event define-joybuttonup :joybuttonup)

@export
(defmacro define-quit ((scene-method-arg) &body body)
  (with-gensyms (event)
    `(defmethod handle-event (,scene-method-arg
			      (event-type (eql :quit)) ,event)
       (declare (ignore ,event))
       ,@body)))

(define-quit (any-scene)
  (declare (ignore any-scene))
  (quit-game))
