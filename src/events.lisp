(in-package #:nova)
(cl-annot:enable-annot-syntax)

@export
(defgeneric handle-event (scene event-type event))
(defmethod handle-event (scene event-type event)
  (declare (ignore scene event-type event)))

(defmacro define-event (method-qualifier event-type scene-method-arg properties
                        &body body)
  (with-gensyms (event)
    `(defmethod handle-event
         ,.(when method-qualifier
             (list method-qualifier))
         (,scene-method-arg
          (event-type (eql ,event-type))
          ,event)
       (let* (,@(sdl2::unpack-event-params event event-type properties))
	 ,@body))))

(defmacro defmacro-key-event (macro-name event-type method-qualifier)
  `(defmacro ,macro-name ((scene-method-arg scancode-arg
                           &key mod) &body body)
     (with-gensyms (keysym)
       `(define-event ,,method-qualifier ,,event-type ,scene-method-arg
            ((:keysym ,keysym))
	  (let ((,scancode-arg (sdl2:scancode ,keysym))
                ,@(when mod `((,mod (sdl2:mod-keywords
                                     (sdl2:mod-value ,keysym))))))
	    ,@body)))))

(defmacro defmacro-event (macro-name event-type method-qualifier)
  `(defmacro ,macro-name ((scene-method-arg &rest properties) &body body)
     `(define-event ,,method-qualifier ,,event-type ,scene-method-arg ,properties
	,@body)))

(defmacro define-with-qualifiers (&body macro-calls)
  `(progn
     ,.(loop for (defmacro-call base-macro-name event-type) in macro-calls
             append (loop for qualifier in '(nil :after :before :around)
                          for macro-name = (if qualifier
                                               (intern
                                                (format nil "~a-~a"
                                                        base-macro-name qualifier))
                                               base-macro-name)
                          append `((export ',macro-name)
                                   (,defmacro-call ,macro-name ,event-type
                                     ,qualifier))))))

(define-with-qualifiers
  (defmacro-key-event define-keydown :keydown)
  (defmacro-key-event define-keyup :keyup)
  (defmacro-event define-mousemotion :mousemotion)
  (defmacro-event define-mousebuttondown :mousebuttondown)
  (defmacro-event define-mousebuttonup :mousebuttonup)
  (defmacro-event define-mousewheel :mousewheel)
  (defmacro-event define-joybuttondown :joybuttondown)
  (defmacro-event define-joybuttonup :joybuttonup))

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
