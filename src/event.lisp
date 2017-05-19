(in-package :nova)
(annot:enable-annot-syntax)

(defmacro unwrap-event (event event-type event-params &body body)
  "Unwraps the parameters of an event of type \"event-type\" into \"event-params\"."
  `(when (eq (car ,event) ,event-type)
     (let (,.(loop
		:for ep :in event-params
		:for n := 1 :then (1+ n)
		:collect `(,ep (nth ,n ,event))))
       ,@body)))

(defmacro handle-keydown (event &body key-handlers)
  "Handles the different keys of a \"keydown\" event."
  (with-gensyms (key)
    `(let (retval handled)
       (unwrap-event ,event :keydown (,key)
	 (setf retval
	       (cond ,.(mapcar (lambda (kh)
				 (let ((scancode (car kh)))
				   `(,(if (eq scancode t)
					  't
					  `(sdl2:scancode= ,key ,scancode))
				      (setf handled t)
				      ,@(cdr kh))))
			       key-handlers)))
	 (values retval handled)))))

@export
(defmacro defevents (scene-decl world-sym &body event-handlers)
  "Creates a scene-event method for \"scene-class\".
   Usage: (defevents my-scene world (:keydown (:scancode-espace nil)))"
  (with-gensyms (event)
    `(defmethod scene-event (,scene-decl ,event ,world-sym)
       (let ((,world-sym ,world-sym))
	 ,.(loop :for eh :in event-handlers
	      :collect `(multiple-value-bind (retval handled)
			    ,(case (car eh)
			       (:keydown `(handle-keydown ,event
					    ,@(cdr eh)))
			       (t `(unwrap-event ,event ,(car eh) ,(cadr eh)
				     (values ((lambda ,(cadr eh) ,@(cddr eh)) ,@(cadr eh)) t))))
			  (when handled
			    (setf ,world-sym retval))))
	 ,world-sym))))
