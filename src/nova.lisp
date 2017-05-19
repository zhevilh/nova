(in-package :nova)
(annot:enable-annot-syntax)

@export
(defparameter *start-game-in-separate-thread* t)

(defvar *current-timestamp* 0)

(defmacro with-window-gl-context (window-sym (&key title (x :centered) (y :centered) (w 800) (h 600)) &body body)
  `(sdl2:with-window (,window-sym :title ,title :x ,x :y ,y :w ,w :h ,h :flags '(:opengl))
     (with-init-gl ,window-sym ()
       (sdl2:hide-window ,window-sym)
       (sdl2:show-window ,window-sym)
       ,@body)))

@export
(defun game (title w h load-initial-scene-fn initial-world
	     &key
	       (fps-limit 60)
	       max-frame-time
	       (show-cursor nil))
  "string -> int -> int -> scene -> world -> IO"
  (flet
      ((run-game ()
	 (unwind-protect
	      (progn
		(sdl2:with-init (:everything)
		  (when (not show-cursor) (sdl2:hide-cursor))
		  (with-audio (:ogg :mp3)
		    (with-window-gl-context window (:title title :w w :h h)
		      (configure-sprite-shader!)
		      (update-resolution! w h)
		      (let ((ms-per-frame (/ 1000 fps-limit))
			    (*current-timestamp* (sdl2:get-ticks))
			    (current-scene (funcall load-initial-scene-fn))
			    (world initial-world))
			(labels! ((get-time () *current-timestamp*))
			  (macrolet ((handle-update (update-operation)
				       `(if current-scene
					    (setf world ,update-operation)
					    (sdl2:push-event :quit))))
			    (flet ((handle-key-event (event-type keysym)
				     (handle-update (scene-event current-scene
								 (list event-type keysym)
								 world))))
			      (labels! ((change-scene (next-scene)
						      (setf current-scene next-scene)))
				(sdl2:with-event-loop ()
				  (:keydown (:keysym keysym) (handle-key-event :keydown keysym))
				  (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
						(handle-update (scene-event current-scene
									    (list :mousemotion x y xrel yrel state)
									    world)))
				  (:mousewheel (:y y :direction direction)
					       (handle-update (scene-event current-scene
									   (list :mousewheel (if (= direction 0)
												 y
												 (* -1 y)))
									   world)))
				  (:mousebuttondown (:button button)
						    (handle-update (scene-event current-scene
										(list :mousebuttondown button) world)))
				  (:idle ()
					 (let ((remaining-time
						(floor (- ms-per-frame
							  (- (sdl2:get-ticks) *current-timestamp*)))))
					   (when (> remaining-time 0)
					     (sdl2:delay remaining-time)))
					 (setf *current-timestamp*
					       (if (and max-frame-time
							(> (- (sdl2:get-ticks)
							      *current-timestamp*)
							   max-frame-time))
						   (+ *current-timestamp* max-frame-time)
						   (sdl2:get-ticks)))
					 (handle-update (scene-update current-scene world))
					 (when current-scene
					   (gl:clear :color-buffer)
					   (scene-draw current-scene world)
					   (sdl2:gl-swap-window window)))
				  (:quit () t)))))))))))
	   (tg:gc))))
    (if *start-game-in-separate-thread*
	(bordeaux-threads:make-thread #'run-game)
	(run-game))))

@export
(defun change-scene (next-scene)
  "scene -> IO"
  (declare (ignore next-scene))
  (error "This function can only be called within the scene-event or scene-update method of a scene."))


@export
(defun get-time ()
  "nil -> int"
  (error "This function can only be called from within a scene's methods."))
