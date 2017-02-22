(in-package :nova)

(defmacro with-window-renderer ((&key title x y w h index renderer-flags) &body form)
  "Creates a window and renderer and binds it to the *render-context* variable."
  (with-gensyms (window renderer)
    `(sdl2:with-window (,window :title ,title :x (or ,x :centered) :y (or ,y :centered)
				:w (or ,w 800) :h (or ,h 600))
       (sdl2:with-renderer (,renderer ,window :index ,index :flags ,renderer-flags)
	 (format t "Created window and renderer. Showing window...")
	 (sdl2:hide-window ,window)
	 (sdl2:show-window ,window)
	 (format t "OK.~%")
	 
	 (let ((*render-context* (make-instance 'render-context :window ,window :renderer ,renderer)))
	   ,@form)))))

(defmacro with-render (&body form)
  "Executes operations in a rendering pass."
  `(using-render-context (renderer)
     (sdl2:render-clear renderer)
     ,@form
     (sdl2:render-present renderer)))

(defun area->sdl-rect (area)
  (with-slots (x y w h) area
    (sdl2:make-rect x y w h)))

(export 'render-text)
(defun render-text (font text color)
  "font -> string -> texture"
  (using-render-context (renderer)
    (with-slots (r g b a) (resolve-color color)
      (let* ((surface (sdl2-ttf:render-text-solid font text r g b a))
	     (texture (sdl2:create-texture-from-surface renderer surface)))
	(sdl2:free-surface surface)
	texture))))

(export 'game)
(defun game (title w h initial-scene initial-world &key (fps 60))
  "string -> int -> int -> scene -> world -> IO"
  (declare (optimize speed))
  (format t "Initializing SDL2...")
  (sdl2:with-init (:everything)
    (format t "OK.~%")
    (with-audio (:ogg :mp3)
      (with-ttf
	(with-window-renderer (:title title :w w :h h)
	  (scene-load initial-scene)
	  (let ((ticks-per-frame (/ 1000 fps))
		(last-frame-ticks (sdl2:get-ticks))
		(current-scene initial-scene)
		(world initial-world))
	    (macrolet ((handle-update (update-operation)
			 `(if current-scene
			      (setf world ,update-operation)
			      (sdl2:push-event :quit))))
	      (flet ((handle-key-event (event-type keysym)
		       (handle-update (scene-event current-scene
						   (list event-type keysym)
						   world))))
		(labels! ((change-scene (next-scene) (setf current-scene next-scene))
			  (get-actor (key) (gethash key (actors current-scene))))
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
		    (:idle ()
			   (handle-update (scene-update current-scene world))
			   (when current-scene
			     (let ((ticks-remaining (floor (- ticks-per-frame
							      (- (sdl2:get-ticks) last-frame-ticks)))))
			       (when (> ticks-remaining 0)
				 (sdl2:delay ticks-remaining))
			       (setf last-frame-ticks (sdl2:get-ticks))
			       (with-render
				 (scene-draw current-scene world)))))
		    (:quit () t)))))))))))
