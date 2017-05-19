(in-package :%nova)
(annot:enable-annot-syntax)

(defvar *gl-context* nil)

(defvar *sprite-shader-program*)
(defvar *sprite-vertex-array*)

(defvar *current-program*)

(defun alloc-gl-array (type seq)
  (let* ((length (length seq))
	 (array (gl:alloc-gl-array type length)))
    (loop :for e :in seq
       :and i = 0 :then (1+ i)
       :do (setf (gl:glaref array i) e))
    array))

(defmacro with-gl-array (array-sym type seq &body body)
  `(let ((,array-sym (alloc-gl-array ,type ,seq)))
     ,@body
     (gl:free-gl-array ,array-sym)))

@export
(defmacro with-init-gl (sdl-window (&key (blend t) viewport-size) &body body)
  "Inits OpenGL on an SDL window.
Note: viewport-size is a values form containing width and height."
  (with-gensyms (gl-context)
    `(progn
       (init-gl-attributes)
       (sdl2:with-gl-context (,gl-context ,sdl-window)
	 (init-gl-context ,gl-context ,sdl-window ,blend ,viewport-size)
	 ;; For now, we'll support only one GL Context. Not sure it's useful to have more anyways.
	 (setf *gl-context* ,gl-context)
	 (unwind-protect
	      (progn ,@body)
	   (setf *gl-context* nil))))))

(defun init-gl-attributes ()
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :doublebuffer 1))

(defun init-gl-context (gl-context sdl-window blend viewport-size)
  (sdl2:gl-make-current sdl-window gl-context)
  (when blend
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha))
  (multiple-value-bind (w h) (or viewport-size
				 (sdl2:get-window-size sdl-window))
    (gl:viewport 0 0 w h)))

@export
(defmacro when-gl-context (&body body)
  "Exedcutes the body only if a gl-context is active."
  ;; Using a function call so the macro doesn't capture the *gl-context* variable in the GC thread.
  `(when (get-gl-context)
     ,@body))

(defun get-gl-context ()
  *gl-context*)

(defmacro with-shader (shader-sym type &body body)
  `(let-protect ((,shader-sym (gl:create-shader ,type)
			      (gl:delete-shader ,shader-sym)))
     ,@body))

(defmacro with-shaders ((&rest shader-decl) &body body)
  (if shader-decl
      (destructuring-bind (shader-sym type) (car shader-decl)
	`(with-shader ,shader-sym ,type
	   (with-shaders ,(cdr shader-decl) ,@body)))
      `(progn ,@body)))

(defun load-shader! (shader source)
  "gl-shader -> string -> nil"
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (cffi:with-foreign-object (success '%gl:int)
    (%gl:get-shader-iv shader :compile-status success)
    (when (= 0 (cffi:mem-ref success '%gl:int))
      (error (gl:get-shader-info-log shader)))))

(defun create-program! (&rest shaders)
  "gl-shader list -> program"
  (let ((program (gl:create-program)))
    (dolist (shader shaders)
      (gl:attach-shader program shader))
    (gl:link-program program)
    (cffi:with-foreign-object (success '%gl:int)
      (%gl:get-program-iv program :link-status success)
      (when (= 0 (cffi:mem-ref success '%gl:int))
	(error (gl:get-program-info-log program)))
      program)))

(defun load-shader-program! (vertex-source fragment-source)
  (with-shaders ((vertex-shader :vertex-shader)
		 (fragment-shader :fragment-shader))
    (load-shader! vertex-shader vertex-source)
    (load-shader! fragment-shader fragment-source)
    (create-program! vertex-shader fragment-shader)))

(defmacro with-program (program &body body)
  `(let ((*current-program* ,program))
     (gl:use-program ,program)
     ,@body))

(defun get-shader-source (filename)
  (ether-io:file->string! (merge-pathnames filename (merge-pathnames "src/shaders/" (asdf:system-source-directory :nova)))))

(export 'configure-sprite-shader!)
(defun configure-sprite-shader! ()
  (setf *sprite-shader-program* (load-shader-program! *sprite-vertex-glsl*
						      *sprite-fragment-glsl*))
  (setf *sprite-vertex-array* (gl:gen-vertex-array))
  (with-gl-array vertices :float '(0.0 1.0 0.0 1.0
				   1.0 0.0 1.0 0.0
				   0.0 0.0 0.0 0.0
				   
				   0.0 1.0 0.0 1.0
				   1.0 1.0 1.0 1.0
				   1.0 0.0 1.0 0.0)
    (let ((vertex-buffer (gl:gen-buffer)))
      (gl:bind-buffer :array-buffer vertex-buffer)
      (gl:buffer-data :array-buffer :static-draw vertices)

      (gl:bind-vertex-array *sprite-vertex-array*)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil
				(* 4 (cffi:foreign-type-size :float)) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0))))

@export
(defun update-resolution! (w h)
  (with-program *sprite-shader-program*
    (set-uniform-mat4 "projection" (projection-matrix-2d w h))))

@export
(defun set-uniform-mat4 (uniform-name mat4)
  (gl:uniform-matrix-4fv (gl:get-uniform-location *current-program* uniform-name)
			 (m4-n:transpose mat4)))

@export
(defun set-uniform-fv (uniform-name value)
  (gl:uniformfv (gl:get-uniform-location *current-program* uniform-name)
		value))

@export
(defmacro with-bind-texture (target texture &body body)
  `(progn
     (gl:bind-texture ,target ,texture)
     ,@body
     (gl:bind-texture ,target 0)))

@export
(defun make-texture-2d (pixels-data input-format output-format w h)
  (let ((texture (gl:gen-texture)))
    (with-bind-texture :texture-2d texture
      (gl:tex-image-2d :texture-2d 0 input-format w h 0
		       output-format :unsigned-byte pixels-data)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear))
    texture))

@export
(defun gl-draw-sprite! (gl-texture x y w h sx sy sw sh tw th rad r g b)
  (with-program *sprite-shader-program*
    (set-uniform-mat4
     "modelView"
     (reduce 'm4-n:*
	     `(,(m4:translation (v3:make (float x) (float y) 0.0))
		,.(when (/= rad 0.0)
		    `(,(m4:translation (v3:make (* 0.5 w) (* 0.5 h) 0.0))
		       ,(m4:rotation-z rad)
		       ,(m4:translation (v3:make (* -0.5 w) (* -0.5 h) 0.0))))
		,(m4:scale (v3:make (float w) (float h) 1.0)))))
    (set-uniform-mat4
     "texCoordTransform"
     (reduce 'm4-n:*
	     (list (projection-matrix-texture (float tw) (float th))
		   (m4:translation (v3:make (float sx) (float sy) 0.0))
		   (m4:scale (v3:make (float sw) (float sh) 1.0)))))
    (set-uniform-fv "spriteColor" (array r g b))

    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d gl-texture)

    (gl:bind-vertex-array *sprite-vertex-array*)
    (gl:draw-arrays :triangles 0 6)
    (gl:bind-vertex-array 0)))
