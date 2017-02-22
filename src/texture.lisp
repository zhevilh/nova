(in-package :nova)

(export 'load-texture)
(defun load-texture (path)
  "string -> IO texture"
  (using-render-context (renderer)
    (sdl2:create-texture-from-surface
     renderer (sdl2-image:load-image path))))
