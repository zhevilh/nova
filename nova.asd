(asdf:defsystem :nova
  :description "A Common Lisp game engine using SDL2."
  :author "William Flageol"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "../package")
	       (:file "render")
	       (:file "texture")
	       (:file "audio")
	       (:file "drawing")
	       (:file "sprite")
	       (:file "animation")
	       (:file "scene")
	       (:file "event")
               (:file "nova"))
  :depends-on (:alexandria :sdl2 :sdl2-image :sdl2-mixer :sdl2-ttf :ether))

