(asdf:defsystem :nova
  :description "A Common Lisp game engine using SDL2."
  :author "William Flageol"
  :license "MIT"
  :serial t
  :pathname "src/"
  :components ((:file "../package")
	       (:file "geometry")
	       (:file "timing")
	       (:file "resources")
	       (:file "colors")
	       (:file "graphics-2d")
	       (:file "text")
	       (:file "audio")
	       (:file "events")
	       (:file "joystick")
	       (:file "form")
	       (:file "nova"))
  :depends-on
  (:alexandria
   :ether :cl-fad :cl-annot :sdl2-image :sdl2-mixer :sdl2 :sdl2-ttf
   :bordeaux-threads :lisp-unit
   :trivial-garbage))
