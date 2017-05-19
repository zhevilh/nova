(asdf:defsystem :nova
  :description "A Common Lisp game engine using SDL2."
  :author "William Flageol"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "../package")
	       (:file "matrices")
	       (:file "shaders")
	       (:file "gl-utils")
	       (:file "drawing")
	       (:file "drawing-2d")
	       (:file "audio")
	       (:file "event")
	       (:file "scene")
	       (:file "physics-2d")
               (:file "nova"))
  :depends-on
  (:alexandria
   :ether :cl-fad :bordeaux-threads :trivial-garbage :cl-plus-c :rtg-math
   :cl-annot :cl-opengl :fset :sdl2 :sdl2-image :sdl2-mixer))

