(in-package :nova)

(defvar *render-context*)

(defclass render-context ()
  ((window :initarg :window :reader window)
   (renderer :initarg :renderer :reader renderer)))

(defmacro using-render-context (slots-entries &body form)
  "Defines a scope using slots from the current *render-context*."
  `(with-slots ,slots-entries *render-context*
     ,@form))

(defmacro with-render-context (render-context slot-entries &body form)
  "Sets the current *render-context* and define a scope using slots from it."
  `(let ((*render-context* ,render-context))
     (using-render-context ,slot-entries
       ,@form)))
