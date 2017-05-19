(defpackage :%nova
  (:use :cl :alexandria :ether))

(defpackage :nova
  (:use :cl :%nova :alexandria :ether))

(defpackage :nova-2d
  (:use :cl :%nova :nova :alexandria :ether))
