;;;; package.lisp

(defpackage #:ltk3d
  (:use #:cl)
  (:export #:main
           #:make-pt3d
           #:create-pt3d
           #:make-matrix
           #:get-mat
           #:set-mat
           #:dot
           #:get-row
           #:xform
           #:get-pt
           #:set-pt
           #:mat-mul
           #:run-tests
           #:rotate-x
           #:rotate-y
           #:rotate-z
           #:create-matrix))

