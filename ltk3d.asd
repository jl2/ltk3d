;;;; ltk3d.asd

(asdf:defsystem #:ltk3d
  :description "Simple 3D drawing with ltk"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:ltk)
  :serial t
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "ltk3d")))

