;;;; ltk3d.lisp

(in-package #:ltk3d)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 1) (debug 1)))

;; Not the most efficient vector and matrix code ever created, but should be "good enough"
;; to get started.

;; pt3d is an array of 4 single-float values
(defstruct pt3d
  (pt (make-array 4 :element-type 'single-float :initial-element 0.0)))

;; Convienence function to create a pt3d with x,y,z and possible w values
;; (declaim (inline create-pt3d))
(defun create-pt3d (x y z &optional (w 1.0d0))
  ;; (declare (type single-float x y z w))
  (the pt3d (make-pt3d :pt (make-array 4 :element-type 'single-float :initial-contents (list (coerce x 'single-float)
                                                                                             (coerce y 'single-float)
                                                                                             (coerce z 'single-float)
                                                                                             (coerce w 'single-float))))))

(declaim (inline get-pt))
(defun get-pt (pt idx)
  (declare (type pt3d pt)
           (type (unsigned-byte 32) idx))
  (the single-float (aref (pt3d-pt pt) idx)))

(declaim (inline set-pt))
(defun set-pt (pt idx val)
  (declare (type pt3d pt)
           (type (unsigned-byte 32))
           (type single-float val))
  (setf (aref (pt3d-pt pt) idx) val))

(declaim (inline pt3d-x))
(defun pt3d-x (pt)
  (get-pt pt 0))

(declaim (inline pt3d-y))
(defun pt3d-y (pt)
  (get-pt pt 1))

(declaim (inline pt3d-z))
(defun pt3d-z (pt)
  (get-pt pt 2))

(declaim (inline pt3d-w))
(defun pt3d-w (pt)
  (get-pt pt 3))

(defun (setf pt3d-x) (pt nx)
  (set-pt pt 0 nx))

(defun (setf pt3d-y) (pt ny)
  (set-pt pt 1 ny))

(defun (setf pt3d-z) (pt nz)
  (set-pt pt 2 nz))

(defun (setf pt3d-w) (pt nw)
  (set-pt pt 3 nw))


(defstruct matrix
  (mat (make-array 16 :element-type 'single-float :initial-element 0.0)))

(declaim (inline create-matrix))
(defun create-matrix (m00 m01 m02 m03
                      m10 m11 m12 m13
                      m20 m21 m22 m23
                      m30 m31 m32 m33)
  (declare (type single-float
                 m00 m01 m02 m03
                 m10 m11 m12 m13
                 m20 m21 m22 m23
                 m30 m31 m32 m33))
  (the matrix
       (make-matrix
        :mat (make-array
              16
              :element-type 'single-float
              :initial-contents
              (list m00 m01 m02 m03
                    m10 m11 m12 m13
                    m20 m21 m22 m23
                    m30 m31 m32 m33)))))

(declaim (inline rotate-x rotate-y rotate-z
                 scale-x scale-y scale-z scale
                 translate))
(defun rotate-x (radians)
  (let ((fr (coerce radians 'single-float)))
    (create-matrix 1.0 0.0 0.0 0.0
                   0.0 (cos fr) (- (sin fr)) 0.0
                   0.0 (sin fr) (cos fr) 0.0
                   0.0 0.0 0.0 1.0)))

(defun rotate-y (radians)
  (let ((fr (coerce radians 'single-float)))
    (create-matrix (cos fr) 0.0 (sin fr) 0.0
                   0.0 1.0 0.0 0.0
                   (- (sin fr)) 0.0 (cos fr)  0.0
                   0.0 0.0 0.0 1.0)))

(defun rotate-z (radians)
  (let ((fr (coerce radians 'single-float)))
    (create-matrix (cos fr) (- (sin fr)) 0.0 0.0
                   (sin fr) (cos fr) 0.0 0.0
                   0.0 0.0 1.0 0.0
                   0.0 0.0 0.0 1.0)))
(defun scale-x (s)
  (let ((ns (coerce s 'single-float)))
    (create-matrix ns 0.0 0.0 0.0
                   0.0 1.0 0.0 0.0
                   0.0 0.0 1.0 0.0
                   0.0 0.0 0.0 1.0)))
(defun scale-y (s)
  (let ((ns (coerce s 'single-float)))
    (create-matrix 1.0 0.0 0.0 0.0
                   0.0 ns 0.0 0.0
                   0.0 0.0 1.0 0.0
                   0.0 0.0 0.0 1.0)))

(defun scale-z (s)
  (let ((ns (coerce s 'single-float)))
    (create-matrix 1.0 0.0 0.0 0.0
                   0.0 1.0 0.0 0.0
                   0.0 0.0 ns 0.0
                   0.0 0.0 0.0 1.0)))

(defun scale (s)
  (let ((ns (coerce s 'single-float)))
    (create-matrix ns 0.0 0.0 0.0
                   0.0 ns 0.0 0.0
                   0.0 0.0 ns 0.0
                   0.0 0.0 0.0 1.0)))

(defun translate (xo yo zo)
  (let ((nx (coerce xo 'single-float))
        (ny (coerce yo 'single-float))
        (nz (coerce zo 'single-float)))
    (create-matrix 1.0 0.0 0.0 nx
                   0.0 1.0 0.0 ny
                   0.0 0.0 1.0 nz
                   0.0 0.0 0.0 1.0)))

(declaim (inline mat-idx))
(defun mat-idx (i j)
  (declare (type  (unsigned-byte 32) i j))
  (the (unsigned-byte 32) (+ (* j 4) i)))

(declaim (inline get-mat))
(defun get-mat (mat i j)
  (declare (type matrix mat)
           (type (unsigned-byte 32) i j))
  (the single-float (aref (matrix-mat mat) (mat-idx i j))))

(declaim (inline set-mat))
(defun set-mat (mat i j val)
  (declare (type matrix mat)
           (type (unsigned-byte 32) i j)
           (type single-float val))
  (setf (aref (matrix-mat mat) (mat-idx i j)) val))

(declaim (inline dot))
(defun dot (pt1 pt2)
  (declare (type pt3d pt1 pt2))
  (the single-float 
       (+ (* (get-pt pt1 0) (get-pt pt2 0))
          (* (get-pt pt1 1) (get-pt pt2 1))
          (* (get-pt pt1 2) (get-pt pt2 2))
          (* (get-pt pt1 3) (get-pt pt2 3)))))

(declaim (inline get-row))
(defun get-row (mat row)
  (declare (type matrix mat)
           (type (unsigned-byte 32) row))
  (create-pt3d (get-mat mat 0 row) (get-mat mat 1 row) (get-mat mat 2 row) (get-mat mat 3 row)))

(declaim (inline get-col))
(defun get-col (mat col)
  (declare (type matrix mat)
           (type (unsigned-byte 32) col))
  (create-pt3d (get-mat mat col 0) (get-mat mat col 1) (get-mat mat col 2) (get-mat mat col 3)))

(declaim (inline xform))
(defun xform (pt mat)
  (declare (type pt3d pt)
           (type matrix mat))
  (create-pt3d (dot (get-row mat 0) pt)
               (dot (get-row mat 1) pt)
               (dot (get-row mat 2) pt)
               (dot (get-row mat 3) pt)))


(declaim (inline mat-mul))
(defun mat-mul (mat1 mat2)
  (declare (type matrix mat1 mat2))
  (let ((rval (make-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
        (set-mat rval i j 
                 (dot (get-col mat1 i) (get-row mat2 j)))))
    rval))

(defun lines (canvas &rest lines)
  (dolist (ln lines)
    (pt-line canvas (line-pt1 ln) (line-pt2 ln))))

(defstruct line3d 
  (pt1 (make-pt3d) :type pt3d)
  (pt2 (make-pt3d) :type pt3d))

(declaim (inline draw-line3d))
(defun draw-line3d (canvas mat p1 p2)
  (declare (type matrix mat)
           (type pt3d p1 p2))
  (let ((tp1 (xform p1 mat))
        (tp2 (xform p2 mat)))
    (let ((pt1x (pt3d-x tp1))
          (pt1y (pt3d-y tp1))
          (pt2x (pt3d-x tp2))
          (pt2y (pt3d-y tp2)))
      (ltk:create-line* canvas
                        (floor pt1x) (floor pt1y)
                        (floor pt2x) (floor pt2y)))))

(declaim (inline draw-quad))
(defun draw-quad (canvas mat p1 p2 p3 p4)
  (declare (type matrix mat)
           (type pt3d p1 p2 p3 p4))
  (draw-line3d canvas mat p1 p2)
  (draw-line3d canvas mat p2 p3)
  (draw-line3d canvas mat p3 p4)
  (draw-line3d canvas mat p4 p1))

(defun draw-pt3d (canvas mat pt &optional (tick-size 0.125))
  "Draw a point as a mini-axis."
  (let ((tp (create-pt3d (pt3d-x pt) (+ (pt3d-y pt) tick-size) (pt3d-z pt)))
        (bp (create-pt3d (pt3d-x pt) (- (pt3d-y pt) tick-size) (pt3d-z pt)))
        (lp (create-pt3d (- (pt3d-x pt) tick-size) (pt3d-y pt) (pt3d-z pt)))
        (rp (create-pt3d (+ (pt3d-x pt) tick-size) (pt3d-y pt) (pt3d-z pt)))
        (np (create-pt3d (pt3d-x pt) (pt3d-y pt) (+ (pt3d-z pt) tick-size)))
        (fp (create-pt3d (pt3d-x pt) (pt3d-y pt) (- (pt3d-z pt) tick-size))))
    (draw-line3d canvas mat tp bp)
    (draw-line3d canvas mat lp rp)
    (draw-line3d canvas mat np fp)))

(defun random-pt3d (&optional (sz 8.0))
  (ltk3d:create-pt3d (- (random sz) (/ sz 2.0)) (- (random sz) (/ sz 2.0))
                     (- (random sz) (/ sz 2.0)) 1.0))

(defun main (&key
               (width 800) (height 800)
               (left -11.0) (right 11.0)
               (bottom -11.0) (top 11.0)
               (near -11.0) (far 11.0))
  (declare (type single-float left right bottom top near far))
  (ltk:with-ltk ()
    (let* ((canv (make-instance 'ltk:canvas 
                                :master nil
                                :width width
                                :height height))
           (trans (create-matrix
                   (/ (- near far) (- right left)) 0.0 (/ (+ right left) (- right left)) 0.0
                   0.0 (/ (- near far) (- top bottom)) (/ (+ top bottom) (- top bottom)) 0.0
                   0.0 0.0 (- (/ (+ far near) (- far near))) (- (/ (* (- near far) far near) (- far near)))
                   0.0 0.0 -1.0 0.0))
           (screen-trans (create-matrix 
                          (/ width (- right left)) 0.0 0.0 (/ (* -1.0 right width) (- right left))
                          0.0 (/ height (- top bottom)) 0.0 (/ (* -1.0 right height) (- top bottom))
                          0.0 0.0 (- near far) 0.0
                          0.0 0.0 0.0 1.0))
           
           ;; (ntrans (mat-mul (rotate-z (/ pi 1.2)) (mat-mul screen-trans (mat-mul (rotate-y (/ pi 4)) trans))))
           (ntrans (mat-mul (translate (- (* 2.0 pi)) 0 (- (* 2.0 pi))) (mat-mul (rotate-x (/ pi 5)) (mat-mul (rotate-z (/ pi 5)) (mat-mul (rotate-y (/ pi 5))  (mat-mul screen-trans trans)))))))

      (let* ((xmin 0.0)
             (xmax (* pi 4))
             (ymin 0.0)
             (ymax (* pi 4))
             (xsteps 50)
             (ysteps 50)
             (dx (/ (- xmax xmin) xsteps))
             (dy (/ (- ymax ymin) ysteps))
             (cx xmin)
             (cy ymin))
        (flet ((fxy (xv yv)
                 (* 4.0 (sin xv) (cos yv))))
        
          (loop for i below xsteps
             do
               (loop for j below ysteps
                  do
                    (let* ((cx (+ xmin (* dx i)))
                           (cy (+ ymin (* dy j)))
                           (p1 (create-pt3d cx (fxy cx cy) cy))
                           (p2 (create-pt3d (+ cx dx) (fxy (+ cx dx) cy) cy))
                           (p3 (create-pt3d (+ cx dx) (fxy (+ cx dx) (+ cy dy)) (+ cy dy)))
                           (p4 (create-pt3d cx (fxy cx (+ cy dy)) (+ cy dy))))
                      (draw-quad canv ntrans p1 p2 p3 p4))))))
             ;;                      (let ((xp (create-pt3d (coerce i 'single-float) 0.0 0.0))
             ;;     (yp (create-pt3d 0.0 (coerce i 'single-float) 0.0))
             ;;     (zp (create-pt3d 0.0 0.0 (coerce i 'single-float))))
             ;; (draw-pt3d canv ntrans xp)
             ;; (draw-pt3d canv ntrans yp)
             ;; (draw-pt3d canv ntrans zp)))
      
      (ltk:pack canv))))

(declaim (inline tol-equal))
(defun tol-equal (a b eps)
  (< (abs (- a b)) eps))

(defun pt3d= (pt1 pt2 &optional (eps 0.000001))
  (and (tol-equal (pt3d-x pt1) (pt3d-x pt2) eps)
       (tol-equal (pt3d-y pt1) (pt3d-y pt2) eps)
       (tol-equal (pt3d-z pt1) (pt3d-z pt2) eps)
       (tol-equal (pt3d-w pt1) (pt3d-w pt2) eps)))

(defun matrix= (m1 m2 &optional (eps 0.000001))
  (let ((good-so-far t))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf good-so-far (and good-so-far (tol-equal (get-mat m1 i j) (get-mat m2 i j) eps)))))
    good-so-far))

(defun xform-test (pt xf npt)
  (assert (pt3d= npt (xform pt xf))))

(defun mat-mul-test (m1 m2 mr)
  (assert (matrix= mr (mat-mul m1 m2))))

(defun run-tests () 
  ;; Test identity
  (xform-test (create-pt3d 1.0 2.0 3.0)
              (create-matrix 1.0 0.0 0.0 0.0
                             0.0 1.0 0.0 0.0
                             0.0 0.0 1.0 0.0
                             0.0 0.0 0.0 1.0)
              (create-pt3d 1.0 2.0 3.0))

  ;; Test scale by 2
  (xform-test (create-pt3d 1.0 2.0 3.0)
              (create-matrix 2.0 0.0 0.0 0.0
                             0.0 2.0 0.0 0.0
                             0.0 0.0 2.0 0.0
                             0.0 0.0 0.0 2.0)
              (create-pt3d 2.0 4.0 6.0 2.0))

  ;; Test rotating 90 degrees
  (xform-test (create-pt3d 1.0 0.0 0.0)
              (rotate-z (/ pi 2.0))
              (create-pt3d 0.0 1.0 0.0 1.0))

  ;; Test rotating 180 degrees
  (xform-test (create-pt3d 1.0 0.0 0.0)
              (rotate-z pi)
              (create-pt3d -1.0 0.0 0.0 1.0))

  ;; Test that rotating along x doesn't change a point on x
  (xform-test (create-pt3d 1.0 0.0 0.0)
              (rotate-x pi)
              (create-pt3d 1.0 0.0 0.0 1.0))
  
  (mat-mul-test (create-matrix 1.0 0.0 0.0 0.0
                               0.0 1.0 0.0 0.0
                               0.0 0.0 1.0 0.0
                               0.0 0.0 0.0 1.0)
                (create-matrix 2.0 0.0 0.0 0.0
                               0.0 2.0 0.0 0.0
                               0.0 0.0 2.0 0.0
                               0.0 0.0 0.0 2.0)
                (create-matrix 2.0 0.0 0.0 0.0
                               0.0 2.0 0.0 0.0
                               0.0 0.0 2.0 0.0
                               0.0 0.0 0.0 2.0))
  (mat-mul-test (create-matrix 1.0 0.0 0.0 0.0
                               0.0 1.0 0.0 0.0
                               0.0 0.0 1.0 0.0
                               0.0 0.0 0.0 1.0)
                (create-matrix 2.0 0.0 0.0 0.0
                               0.0 0.5 0.0 0.0
                               0.0 0.0 1.0 0.0
                               0.0 0.0 0.0 1.0)
                (create-matrix 2.0 0.0 0.0 0.0
                               0.0 0.5 0.0 0.0
                               0.0 0.0 1.0 0.0
                               0.0 0.0 0.0 1.0))
  (format t "All tests passed!~%"))


