This is a very simple implementation of 3D drawing using LTK.  It's primarily for me to expirment with some computer graphics concepts like projects and transformations, as well as some other simple 3D graphics "stuff".

As a demonstration, a simple 3D surface plotter is implemented.

I plan on also implementing a simple 3D turtle graphics system.

Sample usage:

```commonlisp
    * (ql:quickload 'ltk3d)
    To load "ltk3d":
      Load 1 ASDF system:
        ltk3d
    ; Loading "ltk3d"
    .............
    (LTK3D)
    * (ltk3d:plot-function :equation (ltk3d:make-parametric-equation 
                                             :umin (coerce (* -2 pi) 'single-float)
                                             :umax (coerce (* 2 pi) 'single-float)
                                             :vmin (coerce (* -2 pi) 'single-float)
                                             :vmax (coerce (* 2 pi) 'single-float)
                                             :usteps 32 :vsteps 32
                                             :yf (lambda (u v) (* 1.5 (sin u) (cos v)))))
    NIL
```

![Screenshot](http://www.laroccophoto.com/photos/i-9HrFJf6/0/XL/i-9HrFJf6-XL.png)
