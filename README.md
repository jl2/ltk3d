This is a very simple implementation of 3D drawing using LTK.  Right now it only handles lines, but can draw 3D lines using a perspective transformation.

My next step is to clean up the code a bit, but after that I plan on using it as a base for a simple surface/curve plotter, and also a 3D turtle graphics system.

Sample usage:

```commonlisp
    * (ql:quickload 'ltk3d)
    To load "ltk3d":
      Load 1 ASDF system:
        ltk3d
    ; Loading "ltk3d"
    .............
    (LTK3D)
    * (ltk3d:run-tests)
    All tests passed!
    NIL
    * (ltk3d:main)
    NIL
```

