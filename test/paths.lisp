;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; paths.lisp

(in-package #:cl-svg/test)

(nst:def-test-group path-tests ()
  (nst:def-test make-path (:equal "")
    (cl-svg:make-path))

  (nst:def-test move-to (:path= "M1 2")
    (cl-svg:path
      (cl-svg:move-to 1 2)))

  (nst:def-test move-to-r (:path= "m1 2")
    (cl-svg:path
      (cl-svg:move-to-r 1 2)))

  (nst:def-test line-to (:path= "L1 2")
    (cl-svg:path
      (cl-svg:line-to 1 2)))

  (nst:def-test line-to-r (:path= "l1 2")
    (cl-svg:path
      (cl-svg:line-to-r 1 2)))

  (nst:def-test horizontal-to (:path= "H1")
    (cl-svg:path
      (cl-svg:horizontal-to 1)))

  (nst:def-test horizontal-to-r (:path= "h1")
    (cl-svg:path
      (cl-svg:horizontal-to-r 1)))

  (nst:def-test vertical-to (:path= "V1")
    (cl-svg:path
      (cl-svg:vertical-to 1)))

  (nst:def-test vertical-to-r (:path= "v1")
    (cl-svg:path
      (cl-svg:vertical-to-r 1)))

  (nst:def-test curve-to (:path= "C1 2 3 4 5 6")
    (cl-svg:path
      (cl-svg:curve-to 1 2 3 4 5 6)))

  (nst:def-test curve-to-r (:path= "c1 2 3 4 5 6")
    (cl-svg:path
      (cl-svg:curve-to-r 1 2 3 4 5 6)))

  (nst:def-test smooth-curve-to (:path= "S1 2 3 4")
    (cl-svg:path
      (cl-svg:smooth-curve-to 1 2 3 4)))

  (nst:def-test smooth-curve-to-r (:path= "s1 2 3 4")
    (cl-svg:path
      (cl-svg:smooth-curve-to-r 1 2 3 4)))

  (nst:def-test quadratic-curve-to (:path= "Q1 2 3 4")
    (cl-svg:path
      (cl-svg:quadratic-curve-to 1 2 3 4)))

  (nst:def-test quadratic-curve-to-r (:path= "q1 2 3 4")
    (cl-svg:path
      (cl-svg:quadratic-curve-to-r 1 2 3 4)))

  (nst:def-test smooth-quadratic-curve-to (:path= "T1 2")
    (cl-svg:path
      (cl-svg:smooth-quadratic-curve-to 1 2)))

  (nst:def-test smooth-quadratic-curve-to-r (:path= "t1 2")
    (cl-svg:path
      (cl-svg:smooth-quadratic-curve-to-r 1 2)))

  (nst:def-test arc-to (:path= "A1 2 3 4 5 6 7")
    (cl-svg:path
      (cl-svg:arc-to 1 2 3 4 5 6 7)))

  (nst:def-test arc-to-r (:path= "a1 2 3 4 5 6 7")
    (cl-svg:path
      (cl-svg:arc-to-r 1 2 3 4 5 6 7)))

  (nst:def-test close-path (:path= "z")
    (cl-svg:path
      (cl-svg:close-path)))

  (nst:def-test multiple-commands-in-one-with-path (:path= "M1 2t3 4z")
    (let ((path (cl-svg:make-path)))
      (cl-svg:with-path path
        (cl-svg:move-to 1 2)
        (cl-svg:smooth-quadratic-curve-to-r 3 4)
        (cl-svg:close-path))
      path))

  (nst:def-test multiple-with-paths (:path= "M1 2t3 4z")
    (let ((path (cl-svg:make-path)))
      (cl-svg:with-path path
        (cl-svg:move-to 1 2))
      (cl-svg:with-path path
        (cl-svg:smooth-quadratic-curve-to-r 3 4)
        (cl-svg:close-path))
      path)))
