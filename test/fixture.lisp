;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; fixture.lisp

(in-package #:cl-svg/test)

(nst:def-fixtures test-scene ()
  (scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
