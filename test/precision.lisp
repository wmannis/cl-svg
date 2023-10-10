;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; shapes.lisp

(in-package #:cl-svg/test)

(nst:def-test-group set-precision-tests (test-scene)
  (:each-cleanup (cl-svg:set-float-precision #.cl-svg:*float-format-precision*))

  (nst:def-test default-precision-is-two (:eql 2)
    cl-svg:*float-format-precision*)

  (nst:def-test precision-can-be-set-to-zero (:xml= "<circle r='1.'/>")
    (prog1
        (cl-svg:without-attribute-check
          (cl-svg:draw scene (:circle :r 1.2345678)))
      (cl-svg:set-float-precision 0)))

  (nst:def-test precision-can-be-set-to-four (:xml= "<circle r='1.2346'/>")
    (prog1
        (cl-svg:without-attribute-check
          (cl-svg:draw scene (:circle :r 1.2345678)))
      (cl-svg:set-float-precision 4)))

  (nst:def-test integers-are-not-subject-to-precision (:xml= "<circle r='1'/>")
    (prog1
        (cl-svg:without-attribute-check
          (cl-svg:draw scene (:circle :r 1)))
      (cl-svg:set-float-precision 4)))

  (nst:def-test near-zero-is-made-zero-to-be-nice-to-browsers (:xml= "<circle r='0'/>")
    (prog1
        (cl-svg:without-attribute-check
          (cl-svg:draw scene (:circle :r 0.000001)))
      (cl-svg:set-float-precision 6))))
