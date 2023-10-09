;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; shapes.lisp

(in-package #:cl-svg/test)

(nst:def-test-group line-tests (test-scene)
  (nst:def-test line (:xml= "<line x1='1' y1='2' x2='3' y2='4' fill='red'/>")
    (cl-svg:draw scene (:line :x1 1 :y1 2 :x2 3 :y2 4)
        :fill "red"))

  (nst:def-test line-without-x1 (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:line :y1 2 :x2 3 :y2 4)))

  (nst:def-test line-without-y1 (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:line :x1 1 :x2 3 :y2 4)))

  (nst:def-test line-without-x2 (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:line :x1 1 :y1 2 :y2 4)))

  (nst:def-test line-without-y2 (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:line :x1 1 :y1 2 :x2 3)))

  (nst:def-test degenerate-line (:xml= "<line/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:line)))))

(nst:def-test-group rect-tests (test-scene)
  (nst:def-test rect (:xml= "<rect x='1' y='2' width='3' height='4' fill='red'/>")
    (cl-svg:draw scene (:rect :x 1 :y 2 :width 3 :height 4)
        :fill "red"))

  (nst:def-test rect-without-x (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:rect :y 2 :width 3 :height 4)))

  (nst:def-test rect-without-y (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:rect :x 1 :width 3 :height 4)))

  (nst:def-test rect-without-width (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:rect :x 1 :y 2 :height 4)))

  (nst:def-test rect-without-height (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:rect :x 1 :y 2 :width 3)))

  (nst:def-test degenerate-rect (:xml= "<rect/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:rect)))))

(nst:def-test-group ellipse-tests (test-scene)
  (nst:def-test ellipse (:xml= "<ellipse cx='1' cy='2' rx='3' ry='4' fill='red'/>")
    (cl-svg:draw scene (:ellipse :cx 1 :cy 2 :rx 3 :ry 4)
                 :fill "red"))

  (nst:def-test ellipse-without-cx (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:ellipse :cy 2 :rx 3 :ry 4)))

  (nst:def-test ellipse-without-cy (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:ellipse :cx 1 :rx 3 :ry 4)))

  (nst:def-test ellipse-without-rx (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:ellipse :cx 1 :cy 2 :ry 4)))

  (nst:def-test ellipse-without-ry (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:ellipse :cx 1 :cy 2 :rx 3)))

  (nst:def-test degenerate-ellipse (:xml= "<ellipse/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:ellipse)))))

(nst:def-test-group circle-tests (test-scene)
  (nst:def-test circle (:xml= "<circle cx='1' cy='2' r='3' fill='red'/>")
    (cl-svg:draw scene (:circle :cx 1 :cy 2 :r 3)
                 :fill "red"))

  (nst:def-test circle-without-cx (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:circle :cy 2 :r 3)))

  (nst:def-test circle-without-cy (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:circle :cx 1 :r 3)))

  (nst:def-test circle-without-r (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:circle :cx 1 :cy 2)))

  (nst:def-test degenerate-circle (:xml= "<circle/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:circle)))))
