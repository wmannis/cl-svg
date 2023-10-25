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

(nst:def-test-group polyline-tests (test-scene)
  (nst:def-test polyline (:xml= "<polyline points=' 1,2 3,4'/>")
    (cl-svg:draw scene (:polyline :points (cl-svg:points '((1 2) (3 4))))))

  (nst:def-test polyline-without-points (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:polyline)))

  (nst:def-test degenerate-polyline (:xml= "<polyline/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:polyline)))))

(nst:def-test-group polygon-tests (test-scene)
  (nst:def-test polygon (:xml= "<polygon points='some string here'/>")
    (cl-svg:draw scene (:polygon :points "some string here")))

  (nst:def-test polygon-without-points (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:polygon)))

  (nst:def-test degenerate-polygon (:xml= "<polygon/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:polygon)))))

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

(nst:def-test-group path-tests (test-scene)
  (nst:def-test path (:xml= "<path d='some string here'/>")
    (cl-svg:draw scene (:path :d "some string here")))

  (nst:def-test path-without-d (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:path)))

  (nst:def-test degenerate-path (:xml= "<path/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:path)))))

(nst:def-test-group use-tests (test-scene)
  (nst:def-test use (:xml= "<use xlink\:href='some-id'/>")
    (cl-svg:draw scene (:use :xlink-href "some-id")))

  (nst:def-test use-without-xlink-href (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:use)))

  (nst:def-test degenerate-use (:xml= "<use/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:use)))))

(nst:def-test-group image-tests (test-scene)
  (nst:def-test image (:xml= "<image x='1' y='2' width='3' height='4' xlink\:href='some-id'/>")
    (cl-svg:draw scene (:image :x 1 :y 2 :width 3 :height 4 :xlink-href "some-id")))

  (nst:def-test image-without-x (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:image :y 2 :width 3 :height 4 :xlink-href "some-id")))

  (nst:def-test image-without-y (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:image :x 1 :width 3 :height 4 :xlink-href "some-id")))

  (nst:def-test image-without-width (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:image :x 1 :y 2 :height 4 :xlink-href "some-id")))

  (nst:def-test image-without-height (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:image :x 1 :y 2 :width 3 :xlink-href "some-id")))

  (nst:def-test image-without-xlink-href (:err :type cl-svg:missing-attributes)
    (cl-svg:draw scene (:image :x 1 :y 2 :width 3 :height 4)))

  (nst:def-test degenerate-image (:xml= "<image/>")
    (cl-svg:without-attribute-check
      (cl-svg:draw scene (:image)))))
