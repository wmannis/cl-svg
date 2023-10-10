;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; transforms.lisp

(in-package #:cl-svg/test)

(nst:def-test-group scale-tests (test-scene)
  (nst:def-test bare-scale-sx (:xml= "<path transform='scale(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:scale 1)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-scale-sx (:xml= "<path transform='scale(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:scale 1))
        (cl-svg:draw scene (:path)))))

  (nst:def-test bare-scale-sx+sy (:xml= "<path transform='scale(1, 2)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:scale 1 2)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-scale-sx+sy (:xml= "<path transform='scale(1, 2)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:scale 1 2))
        (cl-svg:draw scene (:path))))))

(nst:def-test-group translate-tests (test-scene)
  (nst:def-test bare-translate-tx+ty (:xml= "<path transform='translate(1, 2)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:translate 1 2)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-translate-tx (:xml= "<path transform='translate(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:translate 1))
        (cl-svg:draw scene (:path)))))

  (nst:def-test bare-translate-tx (:xml= "<path transform='translate(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:translate 1)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-translate-tx+ty (:xml= "<path transform='translate(1, 2)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:translate 1 2))
        (cl-svg:draw scene (:path))))))

(nst:def-test-group rotate-tests (test-scene)
  (nst:def-test bare-rotate-angle (:xml= "<path transform='rotate(1, 0, 0)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:rotate 1)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-rotate-angle (:xml= "<path transform='rotate(1, 0, 0)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:rotate 1))
        (cl-svg:draw scene (:path)))))

  (nst:def-test bare-rotate-angle+cx (:xml= "<path transform='rotate(1, 2, 0)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:rotate 1 2)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-rotate-angle+cx (:xml= "<path transform='rotate(1, 2, 0)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:rotate 1 2))
        (cl-svg:draw scene (:path)))))

  (nst:def-test bare-rotate-angle+cx+cy (:xml= "<path transform='rotate(1, 2, 3)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:rotate 1 2 3)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-rotate-angle+cx+cy (:xml= "<path transform='rotate(1, 2, 3)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:rotate 1 2 3))
        (cl-svg:draw scene (:path))))))


(nst:def-test-group skew-x-tests (test-scene)
  (nst:def-test bare-skew-x-angle (:xml= "<path transform='skewX(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:skew-x 1)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-skew-x-angle (:xml= "<path transform='skewX(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:skew-x 1))
        (cl-svg:draw scene (:path))))))

(nst:def-test-group skew-y-tests (test-scene)
  (nst:def-test bare-skew-y-angle (:xml= "<path transform='skewY(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:skew-y 1)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-skew-y-angle (:xml= "<path transform='skewY(1)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:skew-y 1))
        (cl-svg:draw scene (:path))))))

(nst:def-test-group matrix-tests (test-scene)
  (nst:def-test bare-matrix (:xml= "<path transform='matrix(1,2,3,4,5,6)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform (cl-svg:matrix 1 2 3 4 5 6)
        (cl-svg:draw scene (:path)))))

  (nst:def-test nested-matrix (:xml= "<path transform='matrix(1,2,3,4,5,6)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:matrix 1 2 3 4 5 6))
        (cl-svg:draw scene (:path))))))

(nst:def-test-group composite-tests (test-scene)
  (nst:def-test composite-transform (:xml= "<path transform='translate(1, 2) scale(3, 4) translate(5, 6)'/>")
    (cl-svg:without-attribute-check
      (cl-svg:transform ((cl-svg:translate 1 2)
                         (cl-svg:scale 3 4)
                         (cl-svg:translate 5 6))
        (cl-svg:draw scene (:path))))))
