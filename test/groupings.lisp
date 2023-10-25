;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; groupings.lisp

(in-package #:cl-svg/test)

(nst:def-test-group group-tests (test-scene)
  (nst:def-test make-group (:xml= "<g/>")
    (cl-svg:make-group scene ()
      t))

  (nst:def-test group-of-one-element (:xml= "<g>
                                               <circle cx='1' cy='2' r='3'/>
                                             </g>")
    (cl-svg:make-group scene ()
      (cl-svg:draw* (:circle :cx 1 :cy 2 :r 3))))

  (nst:def-test group-with-style (:xml= "<g fill='red'/>")
    (cl-svg:make-group scene (:fill "red")))

  (nst:def-test group-with-styled-element (:xml= "<g>
                                                    <circle cx='1' cy='2' r='3' stroke='red'/>
                                                  </g>")
    (cl-svg:make-group scene ()
      (cl-svg:draw* (:circle :cx 1 :cy 2 :r 3)
                    :stroke "red")))

  (nst:def-test transformed-group (:xml= "<g transform='translate(1, 2)'/>")
    (cl-svg:transform ((cl-svg:translate 1 2))
      (cl-svg:make-group scene ())))

  (nst:def-test draw-in-captured-group (:xml= "<g>
                                                 <circle cx='1' cy='2' r='3'/>
                                               </g>")
    (let ((group (cl-svg:make-group scene ())))
      (prog1
          group
        (cl-svg:draw group (:circle :cx 1 :cy 2 :r 3))))))

(nst:def-test-group foreign-object-tests (test-scene)
  (nst:def-test foreign-object (:xml= "<foreignObject x='1' y='2' width='3' height='4'/>")
    (cl-svg:make-foreign-object scene (:x 1 :y 2 :width 3 :height 4)))

  (nst:def-test foreign-object-with-contents (:xml= "<foreignObject x='1' y='2' width='3' height='4'>
                                                       <p>some paragraph text here</p>
                                                     </foreignObject>")
    (let ((fo (cl-svg:make-foreign-object scene (:x 1 :y 2 :width 3 :height 4))))
      (prog1
          fo
        (cl-svg:add-element fo "<p>some paragraph text here</p>"))))
  )

(nst:def-test-group link-tests (test-scene)
  (nst:def-test link (:xml= "<a xlink\:href='https://lisp.org/'/>")
    (cl-svg:link scene (:xlink-href "https://lisp.org/")))

  (nst:def-test link-with-attributes (:xml= "<a xlink\:href='https://lisp.org/' fill='green'/>")
    (cl-svg:link scene (:xlink-href "https://lisp.org/"
                        :fill "green")))

  (nst:def-test link-with-objects (:xml= "<a xlink\:href='https://lisp.org/'>
                                            <circle cx='3' cy='4' r='5'/>
                                          </a>")
    (cl-svg:link scene (:xlink-href "https://lisp.org/")
      (cl-svg:draw* (:circle :cx 3 :cy 4 :r 5))))

  (nst:def-test draw-in-captured-link (:xml=  "<a xlink\:href='https://lisp.org/'>
                                                 <circle cx='3' cy='4' r='5'/>
                                               </a>")
    (let ((link (cl-svg:link scene (:xlink-href "https://lisp.org/"))))
      (prog1
          link
        (cl-svg:draw link (:circle :cx 3 :cy 4 :r 5))))))

(nst:def-test-group text-tests (test-scene)
  (nst:def-test text (:xml= "<text x='1' y='2'/>")
    (cl-svg:text scene (:x 1 :y 2)))

  (nst:def-test text-with-one-string (:xml= "<text x='1' y='2'>some text here</text>")
    (cl-svg:text scene (:x 1 :y 2)
      "some text here"))

  (nst:def-test text-with-two-strings (:xml= ("<text x='1' y='2'>"
                                              "  aaa"
                                              "  bbb"
                                              "</text>"))
    (cl-svg:text scene (:x 1 :y 2)
      "aaa"
      "bbb"))

  (nst:def-test text-with-tspan (:xml= "<text x='1' y='2'>
                                          aaa
                                          <tspan color='green'>
                                            bbb
                                          </tspan>
                                        </text>")
    (cl-svg:text scene (:x 1 :y 2)
      "aaa"
      (cl-svg:tspan (:color "green") "bbb"))))
