;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; toplevel.lisp

(in-package #:cl-svg/test)

(nst:def-test-group toplevel-tests ()
  (nst:def-test toplevel-1.1 (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                          id='toplevel'
                                          version='1.1'
                                          width='100'
                                          height='50'/>")
    (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel :width 100 :height 50))

  (nst:def-test toplevel-1.2 (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                          id='toplevel'
                                          version='1.2'
                                          width='100'
                                          height='50'/>")
    (cl-svg:make-svg-toplevel 'cl-svg:svg-1.2-toplevel :width 100 :height 50))

  (nst:def-test toplevel-title (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                            id='toplevel'
                                            version='1.1'>
                                         <title>scene title</title>
                                       </svg>")
    (let ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
      (prog1
          scene
        (cl-svg:title scene "scene title"))))

  (nst:def-test toplevel-desc (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                           id='toplevel'
                                           version='1.1'>
                                        <desc>scene desc</desc>
                                      </svg>")
    (let ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
      (prog1
          scene
        (cl-svg:desc scene "scene desc"))))

  (nst:def-test toplevel-script (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                              id='toplevel'
                                              version='1.1'>
                                           <script type='text/ecmascript'>
                                              console.log(&#039;a &lt; b&#039;);
                                           </script>
                                        </svg>")
    (let ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
      (prog1
          scene
        (cl-svg:script scene "console.log('a < b');"))))

  (nst:def-test toplevel-script-link (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                                  id='toplevel'
                                                  version='1.1'>
                                               <script href='http://lisp.org/script.js'
                                                       type='text/ecmascript' />
                                             </svg>")
    (let ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
      (prog1
          scene
        (cl-svg:script-link scene "http://lisp.org/script.js"))))

  (nst:def-test toplevel-style (:xml= "<svg xmlns='http://www.w3.org/2000/svg'
                                            id='toplevel'
                                            version='1.1'>
                                         <style>
                                           color: yellow;
                                         </style>
                                       </svg>")
    (let ((scene (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel)))
      (prog1
          scene
        (cl-svg:style scene "color: yellow;")))))
