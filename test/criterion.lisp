;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; criterion.lisp

(in-package #:cl-svg/test)

(nst:def-criterion (:xml= (expected) (got))
  (let ((expected-xml (ensure-xml expected))
        (got-xml (ensure-xml got)))
    (cond
      ((null expected-xml)
       (nst:make-failure-report :format "The expected value |~A| was not valid XML"
                                :args (list expected)))
      ((null got-xml)
       (nst:make-failure-report :format "The value under test |~A| was not valid XML"
                                :args (list got)))
      ((xml-nodes= expected-xml got-xml)
       (nst:make-success-report))
      (t
       (nst:make-failure-report :format "~A expected, but got ~A"
                                :args (list (xml-to-string expected-xml)
                                            (xml-to-string got-xml)))))))

(nst:def-criterion (:path= (expected) (got))
  (let ((got (remove #\newline got)))
    (cond
      ((string= expected got)
       (nst:make-success-report))
      (t
       (nst:make-failure-report :format "~A expected, but got ~A"
                                :args (list expected
                                            got))))))
