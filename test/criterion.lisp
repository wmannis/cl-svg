;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; criterion.lisp

(in-package #:cl-svg/test)

(defun ensure-xml (xml-designator)
  (typecase xml-designator
    (cl-svg::svg-element
     (ignore-errors (xmls:parse
                     (with-output-to-string (s)
                       (cl-svg:stream-out s xml-designator)))))
    (xmls:node
     xml-designator)
    (string
     (ignore-errors (xmls:parse xml-designator)))))

(defun xml-attrs= (a b)
  (and (= (length (xmls:node-attrs a))
          (length (xmls:node-attrs b)))
       (let ((b-attrs (xmls:node-attrs b)))
         (every #'(lambda (a-attr)
                    (destructuring-bind (a-name a-value &rest a-rest) a-attr
                      (declare (ignore a-rest))
                      (let ((b-attr (assoc a-name b-attrs :test #'string=)))
                        (when b-attr
                          (destructuring-bind (b-name b-value &rest b-rest) b-attr
                            (declare (ignore b-name b-rest))
                            (string= a-value b-value))))))
                (xmls:node-attrs a)))))

(defun strip-whitespace (a)
  (string-trim '(#\space #\tab #\newline #\linefeed #\return) a))

(defun xml-nodes= (a b)
  (cond
    ((and (stringp a) (stringp b))
     (string= (strip-whitespace a) (strip-whitespace b)))

    ((and (xmls:node-p a) (xmls:node-p b))
     (and (equal (xmls:node-ns a)
                 (xmls:node-ns b))
          (equal (xmls:node-name a)
                 (xmls:node-name b))
          (xml-attrs= a b)
          (= (length (xmls:node-children a)) (length (xmls:node-children b)))
          (every #'xml-nodes= (xmls:node-children a) (xmls:node-children b))))))

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
       (nst:make-failure-report :format "~A does not match the expected ~A"
                                :args (list (xmls:toxml got-xml)
                                            (xmls:toxml expected-xml)))))))
