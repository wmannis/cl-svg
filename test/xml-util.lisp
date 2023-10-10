;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; xml-util.lisp

(in-package #:cl-svg/test)

(defun ensure-xml (xml-designator)
  ;; Unless we tell s-xml to ignore the namespaces, then the
  ;; #'PARSE-XML-STRING does not like trying to construct something
  ;; using a namespace prefix when it doesn't know the definition
  ;; for that namespace prefix.
  (let ((s-xml:*ignore-namespaces* t))
    (typecase xml-designator
      (cl-svg::svg-element
       (ignore-errors (s-xml:parse-xml-string
                       (with-output-to-string (s)
                         (cl-svg:stream-out s xml-designator))
                       :output-type :xml-struct)))
      (s-xml::xml-element
       xml-designator)
      (list
       (ensure-xml (reduce #'(lambda (a b)
                               (if (null a)
                                   b
                                   (concatenate 'string a (list #\Newline) b)))
                           xml-designator
                           :initial-value nil)))
      (string
       (ignore-errors (s-xml:parse-xml-string xml-designator :output-type :xml-struct))))))

(defun xml-attrs= (a b)
  (let ((a-attrs (s-xml:xml-element-attributes a))
        (b-attrs (s-xml:xml-element-attributes b)))
    (and (null (set-exclusive-or a-attrs b-attrs :key #'car))
         (every #'(lambda (a-attr)
                    (destructuring-bind (a-name . a-value) a-attr
                      (let ((b-attr (assoc a-name b-attrs)))
                        (when b-attr
                          (destructuring-bind (b-name . b-value) b-attr
                            (declare (ignore b-name))
                            (string= a-value b-value))))))
                a-attrs))))

(defun strip-whitespace (a)
  (string-trim '(#\space #\tab #\newline #\linefeed #\return) a))

(defun xml-nodes= (a b)
  (cond
    ((and (stringp a) (stringp b))
     (string= (strip-whitespace a) (strip-whitespace b)))

    ((and (s-xml:xml-element-p a) (s-xml:xml-element-p b))
     (and (equal (s-xml:xml-element-name a)
                 (s-xml:xml-element-name b))
          (equal (s-xml:xml-element-name a)
                 (s-xml:xml-element-name b))
          (xml-attrs= a b)
          (= (length (s-xml:xml-element-children a))
             (length (s-xml:xml-element-children b)))
          (every #'xml-nodes=
                 (s-xml:xml-element-children a)
                 (s-xml:xml-element-children b))))))

(defun xml-to-string (x)
  (s-xml:print-xml-string x :pretty t :input-type :xml-struct))
