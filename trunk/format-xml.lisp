;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg; Lowercase: Yes -*-
;;; $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.


(in-package :cl-svg)

;;; I'm prepared to waste bytes of spaces for more or less readable XML.
;;; The output has some quirks, but it's better than nothing.
(defvar *indent-level* 0)

(defvar *indent-spacing* 2)

(defmacro with-indentation (&body body)
  `(let ((*indent-level* (+ *indent-level* *indent-spacing*)))
     ,@body))


(defgeneric pp-xml-attr (stream keyword &optional colon-p at-p)
  (:documentation "This turns a keyword slot of a p-list into something XML
will recognize, in particular making the case correct.  It is intended
for ~/pp-xml-attr/ use in a FORMAT string."))

;;; Some of these keyword name transformations could be done
;;; programatically, but there are enough oddities that this wouldn't
;;; be at all reliable.
(defun xmlify-keyword (kw)
  "Convert a ':view-box' lisp-style name into XMLish 'viewBox'."
  (let ((translation
         (case kw
           (:view-box "viewBox")
           (:xlink-href "xlink:href")
           (:gradient-units "gradientUnits")
           (:gradient-transform "gradientTransform")
           (:spread-method "spreadMethod")
           (:zoom-and-pan "zoomAndPan")
           (:preserve-aspect-ration "preserveAspectRatio")
           (:pattern-units "patternUnits")
           (:pattern-content-units "patternContentUnits")
           (:pattern-transform "patternTransform")
           (:marker-units "markerUnits")
           (:marker-width "markerWidth")
           (:marker-height "markerHeight")
           (:mask-units "maskUnits")
           (:mask-content-units "maskContentUnits")
           ((:ref-x :refx) "refX")
           ((:ref-y :refy) "refY")
           (:text-length "textLength")
           (:start-offset "startOffset")
           (:glyph-ref "glyphRef")
           (:text-length "textLength")
           (:length-adjust "lengthAdjust"))))
    (if translation
        translation
        (string-downcase (symbol-name kw)))))

(defmethod pp-xml-attr ((s stream) (kw symbol) &optional colon-p at-p)
  (declare (ignore colon-p at-p))
  (format s "~A" (xmlify-keyword kw)))

(defmethod pp-xml-attr ((s stream) (kw string) &optional colon-p at-p)
  (declare (ignore colon-p at-p))
  (format s "~A" kw))

(defun element->xml (stream element properties)
  ;; FORMAT ~/ functions not in CL-USER have to state their package.
  (format stream "~v,0T<~A ~@<~{~/cl-svg:pp-xml-attr/=\"~A\"~^ ~}~:@>/>~&"
                 *indent-level* element properties))

(defun string->xml (stream string)
  (format stream "~v,0T~@<~A~:@>~&" *indent-level* string))

(defun begin-group->xml (stream element properties)
  (format stream "~v,0T<~A~@<~{ ~/cl-svg:pp-xml-attr/=\"~A\"~}~:@>>~&"
          *indent-level* element properties))

(defun end-group->xml (stream element)
  (format stream "~v,0T</~A>~&" *indent-level* element))

(defmacro with-xml-group-element ((stream element properties) &body body)
  (let ((s (gensym "stream"))
        (e (gensym "element")))
    `(let ((,s ,stream)
           (,e ,element))
       (begin-group->xml ,s ,e ,properties)
       (with-indentation 
         ,@body)
       (end-group->xml ,s ,e))))

;;; FIX FIX FIX (make points sub-lists)
(defun format-points (stream points)
  (let ((*print-pretty* t))
    (format
     stream
     ;; The ~<... with closing ~:@> does paragraph filling.
     "~@<~{~/cl-svg:format-truncate/,~/cl-svg:format-truncate/~^ ~}~:@>"
     points)))

;;; format-xml.lisp ends here
