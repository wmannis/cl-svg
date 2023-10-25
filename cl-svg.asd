;;; -*- mode: lisp; syntax: common-lisp; package: cl-svg; encoding: utf-8 -*-
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


(in-package :asdf)

(defsystem :cl-svg
  :name "CL-SVG"
  :author "William S. Annis <wm.annis@gmail.com>"
  :version "0.4.0"
  :maintainer "William S. Annis <wm.annis@gmail.com>"
  :licence "MIT License"
  :description "Produce Scalable Vector Graphics (SVG) files"
  :in-order-to ((asdf:test-op (asdf:test-op :cl-svg/test)))
  :components ((:module "src"
                :components ((:file "package")
                             (:file "format-xml")
                             (:file "path")
                             (:file "svg"))
                :serial t)))

(defsystem :cl-svg/test
  :name "CL-SVG/TEST"
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.4"
  :maintainer "Patrick Stein <pat@nklein.com>"
  :licence "MIT License"
  :description "Tests for the CL-SVG package"
  :depends-on ((:version #:cl-svg "0.4") #:nst #:s-xml)
  :perform (asdf:test-op (o c)
                         (uiop:symbol-call :cl-svg/test :run-all-tests))

  :components ((:module "test"
                :components ((:file "package")
                             (:file "xml-util")
                             (:file "criterion")
                             (:file "fixture")
                             (:file "toplevel")
                             (:file "shapes")
                             (:file "transforms")
                             (:file "paths")
                             (:file "groupings")
                             (:file "precision")
                             (:file "run"))
                :serial t)))
