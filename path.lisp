;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg; Lowercase: Yes -*-
;;; Version: $Id$
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

;;; A more verbose, but easier to read, description language for SVG paths.

(in-package :cl-svg)

(defvar *previous-path-instruction* "")

(defvar *insert-instruction* t)

(defmacro with-path-instruction (instruction &body body)
  `(prog1
       (let ((*insert-instruction*
              (not (equal *previous-path-instruction* ,instruction))))
         ,@body)
     (setf *previous-path-instruction* ,instruction)))

(defun format-instruction (instruction args)
  (with-path-instruction instruction
    (if *insert-instruction*
        (format nil "~A~{~A~^ ~}" instruction args)
        (format nil " ~{~A~^ ~}" args))))

(defun assert-arity (instruction n args)
  (let ((len (length args)))
    (unless (= n len)
      (error "~A requires ~A not ~A arguments: ~A" instruction n len args))))

(defun path-command-maker (name instruction arity)
  `(,name (&rest args)
     (assert-arity ',name ,arity args)
     (format-instruction ,instruction args)))

(defmacro path (&rest cmds)
  (let ((labels-body
         (list (path-command-maker 'move-to "M" 2)
               (path-command-maker 'move-to-r "m" 2)
               (path-command-maker 'line-to "L" 2)
               (path-command-maker 'line-to-r "l" 2)
               (path-command-maker 'horizontal-to "H" 1)
               (path-command-maker 'horizontal-to-r "h" 1)
               (path-command-maker 'vertical-to "V" 1)
               (path-command-maker 'vertical-to-r "v" 1)
               (path-command-maker 'curve-to "C" 6)
               (path-command-maker 'curve-to-r "c" 6)
               (path-command-maker 'smooth-curve-to "S" 4)
               (path-command-maker 'smooth-curve-to-r "s" 4)
               (path-command-maker 'quadratic-curve-to "Q" 4)
               (path-command-maker 'quadratic-curve-to-r "q" 4)
               (path-command-maker 'smooth-quadratic-curve-to "T" 2)
               (path-command-maker 'smooth-quadratic-curve-to-r "t" 2)
               (path-command-maker 'arc-to "A" 7)
               (path-command-maker 'arc-to-r "a" 7))))
    `(labels ,labels-body
       (let ((*previous-path-instruction* ""))
       ;(format nil "~A" (list ,@cmds)))))
       (apply #'concatenate 'string (list ,@cmds))))))

(defmacro 
;;; path.lisp ends here

