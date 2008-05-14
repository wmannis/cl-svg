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

(defvar *previous-path-instruction* ""
  "Keeps track of the previously called path instruction - SVG will assume
a repetition of the this on its own if it keeps seeing points.")

(defvar *insert-instruction-p* t
  "Works with *PREVIOUS-PATH-INSTRUCTION* to decide if a path command
needs to be expressed.")

(defmacro with-path-instruction (instruction &body body)
  `(prog1
       (let ((*insert-instruction-p*
              (not (equal *previous-path-instruction* ,instruction))))
         ,@body)
     (setf *previous-path-instruction* ,instruction)))

(defun format-instruction (instruction args)
  (with-path-instruction instruction
    (if *insert-instruction-p*
        (format nil "~A~{~A~^ ~}" instruction args)
        (format nil " ~{~A~^ ~}" args))))

(defun assert-arity (instruction n args)
  (let ((len (length args)))
    (unless (= n len)
      (error "~A requires ~A not ~A arguments: ~A" instruction n len args))))

(defmacro define-path-instruction (name instruction arity)
  `(defun ,name (&rest args)
     (assert-arity ',name ,arity args)
     (format-instruction ,instruction args)))
    
(define-path-instruction move-to "M" 2)
(define-path-instruction move-to-r "m" 2)
(define-path-instruction line-to "L" 2)
(define-path-instruction line-to-r "l" 2)
(define-path-instruction horizontal-to "H" 1)
(define-path-instruction horizontal-to-r "h" 1)
(define-path-instruction vertical-to "V" 1)
(define-path-instruction vertical-to-r "v" 1)
(define-path-instruction curve-to "C" 6)
(define-path-instruction curve-to-r "c" 6)
(define-path-instruction smooth-curve-to "S" 4)
(define-path-instruction smooth-curve-to-r "s" 4)
(define-path-instruction quadratic-curve-to "Q" 4)
(define-path-instruction quadratic-curve-to-r "q" 4)
(define-path-instruction smooth-quadratic-curve-to "T" 2)
(define-path-instruction smooth-quadratic-curve-to-r "t" 2)
(define-path-instruction arc-to "A" 7)
(define-path-instruction arc-to-r "a" 7)

(defun make-path ()
  (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))

(defun error-unless-string (s form)
  (if (not (typep s 'string))
      (error "you must use only path commands in ~A: ~A" form s)
      s))

(defmacro with-path (path &body cmds)
  (let ((s (gensym "stream"))
        (n (gensym)))
    `(let ((*previous-path-instruction* ""))
       (flet ((assert-string (s)
                (error-unless-string s 'with-path)))
         (with-output-to-string (,s ,path)
           ;; Do trivial breaking up of the path data - SVG does not have
           ;; to accept indefinitely long lines of data.
           (let ((,n 0))
             (dolist (inst (mapcar #'assert-string (list ,@cmds)))
               (format ,s "~@{~A~}" inst)
               (incf ,n)
               (when (= (mod ,n 30) 0)
                 (format ,s "~&")))))))))

(defmacro path (&body cmds)
  (let ((path (gensym "path")))
    `(let ((,path (make-path)))
       (with-path ,path
         ,@cmds)
       ,path)))

;;; path.lisp ends here

