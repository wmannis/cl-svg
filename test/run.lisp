;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; run.lisp

(in-package #:cl-svg/test)

(defun run-all-tests (&optional
                        (*print-pretty* t)
                        (nst:*debug-on-error* t)
                        (nst:*debug-on-fail* nil))
  (nst:nst-cmd :run-package #.*package*))
