;;; -*- Mode: LISP; Syntax: Common-lisp; Package: cl-svg/test; Lowercase: Yes -*-
;;;; run.lisp

(in-package #:cl-svg/test)

(defun run-all-tests (&key
                        (print-pretty *print-pretty*)
                        (debug-on-error nst:*debug-on-error*)
                        (debug-on-fail nst:*debug-on-fail*))
  (let ((*print-pretty* print-pretty)
        (nst:*debug-on-error* debug-on-error)
        (nst:*debug-on-fail* debug-on-fail))
    #-abcl (nst:nst-cmd :run-package #.*package*)
    #+abcl (unwind-protect
                (nst-control-api:run-package #.*package*)
             (nst-control-api:report-package #.*package*))))
