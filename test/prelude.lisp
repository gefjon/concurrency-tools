(uiop:define-package :concurrency-tools/test/prelude
  (:use :cl :fiveam :iterate)
  (:export #:with-many-threads #:concurrency-tools-tests #:+many-threads+))
(in-package :concurrency-tools/test/prelude)

(def-suite concurrency-tools-tests)

(defparameter +many-threads+ 8)

(defun call-in-many-threads (thunk &aux threads)
  (unwind-protect (iter (repeat +many-threads+)
                    (push (sb-thread:make-thread thunk)
                          threads))
    (iter (for thread in threads)
      (sb-thread:join-thread thread))))

(defmacro with-many-threads (&body body)
  `(call-in-many-threads (lambda () ,@body)))
