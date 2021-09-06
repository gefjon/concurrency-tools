(uiop:define-package :concurrency-tools/test/once-cell
  (:use :concurrency-tools/once-cell :cl :fiveam :iterate))
(in-package :concurrency-tools/test/once-cell)

(def-suite once-cell-tests)

(defparameter +many-threads+ 8)

(defun call-in-many-threads (thunk &aux threads)
  (unwind-protect (iter (repeat +many-threads+)
                    (push (sb-thread:make-thread thunk)
                          threads))
    (iter (for thread in threads)
      (sb-thread:join-thread thread))))

(defmacro with-many-threads (&body body)
  `(call-in-many-threads (lambda () ,@body)))

(def-test read-once (:suite once-cell-tests)
  (let* ((count (list 0))
         (found-values (list nil))
         (cell (once-cell
                 (sb-ext:atomic-incf (car count) 1)
                 :foo)))
    (with-many-threads
        (let* ((val (once-cell-get cell)))
          (sb-ext:atomic-push val (car found-values))))
    (dolist (val (car found-values))
      (is (eq val :foo)))
    (is (= 1 (car count)))))
