(uiop:define-package :concurrency-tools/test/once-cell
  (:use
   :concurrency-tools/test/prelude
   :concurrency-tools/once-cell
   :cl
   :fiveam
   :iterate)
  (:export #:once-cell-tests))
(in-package :concurrency-tools/test/once-cell)

(def-suite once-cell-tests :in concurrency-tools-tests)

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
