(uiop:define-package :concurrency-tools/test/shared-cell
  (:use
   :concurrency-tools/test/prelude
   :concurrency-tools/shared-cell
   :cl
   :fiveam
   :iterate))
(in-package :concurrency-tools/test/shared-cell)

(def-suite shared-cell-tests :in concurrency-tools-tests)

(defparameter +reps-per-thread+ 1024)

(def-test shared-mutate (:suite shared-cell-tests)
  (let* ((cell (make-shared-cell 0)))
    (with-many-threads
      (iter (repeat +reps-per-thread+)
        (with-shared-cell (counter cell)
          (incf counter))))
    (with-shared-cell (counter cell)
      (is (= (* +many-threads+ +reps-per-thread+)
             counter)))))
