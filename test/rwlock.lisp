(uiop:define-package :concurrency-tools/test/rwlock
  (:use
   :concurrency-tools/test/prelude
   :concurrency-tools/rwlock
   :cl
   :fiveam
   :iterate)
  (:export #:rwlock-tests))
(in-package :concurrency-tools/test/rwlock)

(def-suite rwlock-tests :in concurrency-tools-tests)

(def-test simple-rwlock (:suite rwlock-tests)
  (let* ((cell (make-rwlock 0))
         (results (list nil)))
    (labels ((add-result (res)
               (sb-ext:atomic-push res
                                   (car results)))
             (all-equal (list)
               (apply #'= list))
             (increment ()
               (with-writing (value cell)
                 (incf value)))
             (read-many ()
               (with-reading (value cell)
                 (iter (repeat 128)
                   (collect value)))))
      (with-many-threads
        (iter (repeat 1024)
          (increment)
          (add-result (all-equal (read-many))))))
    (is (every #'identity (car results)))))
