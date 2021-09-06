(uiop:define-package :concurrency-tools/shared-cell
  (:use :cl)
  (:import-from :alexandria #:once-only)
  (:export #:shared-cell #:make-shared-cell #:with-shared-cell))
(in-package :concurrency-tools/shared-cell)

(defstruct (shared-cell (:constructor make-shared-cell (value))
                        (:conc-name %shared-cell-))
  (value (error "supply an initial value to make-shared-cell"))
  (lock (sb-thread:make-mutex)
   :type sb-thread:mutex
   :read-only t))

(defmacro with-shared-cell ((accessor cell) &body body)
  (once-only (cell)
    `(sb-thread:with-mutex ((%shared-cell-lock ,cell))
       (with-accessors ((,accessor %shared-cell-value)) ,cell
         ,@body))))
