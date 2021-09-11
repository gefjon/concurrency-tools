(uiop:define-package :concurrency-tools/rwlock
  (:use :cl :iterate)
  (:import-from :alexandria #:once-only)
  (:export #:rwlock #:make-rwlock #:with-reading #:with-writing))
(in-package :concurrency-tools/rwlock)

(defstruct (rwlock (:constructor make-rwlock (value))
                   (:conc-name %rwlock-))
  (value (error "supply an initial value to make-rwlock"))
  (lock (sb-thread:make-mutex)
   :type sb-thread:mutex
   :read-only t)
  (write-queue (sb-thread:make-waitqueue)
   :type sb-thread:waitqueue
   :read-only t)
  (readers 0
   :type sb-ext:word))

(declaim (ftype (function (rwlock (function () (values &rest t))) (values &rest t))
                call-with-writing))
(defun call-with-writing (rwlock thunk)
  (with-accessors ((lock %rwlock-lock)
                   (queue %rwlock-write-queue)
                   (readers %rwlock-readers))
      rwlock
    (sb-thread:with-mutex (lock)
      (iter (until (zerop readers))
        (sb-thread:condition-wait queue lock))
      (funcall thunk))))

(defmacro with-writing ((value rwlock) &body body)
  (once-only (rwlock)
    `(call-with-writing ,rwlock
                        (lambda ()
                          (with-accessors ((,value %rwlock-value)) ,rwlock
                            ,@body)))))

(declaim (ftype (function (rwlock (function () (values &rest t))) (values &rest t))
                call-with-reading))
(defun call-with-reading (rwlock thunk)
  (with-accessors ((lock %rwlock-lock)
                   (queue %rwlock-write-queue)
                   (readers %rwlock-readers))
      rwlock
    (unwind-protect (prog2 (sb-thread:with-mutex (lock)
                             (incf readers))
                        (funcall thunk))
      (sb-thread:with-mutex (lock)
        (decf readers)
        (when (zerop readers)
          (sb-thread:condition-notify queue))))))

(defmacro with-reading ((value rwlock) &body body)
  (once-only (rwlock)
    `(call-with-reading ,rwlock
                        (lambda ()
                          (with-accessors ((,value %rwlock-value)) ,rwlock
                            ,@body)))))
