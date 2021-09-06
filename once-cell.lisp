(uiop:define-package :concurrency-tools/once-cell
  (:use :cl)
  (:export #:once-cell #:make-once-cell #:once-cell-get #:once))
(in-package :concurrency-tools/once-cell)

(deftype thunk ()
  '(function () (values t &optional)))

(defclass once-cell ()
  (%value
   (%thunk :type thunk
           :initarg :thunk)
   (%lock :type sb-thread:mutex
          :initform (sb-thread:make-mutex)
          :reader once-cell-lock)
   (%condvar :type sb-thread:waitqueue
             :initform (sb-thread:make-waitqueue)
             :reader once-cell-condvar)))

(declaim (ftype (function (once-cell) (values t &optional))
                once-cell-get))
(defun once-cell-get (once-cell)
  (with-accessors ((lock once-cell-lock)
                   (condvar once-cell-condvar))
      once-cell
    (sb-thread:with-mutex (lock)
      (labels ((finishedp ()
                 (slot-boundp once-cell '%value))
               (startedp ()
                 (not (slot-boundp once-cell '%thunk)))
               (steal-thunk ()
                 (prog1 (slot-value once-cell '%thunk)
                   (slot-makunbound once-cell '%thunk)))
               (make-finished (value)
                 (setf (slot-value once-cell '%value)
                       value)
                 (sb-thread:condition-broadcast condvar)
                 value)
               (compute-value ()
                 (let* ((thunk (steal-thunk)))
                   (make-finished (funcall (the thunk thunk)))))
               (get-value () (slot-value once-cell '%value))
               (wait ()
                 (sb-thread:condition-wait condvar lock)
                 (if (finishedp)
                     (get-value)
                     (wait))))
        (cond ((finishedp) (get-value))
              ((startedp) (wait))
              (t (compute-value)))))))

(defun make-once-cell (thunk)
  (make-instance 'once-cell :thunk thunk))

(defmacro once-cell (&body body)
  `(make-once-cell (lambda () ,@body)))

(defmacro once (&body body)
  `(once-cell-get (load-time-value (once-cell ,@body))))
