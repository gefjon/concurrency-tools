(uiop:define-package :concurrency-tools/package
  (:nicknames :concurrency-tools)
  (:use :cl)
  (:use-reexport
   :concurrency-tools/once-cell
   :concurrency-tools/rwlock
   :concurrency-tools/shared-cell))
(in-package :concurrency-tools/package)
