(uiop:define-package :concurrency-tools/test/package
  (:nicknames :concurrency-tools/test)
  (:use-reexport
   :concurrency-tools/test/prelude
   :concurrency-tools/test/once-cell
   :concurrency-tools/test/shared-cell
   :concurrency-tools/test/rwlock)
  (:use :cl))
(in-package :concurrency-tools/test/package)
