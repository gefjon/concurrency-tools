(defsystem "concurrency-tools"
  :class :package-inferred-system
  :version "0.0.1"
  :depends-on ("concurrency-tools/package")
  :author "Phoebe Goldman <phoebe@goldman-tribe.org>"
  :in-order-to ((test-op (test-op "concurrency-tools/test"))))

(defsystem "concurrency-tools/test"
  :defsystem-depends-on ((:version "fiveam-asdf" "3.0.1"))
  :class :package-inferred-fiveam-tester-system
  :depends-on ("concurrency-tools/test/once-cell")
  :test-names ((#:once-cell-tests . :concurrency-tools/test/once-cell)))
