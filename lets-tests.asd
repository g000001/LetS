(defpackage #:lets-tests-system
  (:use #:asdf #:cl))
(in-package #:lets-tests-system)

(defsystem lets-tests
  :depends-on (:rt :lets)
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :lets-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package '#:regression-test)))
      (error "test-op failed")))

