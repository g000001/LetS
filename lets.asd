;;;; /tmp/lets/lets.asd

(in-package :asdf)

(defsystem #:lets
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "lets")))

(defmethod perform ((o test-op) (c (eql (find-system :lets))))
  (load-system :lets)
  (test-system :lets-tests :force t))
