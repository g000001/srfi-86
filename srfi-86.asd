;;;; srfi-86.asd

(cl:in-package :asdf)

(defsystem :srfi-86
  :serial t
  :depends-on (:srfi-23 :mbe)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-86")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-86))))
  (load-system :srfi-86)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-86-internal :srfi-86))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

