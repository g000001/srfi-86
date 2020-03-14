;;;; srfi-86.asd

(cl:in-package asdf)


(defsystem :srfi-86
  :version "20200315"
  :description "SRFI 86 for CL: MU and NU simulating VALUES & CALL-WITH-VALUES, and their related LET-syntax"
  :long-description "SRFI 86 for CL: MU and NU simulating VALUES & CALL-WITH-VALUES, and their related LET-syntax
https://srfi.schemers.org/srfi-86"
  :author "Joo ChurlSoo"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (srfi-23 mbe fiveam)
  :components ((:file "package")
               (:file "utils")
               (:file "srfi-86")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-86))))
  (let ((name "https://github.com/g000001/srfi-86")
        (nickname :srfi-86))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-86))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-86#internals")))
    (eval
     (read-from-string
        "
      (or (let ((result (run 'srfi-86)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
