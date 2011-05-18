(in-package :srfi-86-internal)

(defmacro defun-inline (name (&rest args) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@args)
       ,@body)))

#|(defun-inline map (function list &rest more-list)
  (apply #'mapcar function list more-list))|#

(defun-inline null? (obj) (null obj))

(defun-inline eq? (x y) (eq x y))

(defun-inline pair? (obj) (consp obj))

(defun-inline list? (obj) (listp obj))

(defun-inline zero? (x) (zerop x))

(defun-inline equal? (x y)
  (equal x y))

(defun-inline set-car! (list obj)
  (rplaca list obj))

(defun-inline set-cdr! (cons x)
  (rplacd cons x))

(defun-inline memq (x list)
  (cl:member x list :test #'eq))

(defmacro begin (&body body)
  `(progn ,@body))

(defun-inline display (obj)
  (princ obj))

(defun-inline newline ()
  (terpri))

(defmacro set! (var val)
  `(setq ,var ,val))

(defun-inline string-length (string)
  (length string))

;; from sbcl
(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (= 2(length x))
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

(defmacro let (&rest args)
  (etypecase (car args)
    (list `(cl:let ,@args))
    (symbol `(#+sbcl sb-int:named-let
              #-sbcl named-let
                     ,@args))))

(defmacro receive ((&rest args) vals &body body)
  `(multiple-value-bind (,@args) ,vals
     ,@body))

(defmacro define (name&args &body body)
  (etypecase name&args
    (list (destructuring-bind (name &rest args)
                              name&args
            `(defun ,name (,@args)
               ,@body)))
    (symbol `(setf (symbol-function ',name&args) (progn ,@body)))))


(defmacro call-with-values (producer consumer)
  `(multiple-value-call ,consumer (funcall ,producer)))

(defmacro let*-mbe ((&rest vars) (&rest vals) &body body)
  `(let* (,@(mapcar #'list vars vals))
     ,@body))


;; 手抜き
#|(defmacro letrec ((&rest vars-and-fctns) &body body)
  `(labels (,@(mapcar (lambda (x)
                        (destructuring-bind (name lambda) x
                          (destructuring-bind (lambda args . body)
                                              (if (eq 'lambda*
                                                      (car lambda))
                                                  (sb-cltl2::macroexpand-all
                                                   lambda)
                                                  lambda)
                            (declare (ignore lambda))
                            `(,name ,args ,@body))))
                vars-and-fctns))
     ,@body))|#

;; adopted from kawa 1.9 (mit licence)
(define-syntax letrec
  (syntax-rules ()
    ((letrec bindings . body)
     (%letrec1 () bindings . body))))

(define-syntax %letrec1
  (syntax-rules (|::|)
    ((%letrec1 done ((x |::| type init) . bindings) . body)
     (%letrec1 ((x |::| type '#:undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done ((x init) . bindings) . body)
     (%letrec1 ((x '#:undefined) . done) bindings (set! x init) . body))
    ((%letrec1 done () . body)
     (srfi-86-internal::let done . body))))