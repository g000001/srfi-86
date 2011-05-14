;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-86
  (:export :mu
           :nu
           :alet
           :alet*
           ;;
           :opt
           :cat
           :key
           :rec))

(defpackage :srfi-86-internal
  (:use :srfi-86 :mbe :cl :fiveam)
  (:shadowing-import-from :srfi-23
                          :error)
  (:shadow :t))

