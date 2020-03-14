;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-86"
  (:use)
  (:export mu
           nu
           alet
           alet*
           ;;
           opt
           cat
           key
           rec))


(defpackage "https://github.com/g000001/srfi-86#internals"
  (:use
   "https://github.com/g000001/srfi-86" 
   mbe
   cl
   fiveam)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-23"
   error)
  (:shadow t loop let))


;;; *EOF*
