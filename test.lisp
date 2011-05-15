(in-package :srfi-86-internal)

(def-suite srfi-86)
(in-suite srfi-86)

(defun macroexpand-equal (macro form &optional env)
  (equal (funcall (macro-function (car macro)) macro env)
         form))

(test alet
  (is (equal (alet (a (mu 1 2))
               (list a))
             '((1 2))))
  ;; 1
  (is (equal (alet (a (mu 1 2) ((b c) (mu 3 4)))
               (list a b c))
             '((1 2) 3 4) ))
  ;; 2
  (is (equal (let ((*standard-output*
                    (make-string-output-stream :element-type 'character)))
               (unwind-protect
                   (progn
                     (alet ((a (progn (princ "1st") 1))
                            (b c (mu (progn (princ "2nd") 2) 3))
                            (d (progn (princ "3rd") 4))
                            ((e . f) (mu (progn (princ "4th") 5) 6)))
                       (close *standard-output*)
                       (list (list a b c d e f)
                             (get-output-stream-string *standard-output*))))))
             '((1 2 3 4 5 (6)) "1st2nd3rd4th")))
  ;; 3
  (is (equal (alet* (((a b) (mu 1 2))
                     ((c d e) a (+ a b c) (+ a b c d))
                     ((f . g) (mu 5 6 7))
                     ((h i j . k) e 9 10 h i j))
                    (list a b c d e f g h i j k))
             '(1 2 1 4 8 5 (6 7) 8 9 10 (8 9 10))))
  ;;
  (is (equal (let ((*standard-output*
                    (make-string-output-stream :element-type 'character)))
               (unwind-protect
                   (progn
                     (alet ((exit)
                            (a (begin (display "1st") 1))
                            (b c (mu (begin (display "2nd") 2)
                                     (begin (display "3rd") 3))))
                       (display (list a b c))
                       (exit (list 10
                                   (get-output-stream-string *standard-output*)))
                       (display "end")))))
             '(10 "1st2nd3rd(1 2 3)")))
  (is (equal (let ((*standard-output*
                    (make-string-output-stream :element-type 'character)))

               (list

                (unwind-protect
                    (progn
                      (alet ((and (a (begin (display "1st") 1))
                                  (b (begin (display "2nd") 2))
                                  (c (begin (display "false") nil))
                                  (d (begin (display "3nd") 3))))
                        (list a b c d))))

                (get-output-stream-string *standard-output*)))

             '(nil "1st2ndfalse")))
  (is (string= ((lambda (str &rest rest)
                  (alet ((cat rest
                              (start 0
                                     (and (listp start) (= 2 (length start))
                                          (eq 'start (car start)))
                                     (cadr start))	; true
                              (end (string-length str)
                                   (and (listp end)
                                        (= 2 (length end))
                                        (eq 'end (car end)))
                                   (cadr end))))	; true
                    (subseq str start end))) "abcdefg" '(end 6) '(start 1))
               "bcdef"))
  (let ((rest-list '(a 10 cc 30 40 b 20)))
    (is (equal (alet ((key rest-list
                           (a 1) (b 2) ((c 'cc) 3) . d))
                 (list a b c d))
               '(10 2 30 (40 B 20))))
    (is (equal (alet ((key rest-list
                           (a 1)
                           (b 2)
                           ((c 'cc) 3)
                           :false . d))
                 (list a b c d))
               '(10 2 30 (40 B 20))))
    #|(is (equal (alet ((key rest-list
    (a 1)
    (b 2)
    ((c 'cc) 3)
    :true . d))
    (list a b c d))
    '(10 2 30 (40))))|#)
  (let ((rest (list 'a 10 'd 40 "c" 30 50 'b 20)))
    (is (equal (alet ((key rest1 (a 1) (b 2) ((c "c") 3) . d))
                 (list a b c d))
               '(10 2 30 (D 40 50 B 20))))
    (is (equal (alet ((key rest (a 1) (b 2) ((c "c") 3) :false . d))
                 (list a b c d))
               '(10 2 3 (d 40 "c" 30 50 b 20)) ))
    #|(is (equal (alet ((key rest (a 1) (b 2) ((c "c") 3) :true . d))
    (list a b c d))
    '(10 20 30 (d 40 50))))|#)
  ;; 11
  (is (equal
       (alet ((a b (mu 1 2))
              (values c d (values 3 4))	;This is different from SRFI 71.
              ((e f) (mu 5 6))
              ((values g h) (values 7 8))
              ((i j . k) (nu 9 '(10 11 12)))
              ((values l m . n) (apply #'values 13 '(14 15 16)))
              o (mu 17 18)
              ((values . p) (values 19 20)))
         (list a b c d e f g h i j k l m n o p))
       '(1 2 3 4 5 6 7 8 9 10 (11 12) 13 14 (15 16) (17 18) (19 20))))
  ;; 12
  (is (equal
       (let (a b)
         (alet ((a 1)
                (() (setq a 10) (setq b 100))
                (b a))
           (list a b)))
       '(1 10))))

