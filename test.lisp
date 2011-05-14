(in-package :srfi-86-internal)

(def-suite srfi-86)
(in-suite srfi-86)

(test alet
  (is (equal (alet (a (mu 1 2))
                   (list a))
             '((1 2))))
  (is (equal (alet (a (mu 1 2) ((b c) (mu 3 4)))
                   (list a b c))
             '((1 2) 3 4) ))
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
             '((1 2 3 4 5 (6)) "1st2nd3rd4th"))))
