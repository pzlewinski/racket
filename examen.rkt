;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname examen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct person (name ec kids))
;; Person is (make-person String String (listof Person))
;; interp. A descendant family tree, each person has:
;;           a name of the form "Harry"
;;           an eye color of the form "green"
;;           kids is a list of the children
;;  NOTE: we are not writing the (listof Person) type out in its detailed
;;        ListOfPerson is one of... form, but there are two types here: Person and
;;        (listof Person). They are involved in a mutual reference relationship and
;;        this is reflected in the templates below.
(define DFT1 (make-person "Lily" "brown" empty))
(define DFT2 (make-person "Harry"
                          "green"
                          (list (make-person "James" "brown" empty)
                                (make-person "Albus" "green" empty)
                                DFT1)))
(define DFT3 (make-person "Lily"
                          "green"
                          (list DFT2)))
#;
(define (fn-for-dft p)
  (local [(define (fn-for-person p)
            (... (person-name p)
                 (person-ec p)
                 (fn-for-lop (person-kids p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-person (first lop))
                        (fn-for-lop (rest lop)))]))]
    (fn-for-person p)))

;; String Person -> String
;; produce name of everyone in tree with given eye color
(check-expect (matching-ec "blue" DFT1) empty)
(check-expect (matching-ec "orange" DFT3) empty)
(check-expect (matching-ec "green" DFT3) (list "Lily" "Harry" "Albus"))

(define (matching-ec ec p)
  (local [(define (fn-for-person p)
            (if (string=? (person-ec p) ec)
                (cons (person-name p)                 
                      (fn-for-lop (person-kids p)))
                (fn-for-lop (person-kids p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) empty]
                  [else
                   (append (fn-for-person (first lop))
                           (fn-for-lop (rest lop)))]))]
    (fn-for-person p)))

;; DFT -> Natural
;; count the number of people in DFT that have children
(check-expect (count-have-children DFT1) 0)
(check-expect (count-have-children DFT2) 1)
(check-expect (count-have-children DFT3) 2)

(define (count-have-children p)
  (local [(define (fn-for-person p)
            (if (empty? (person-kids p))
    (fn-for-lop (person-kids p))
    (add1 (fn-for-lop (person-kids p)))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) 0]
                  [else
                   (+ (fn-for-person (first lop))
                        (fn-for-lop (rest lop)))]))]
    (fn-for-person p)))

(define (bar c f r p z l)
  (cond [(p l) z] ;X
        [else
         (c (f l ;Y)
            (bar c f r p z (r l ;Z 
                              ) ))) ;X
            ]))

(local[(define (max-value c1 c2)
         (if(>(string-length c1) (string-length c2))
            c1
            c2))]
  (foldl max-value "" (list "hola" "mundo" "etc")))