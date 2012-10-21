(define rev 
  (letrec rev2 
    (lambda (x y) (if (null x) y (rev2 (cdr x) (cons (car x) y)))) 
    (lambda x (rev2 x nil)))
    )

(definerec (fold_right f l acc) 
  (if (null l) 
    acc 
    (fold_right f (cdr l) (f acc (car l)))
    ))

(define (fold_left f acc l) 
  (fold_right f (rev l) acc))

(definerec (map f l ) 
  (if (null l) 
    Nil 
    (cons (f (car l)) (map f (cdr l)))
    ))

(definerec (append l1 l2) 
  (if (null l1) 
    l2 
    (cons (car l1) (append (cdr l1) l2) )
    ))

(definerec (flatten l) 
  (if (null l) 
    Nil 
    (append (car l) (flatten (cdr l)))
    ))

(definerec (mem f l) 
  (if (null l) 
     Nil 
     (if (f (car l)) 
       (cons (car l) (mem f (cdr l))) 
       (mem f (cdr l)))))

(definerec (quicksort l) 
   (if (null l) 
     Nil 
     (flatten 
        (list
        (quicksort (mem (lambda (x) (<= x (car l))) (cdr l))) 
        (list (car l))
        (quicksort (mem (lambda (x) (> x (car l))) (cdr l)))
        )
        )))

