(defn get-x (p) (car p))
(defn get-y (p) (car (cdr p)))

(defn difference-square (x y)
  (* (- x y) (- x y)))

(defn distance-square (p1 p2)
  (+
   (difference-square (get-x p1) (get-x p2))
   (difference-square (get-y p1) (get-y p2))))

(defn sqrt-helper (l r square)
  (if (> l r) r
      (progn
        (let m (/ (+ l r) 2))
        (let m-sq (* m m))
        (cond
          ((< m-sq square) (sqrt-helper (+ m 1) r square))
          ((> m-sq square) (sqrt-helper l (- m 1) square))
          ((= m-sq square) m)))))

(defn sqrt (x) (sqrt-helper 1 x x))

(defn distance (p1 p2)
  (sqrt (distance-square p1 p2)))

(if (= (distance '(2 13) '(9 -2)) 16) "OK!" "Wrong answer!")
(if (= (distance '(0 0) '(3 4)) 5) "OK!" "Wrong answer!")
