(defn fib (x)
  (if (<= x 2) 1
      (+ (fib (- x 1)) (fib (- x 2)))))

(def xs '(1 5 10 15 20 25))
(map fib xs)
