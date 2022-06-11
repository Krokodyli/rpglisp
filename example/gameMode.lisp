(defn init ()
  (def is-running #t))

(defn update ()
  (if (eq? input ":q")
      (def is-running #f)
      (def output input)))
