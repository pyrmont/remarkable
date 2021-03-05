(defn tap> [note val]
  (print "--- " note " ---")
  (pp val)
  (print))


(defn dapper [node &opt level]
  (default level 0)
  (def first-indent (string/repeat " " 0))
  (def item-indent (string/repeat " " (inc level)))

  (cond
    (indexed? node)
    (do
      (prinf "[")
      (var first-item? true)
      (each val node
        (if first-item?
          (prinf "%s" first-indent)
          (prinf "\n%s" item-indent))
        (if (or (indexed? val) (dictionary? val))
          (dapper val (inc level))
          (prinf "%s" (describe val)))
        (set first-item? false))
      (prinf "]"))

    (dictionary? node)
    (do
      (prinf "{")
      (var first-item? true)
      (each [key val] (pairs node)
        (if first-item?
          (prinf "%s" first-indent)
          (prinf "\n%s" item-indent))
        (prinf "%s" (describe key))
        (if (or (indexed? val) (dictionary? val))
          (do
            (prinf (dapper val (inc level))))
          (prinf " %s" (describe val)))
        (set first-item? false))
      (prinf "}")))

  (when (zero? level)
    (prinf "\n")))


