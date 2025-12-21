(import ../state)
(import ../util)

## Grammar

(defn- codespan [delim content]
  (def buf @"")
  (var only-spaces? true)
  (each c content
    (case c
      10 (buffer/push buf " ")
      32 (buffer/push buf c)
         (do
           (buffer/push buf c)
           (set only-spaces? false))))
  (def [start end]
    (if (and (= 32 (first buf))
             (= 32 (last buf))
             (not only-spaces?))
      [1 (dec (length buf))]
      [0 nil]))
  [:codespan @{} (string/slice buf start end)])

(def grammar
  ~{:codespan {:main  (+ (unref (/ (* :open '(to :close) :close) ,codespan))
                         '(some "`"))
               :open  (<- (some "`") :delim)
               :close (* (! (> -1 "`")) (backmatch :delim) (not "`"))}})

