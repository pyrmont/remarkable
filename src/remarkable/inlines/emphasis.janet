(use ../globals)
(use ../utilities)


## Grammar

(defn- emphasis [start-pos run flank &opt pre-punc? post-punc?]
  (def delim (case (get run 0) 42 "*" 95 "_"))
  (def num (length run))
  (def left? (case flank :left true :left-and-right true))
  (def right? (case flank :right true :left-and-right true))
  (def end-pos (+ num start-pos))
  (array/push delimiters
    [:delims @{:kind :emphasis :delim delim :count num :left? left? :right? right? :pre-punc? pre-punc? :post-punc? post-punc? :start-pos start-pos :end-pos end-pos} @[]])
  (array/peek delimiters))


(def grammar
  ~{:emphasis {:main   (+ (/ (* ($) :runs) ,emphasis) (/ :run ,to-fragment))
               :uws    (+ :ws # include other Unicode whitespace
                          "\u00A0" "\u1680" "\u2000" "\u2001" "\u2002" "\u2003"
                          "\u2004" "\u2005" "\u2006" "\u2007" "\u2008" "\u2009"
                          "\u200A" "\u202F" "\u205F" "\u3000")
               :runs   (+ (* (! :l-wsp) :run (! :t-wsp) (constant :left-and-right))
                          (* :l-punc    :run :t-punc    (constant :left-and-right) (constant true) (constant true))
                          (*            :run (! :t-wsp) (constant :left))
                          (* :l-wsp     :run :t-punc    (constant :left) (constant nil) (constant true))
                          (* (! :l-wsp) :run            (constant :right))
                          (* :l-punc    :run :t-wsp     (constant :right) (constant true)))
               :run    '(+ (some "*") (some "_"))
               :l-wsp  (+ :l-uws :l-punc)
               :t-wsp  (+ :t-uws :t-punc)
               :l-uws  (+ (> -1 :uws) (! (> -1 1)))
               :t-uws  (+ (> 0 :uws) (> 0 -1))
               :l-punc (> -1 :punc)
               :t-punc (> 0 :punc)}})


## Functions


(defn- emphasis-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (and (= (attribute opener :kind) (attribute closer :kind))
            (= (attribute opener :delim) (attribute closer :delim))
            (not (attribute opener :skip?))
            (not= 0 (attribute opener :count))
            (not= 0 (attribute closer :count)))
       (case (attribute closer :delim)
         "*"
         (do
           (and # rules 1 and 5
                (attribute opener :left?)
                # rules 3 and 7
                (attribute closer :right?)
                # rules 9 and 10
                (or (and (not (attribute opener :right?))
                         (not (attribute closer :left?)))
                    (or (not= 0 (% (+ (attribute opener :count) (attribute closer :count)) 3))
                        (and (= 0 (% (attribute opener :count) 3))
                             (= 0 (% (attribute closer :count) 3)))))))
         "_"
         (do
           (and # rules 2 and 6
                (and (attribute opener :left?)
                     (or (not (attribute opener :right?))
                         (attribute opener :pre-punc?)))
                # rules 4 and 8
                (and (attribute closer :right?)
                     (or (not (attribute closer :left?))
                         (attribute closer :post-punc?)))
                # rules 9 and 10
                (or (and (not (attribute opener :right?))
                         (not (attribute closer :left?)))
                    (or (not= 0 (% (+ (attribute opener :count) (attribute closer :count)) 3))
                        (and (= 0 (% (attribute opener :count) 3))
                             (= 0 (% (attribute closer :count) 3))))))))))


(defn- emphasis-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (def [kind len] (if (and (>= (attribute opener :count) 2)
                           (>= (attribute closer :count) 2))
                    [:strong 2] [:emphasis 1]))
  (array/insert (children-of opener) 0 [:open @{:kind kind} @[]])
  (attribute opener :count (- (attribute opener :count) len))
  (attribute opener :end-pos (- (attribute opener :end-pos) len))
  (array/push (children-of closer) [:close @{:kind kind} @[]])
  (attribute closer :count (- (attribute closer :count) len))
  (attribute closer :start-pos (+ (attribute closer :start-pos) len)))


(add-to rules
  {:inlines
    {:emphasis {:match?   emphasis-match?
                :match-up emphasis-match-up}}})
