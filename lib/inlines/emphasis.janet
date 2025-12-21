(import ../state)
(import ../util)

## Grammar

(defn- emphasis [start-pos run flank &opt pre-punc? post-punc?]
  (def delim (case (get run 0) 42 "*" 95 "_"))
  (def num (length run))
  (def left? (case flank :left true :left-and-right true))
  (def right? (case flank :right true :left-and-right true))
  (def end-pos (+ num start-pos))
  (array/push state/delimiters
    [:delims @{:kind :emphasis :delim delim :count num :left? left? :right? right? :pre-punc? pre-punc? :post-punc? post-punc? :start-pos start-pos :end-pos end-pos} @[]])
  (array/peek state/delimiters))

(def grammar
  ~{:emphasis {:main   (+ (/ (* ($) :runs) ,emphasis) :run)
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
               # UTF-8 aware lookbehind: verify start byte matches offset to ensure char ends here
               :l-punc (+ (* (> -1 (range "\x00\x7F")) (> -1 :punc))  # 1-byte (ASCII)
                          (* (> -2 (range "\xC0\xDF")) (> -2 :punc))  # 2-byte
                          (* (> -3 (range "\xE0\xEF")) (> -3 :punc))  # 3-byte
                          (* (> -4 (range "\xF0\xF7")) (> -4 :punc))) # 4-byte
               :t-punc (> 0 :punc)}})

## Functions

(defn- emphasis-match? [open-i close-i delimiters]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (and (and (= (util/attribute opener :kind) (util/attribute closer :kind))
            (= (util/attribute opener :delim) (util/attribute closer :delim))
            (not (util/attribute opener :skip?))
            (not= 0 (util/attribute opener :count))
            (not= 0 (util/attribute closer :count)))
       (case (util/attribute closer :delim)
         "*"
         (do
           (and # rules 1 and 5
                (util/attribute opener :left?)
                # rules 3 and 7
                (util/attribute closer :right?)
                # rules 9 and 10
                (or (and (not (util/attribute opener :right?))
                         (not (util/attribute closer :left?)))
                    (or (not= 0 (% (+ (util/attribute opener :count) (util/attribute closer :count)) 3))
                        (and (= 0 (% (util/attribute opener :count) 3))
                             (= 0 (% (util/attribute closer :count) 3)))))))
         "_"
         (do
           (and # rules 2 and 6
                (and (util/attribute opener :left?)
                     (or (not (util/attribute opener :right?))
                         (util/attribute opener :pre-punc?)))
                # rules 4 and 8
                (and (util/attribute closer :right?)
                     (or (not (util/attribute closer :left?))
                         (util/attribute closer :post-punc?)))
                # rules 9 and 10
                (or (and (not (util/attribute opener :right?))
                         (not (util/attribute closer :left?)))
                    (or (not= 0 (% (+ (util/attribute opener :count) (util/attribute closer :count)) 3))
                        (and (= 0 (% (util/attribute opener :count) 3))
                             (= 0 (% (util/attribute closer :count) 3))))))))))

(defn- emphasis-match-up [open-i close-i delimiters text]
  (def opener (get delimiters open-i))
  (def closer (get delimiters close-i))
  (def [kind len] (if (and (>= (util/attribute opener :count) 2)
                           (>= (util/attribute closer :count) 2))
                    [:strong 2] [:emphasis 1]))
  (array/insert (util/children-of opener) 0 [:open @{:kind kind} @[]])
  (util/attribute opener :count (- (util/attribute opener :count) len))
  (util/attribute opener :end-pos (- (util/attribute opener :end-pos) len))
  (array/push (util/children-of closer) [:close @{:kind kind} @[]])
  (util/attribute closer :count (- (util/attribute closer :count) len))
  (util/attribute closer :start-pos (+ (util/attribute closer :start-pos) len)))

(util/add-to state/protocols
  {:inlines
    {:emphasis {:match?   emphasis-match?
                :match-up emphasis-match-up}}})
