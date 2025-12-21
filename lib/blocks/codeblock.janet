(import ../state)
(import ../util)

## Grammar

(defn- codeblock-f [delim fence info]
  (def num (inc (length fence)))
  (def [lang extra]
    (if (empty? info)
      [nil]
      (if (def first-space (string/find " " info))
        [(string/slice info 0 first-space) (string/slice info (inc first-space))]
        [info nil])))
  (def indent (- state/col-edge state/col-pos))
  [:codeblock @{:open? true :kind :fenced :delim delim :num num :info lang :extra extra :indent indent} @[]])

(defn- codeblock-i [text]
  (def indent (- state/col-edge state/col-pos))
  (when (>= indent 4)
    (def extra-cols (- indent 4))
    (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
    (def line (string extra-space text))
    [:codeblock @{:open? true :kind :indented} @[line]]))

(def grammar
  ~{:codeblock {:main     (+ :indented :fenced)
                :indented {:main (cmt (* :code :eol) ,codeblock-i)
                           :code '(thru :eol)}
                :fenced   {:main  (/ (* :fence :eol) ,codeblock-f)
                           :fence (+ :tilde :tick)
                           :tilde (* '"~" '(at-least 2 "~") (any :space) (% (any :char)))
                           :tick  (* '"`" '(at-least 2 "`") (any :space) (% (any (if-not "`" :char))))}}})

## Functions

(defn- codeblock-blank [a-codeblock parent protocols]
  (when (util/attribute a-codeblock :open?)
    (array/push (util/children-of a-codeblock) "\n"))
  nil)

(defn- codeblock-close [a-codeblock &opt parent]
  (util/attribute a-codeblock :open? false)
  (when (= :indented (util/attribute a-codeblock :kind))
    (def lines (util/children-of a-codeblock))
    (def last-index (dec (length lines)))
    (var start 0)
    (var line (get lines start))
    (while (string/check-set " \t\n" line)
      (set line (get lines (++ start))))
    (var end last-index)
    (set line (get lines end))
    (while (string/check-set " \t\n" line)
      (set line (get lines (-- end))))
    (unless (and (= 0 start) (= last-index end))
      (array/remove lines (inc end) (- last-index end))
      (array/remove lines 0 start))))

(defn- codeblock-continue [a-codeblock block]
  (def lines (util/children-of block))
  (if (= :close (first lines))
    (codeblock-close a-codeblock)
    (array/concat (util/children-of a-codeblock) lines)))

(defn- codeblock-equal? [a-codeblock block]
  (and (= :codeblock (util/type-of block))
       (util/attribute a-codeblock :open?)))

(defn- codeblock-follower [a-codeblock block]
  (when (= :paragraph (util/type-of block))
    [:paragraph {} (util/children-of a-codeblock)]))

(defn- codeblock-lazy? [a-codeblock]
  (= :indented (util/attribute a-codeblock :kind)))

(defn- codeblock-needs-nl? [a-codeblock]
  (= :indented (util/attribute a-codeblock :kind)))

(defn- codeblock-next-block [a-codeblock line pos grammar protocols]
  (if (= :indented (util/attribute a-codeblock :kind))
    (peg/match grammar line pos)
    (do
      (def delim (util/attribute a-codeblock :delim))
      (def delim-num (util/attribute a-codeblock :num))
      (def max-indent (util/attribute a-codeblock :indent))
      (defn fence []
        (when (< (- state/col-edge state/col-pos) 4)
          [:codeblock {} [:close]]))
      (defn code [text]
        (def extra-cols (- state/col-edge state/col-pos max-indent))
        (def extra-space (when (> extra-cols 0) (string/repeat " " extra-cols)))
        (def line (string extra-space text))
        [:codeblock {} @[line]])
      (def fence-grammar
        ~{:main    (* :padding (+ :fence :code) :eol ($))
          :eol     -1
          :ws      (set " \t\n")
          :padding (drop (/ '(any (set " \t")) ,util/record-padding))
          :fence   (cmt (* (at-least ,delim-num ,delim) (any :ws) (> 0 :eol)) ,fence)
          :code    (/ '(thru :eol) ,code)})
      (peg/match fence-grammar line pos))))

(defn- codeblock-see-blank [a-codeblock protocols]
  (util/attribute a-codeblock :open?))

(util/add-to state/protocols
  @{:blocks
    @{:codeblock  {:blank      codeblock-blank
                   :close      codeblock-close
                   :continue   codeblock-continue
                   :equal?     codeblock-equal?
                   :follower   codeblock-follower
                   :lazy?      codeblock-lazy?
                   :needs-nl?  codeblock-needs-nl?
                   :next-block codeblock-next-block
                   :see-blank  codeblock-see-blank}}})
