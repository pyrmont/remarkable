(import ../state)
(import ../util)
(import ../node)

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
  (when (node/attribute a-codeblock :open?)
    (array/push (node/children-of a-codeblock) "\n"))
  nil)

(defn- codeblock-close [a-codeblock &opt parent]
  (node/attribute a-codeblock :open? false)
  (when (= :indented (node/attribute a-codeblock :kind))
    (def lines (node/children-of a-codeblock))
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
  (def lines (node/children-of block))
  (if (= :close (first lines))
    (codeblock-close a-codeblock)
    (array/concat (node/children-of a-codeblock) lines)))

(defn- codeblock-equal? [a-codeblock block]
  (and (= :codeblock (node/type-of block))
       (node/attribute a-codeblock :open?)))

(defn- codeblock-follower [a-codeblock block]
  (when (= :paragraph (node/type-of block))
    [:paragraph {} (node/children-of a-codeblock)]))

(defn- codeblock-lazy? [a-codeblock]
  (= :indented (node/attribute a-codeblock :kind)))

(defn- codeblock-needs-nl? [a-codeblock]
  (= :indented (node/attribute a-codeblock :kind)))

(defn- codeblock-next-block [a-codeblock line pos grammar protocols]
  (if (= :indented (node/attribute a-codeblock :kind))
    (peg/match grammar line pos)
    (do
      (def delim (node/attribute a-codeblock :delim))
      (def delim-num (node/attribute a-codeblock :num))
      (def max-indent (node/attribute a-codeblock :indent))
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
  (node/attribute a-codeblock :open?))

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
