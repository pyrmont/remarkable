(use ./remarkable/globals)
(use ./remarkable/utilities)


(import ./remarkable/blocks)
(import ./remarkable/inlines)
(import ./remarkable/renderers/html)


(import ./remarkable/debug)


## Looping

(defn- append-to-document [document line grammar functions]
  (set col-edge 0)
  (set col-pos 0)
  (var block nil)
  (var pos 0)
  (var parent-node document)
  (var peer-node (next-container parent-node))

  (defn contains-block? [curr-b]
    (not (nil? (next-child curr-b))))

  # TODO Is this too too complicated?
  (defn descend []
    (defn descend-1 []
      (def next-block-fn (get-fn :next-block peer-node functions))
      (def [new-b new-pos] (next-block-fn peer-node line pos grammar functions))
      (var curr-b new-b)
      (var next-b (next-container curr-b))
      (while next-b
        (def equal-fn (get-fn :equal? peer-node functions))
        (if (equal-fn peer-node curr-b)
          (do
            (set parent-node peer-node)
            (set peer-node (next-container peer-node))
            (set curr-b next-b)
            (set next-b (next-container next-b)))
          (set next-b nil)))
      (set block curr-b)
      (set pos new-pos)
      (def equal-fn (get-fn :equal? peer-node functions))
      (if (and (< pos (length line)) # TODO Is this a hack or should there be a check here?
               (equal-fn peer-node curr-b))
        [true (next-container peer-node)]
        [false nil]))
    (var keep-descending? true)
    (while (and (< pos (length line))
                keep-descending?)
      (def [match? next-n] (descend-1))
      (if match?
        (if (nil? next-n)
          (do
            (set keep-descending? false)
            (def [next-b next-pos] (peg/match grammar line pos))
            (set block next-b)
            (set pos next-pos)
            (set parent-node peer-node))
          (do
            (set parent-node peer-node)
            (set peer-node next-n)))
        (set keep-descending? false))))

  (defn lazy-continuation []
    (when-let [lazy-fn     (get-fn :lazy? block functions)
               _           (lazy-fn block)
               last-n      (last-descendant parent-node)
               _           (attribute last-n :open?)
               follower-fn (get-fn :follower block functions)
               follow-b    (follower-fn block last-n)]
      (set block follow-b)
      last-n))

  (defn realise-block []
    (var curr-b (or (last-descendant block) block))
    (while (< pos (length line))
      (def [next-b next-pos] (peg/match grammar line pos))
      (def append-fn (get-fn :append curr-b functions))
      (append-fn curr-b next-b functions)
      (set curr-b (or (last-descendant next-b) next-b))
      (set pos next-pos)))

  (defn append []
    (if (= :blank (type-of block))
      (do
        (var prev-n parent-node)
        (var curr-n (next-container prev-n))
        (var stopped? false)
        (while (and curr-n (not stopped?))
          (def see-blank-fn (get-fn :see-blank curr-n functions))
          (if (see-blank-fn curr-n functions)
            (set stopped? true)
            (do
              (set prev-n curr-n)
              (set curr-n (next-container curr-n)))))
        (if stopped?
          ((get-fn :blank curr-n functions) curr-n prev-n functions)
          (do
            (set prev-n parent-node)
            (set curr-n (next-container prev-n))
            (while curr-n
              (def blank-fn (get-fn :blank curr-n functions))
              (def prev-prev-n prev-n)
              (set prev-n curr-n)
              (set curr-n (blank-fn prev-n prev-prev-n functions))))))
      (do
        (def append-fn (get-fn :append parent-node functions))
        (append-fn parent-node block functions))))

  # Match against existing nodes
  (descend)

  # Either continue direct, continue lazy or close children
  (unless (= :blank (type-of block))
    (when-let [peer-node (next-child parent-node)
               equal-fn  (get-fn :equal? peer-node functions)
               _         (not (equal-fn peer-node block))]
      (if (def last-node (lazy-continuation))
        (set parent-node last-node)
        (close-children parent-node functions))))

  # Realise block
  (realise-block)

  # Append to parent node
  (unless (nil? block)
    (append)))


(defn- parse-blocks [input grammar functions]
  (reset-block-globals)

  (def block-functions (get functions :blocks))
  (def document [:document @{:container? true :open? true} @[]])
  (def ends (string/find-all "\n" input))
  (var start 0)

  (each end ends
    (reset-cols)
    (def line (string/slice input start (inc end)))
    (append-to-document document line grammar block-functions)
    (set start (inc end)))
  (when (< start (length input)) # TODO Is this necessary?
    (reset-cols)
    (def line (string/slice input start))
    (append-to-document document line grammar block-functions))
  (close-children document block-functions)

  document)


(defn- append-element [node element]
  (def children (children-of node))
  (def last-child (array/peek children))
  (if (= :fragment (type-of element))
    (if (buffer? last-child)
      (buffer/push last-child (get element 1))
      (array/push children (buffer (get element 1))))
    (array/push children element)))


(defn- parse-inlines [text grammar functions priorities]
  (reset-inline-globals)

  (defn priority [kind]
    (-> (get priorities :kinds) (get kind)))

  (defn match? [open-i close-i]
    (def opener (get delimiters open-i))
    (def closer (get delimiters close-i))
    (when (= (attribute opener :kind) (attribute closer :kind))
      (def match-fn (-> (get functions (attribute opener :kind)) (get :match?)))
      (match-fn open-i close-i delimiters)))

  (defn match-up [open-i close-i]
    (def opener (get delimiters open-i))
    (def match-up-fn (-> (get functions (attribute opener :kind)) (get :match-up)))
    (match-up-fn open-i close-i delimiters text))

  (defn matching-pos [closer close-i]
    (var open-i close-i)
    (while (def opener (get delimiters (-- open-i)))
      (when (and (= (attribute closer :kind) (attribute opener :kind))
                 (zero? (attribute opener :count)))
        (break)))
    open-i)

  # Create inline elements
  (def elements (peg/match grammar text))

  # Match delimiters
  (each level (get priorities :levels)
    (var close-i 1)
    (while (def closer (get delimiters close-i))
      (var open-i close-i)
      (when (and (attribute closer :right?)
                 (not (zero? (attribute closer :count)))
                 (not (attribute closer :skip?))
                 (= level (priority (attribute closer :kind))))
        (def discards @[])
        (while (def opener (get delimiters (-- open-i)))
          (def opener-level (priority (attribute opener :kind)))
          (cond
            # If the opener level has higher priority than the current level,
            # we are either inside a pair or outside pair. If we are inside a
            # pair, this closing delimiter cannot match anything and should be
            # skipped. If we are outside a pair, we need to skip to the
            # matching delimiter of this pair before we continue searching.
            (and (> opener-level level)
                 (zero? (attribute opener :count))
                 (not (zero? (length (children-of opener)))))
            (if (= :open (type-of (last (children-of opener))))
              (do
                (attribute closer :skip? true)
                (++ close-i)
                (break))
              (set open-i (matching-pos opener open-i)))

            (= opener-level level)
            (if (match? open-i close-i)
              (when (match-up open-i close-i)
                (each discard discards (attribute discard :skip? true))
                (break))
              (array/push discards opener)))))
      (when (or (= open-i close-i) (< open-i 0))
        (++ close-i))))

  # Build tree of elements
  (def root [:root @{} @[]])
  (def ancestors @[])
  (var curr root)
  (var ignore? false)
  (each element elements
    (if (not= :delims (type-of element))
      (unless ignore?
        (append-element curr element))
      (do
        # If there are any leftover characters in the delimiter we need to
        # decide either to append them to the parent either before or after
        # appending the inline elements. If the first child is :open, the
        # leftover characters should be appended first. Otherwise, the
        # characters should be appended after the inline elements.
        (def leftovers
          (unless (zero? (attribute element :count))
            [:fragment (string/slice text (attribute element :start-pos) (attribute element :end-pos))]))

        (def prepend-leftovers?
          (and (not= 0 (length (children-of element)))
               (= :open (type-of (first (children-of element))))))

        (when (and leftovers prepend-leftovers?)
          (append-element curr leftovers))

        (each delim (children-of element)
          (if (= :open (type-of delim))
            (do
              (array/push ancestors curr)
              (set curr [(attribute delim :kind) @{} @[]])
              (array/push (children-of (array/peek ancestors)) curr))
            (do
              (def close-fn (get-fn :close curr functions))
              (set curr (close-fn curr delim ancestors)))))

        (when (and leftovers (not prepend-leftovers?))
          (append-element curr leftovers)))))

  # Return root
  (children-of root))


(defn- parse-all-inlines [document grammar functions priorities]
  (def inline-functions (get functions :inlines))

  (defn walk-tree [node]
    (def children (children-of node))
    (if (attribute node :container?)
      (each child children
        (walk-tree child))
      (when (attribute node :inlines?)
        (def text (-> (string/join children "\n") string/trim))
        (array/clear children)
        (array/concat children (parse-inlines text grammar inline-functions priorities)))))

  (walk-tree document)
  document)


(defn parse-md [input &opt your-blocks your-inlines your-functions your-priorities]
  (default your-blocks (-> (table/to-struct blocks/grammar) peg/compile))
  (default your-inlines (peg/compile inlines/grammar))
  (default your-functions rules)
  (default your-priorities priorities)
  (-> input
      (parse-blocks your-blocks your-functions)
      (parse-all-inlines your-inlines your-functions your-priorities)))


(defn render-html [node &opt opts]
  (html/render node opts))
