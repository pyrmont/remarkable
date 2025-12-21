(import ./state)
(import ./util)

## Blocks

(defn- append-to-document [document line grammar protocols]
  (set state/col-edge 0)
  (set state/col-pos 0)
  (var block nil)
  (var pos 0)
  (var parent-node document)
  (var peer-node (util/next-container parent-node))
  (defn contains-block? [curr-b]
    (not (nil? (util/next-child curr-b))))
  # TODO: Is this too too complicated?
  (defn descend []
    (defn descend-1 []
      (def next-block-fn (util/get-fn :next-block peer-node protocols))
      (def [new-b new-pos] (next-block-fn peer-node line pos grammar protocols))
      (var curr-b new-b)
      (var next-b (util/next-container curr-b))
      (while next-b
        (def equal-fn (util/get-fn :equal? peer-node protocols))
        (if (equal-fn peer-node curr-b)
          (do
            (set parent-node peer-node)
            (set peer-node (util/next-container peer-node))
            (set curr-b next-b)
            (set next-b (util/next-container next-b)))
          (set next-b nil)))
      (set block curr-b)
      (set pos new-pos)
      (def equal-fn (util/get-fn :equal? peer-node protocols))
      (if (and (< pos (length line)) # TODO: Is this a hack or should there be a check here?
               (equal-fn peer-node curr-b))
        [true (util/next-container peer-node)]
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
    (when-let [lazy-fn     (util/get-fn :lazy? block protocols)
               _           (lazy-fn block)
               last-n      (util/last-descendant parent-node)
               _           (util/attribute last-n :open?)
               follower-fn (util/get-fn :follower block protocols)
               follow-b    (follower-fn block last-n)]
      (set block follow-b)
      last-n))

  (defn realise-block []
    (var curr-b (or (util/last-descendant block) block))
    (while (< pos (length line))
      (def [next-b next-pos] (peg/match grammar line pos))
      (def append-fn (util/get-fn :append curr-b protocols))
      (append-fn curr-b next-b protocols)
      (set curr-b (or (util/last-descendant next-b) next-b))
      (set pos next-pos)))

  (defn append []
    (if (= :blank (util/type-of block))
      (do
        (var prev-n parent-node)
        (var curr-n (util/next-container prev-n))
        (var stopped? false)
        (while (and curr-n (not stopped?))
          (def see-blank-fn (util/get-fn :see-blank curr-n protocols))
          (if (see-blank-fn curr-n protocols)
            (set stopped? true)
            (do
              (set prev-n curr-n)
              (set curr-n (util/next-container curr-n)))))
        (if stopped?
          ((util/get-fn :blank curr-n protocols) curr-n prev-n protocols)
          (do
            (set prev-n parent-node)
            (set curr-n (util/next-container prev-n))
            (while curr-n
              (def blank-fn (util/get-fn :blank curr-n protocols))
              (def prev-prev-n prev-n)
              (set prev-n curr-n)
              (set curr-n (blank-fn prev-n prev-prev-n protocols))))))
      (do
        (def append-fn (util/get-fn :append parent-node protocols))
        (append-fn parent-node block protocols))))

  # Match against existing nodes
  (descend)

  # Either continue direct, continue lazy or close children
  (unless (= :blank (util/type-of block))
    (when-let [peer-node (util/next-child parent-node)
               equal-fn  (util/get-fn :equal? peer-node protocols)
               _         (not (equal-fn peer-node block))]
      (if (def last-node (lazy-continuation))
        (set parent-node last-node)
        (util/close-children parent-node protocols))))

  # Realise block
  (realise-block)

  # Append to parent node
  (unless (nil? block)
    (append)))


(defn parse-blocks [input grammar protocols]
  (state/reset-block-globals)
  # Normalize line endings (CRLF -> LF) per CommonMark spec
  (def normalized-input (string/replace-all "\r\n" "\n" input))
  (def block-protocols (get protocols :blocks))
  (def document [:document @{:container? true :open? true} @[]])
  (def ends (string/find-all "\n" normalized-input))
  (var start 0)
  (each end ends
    (state/reset-cols)
    (def line (string/slice normalized-input start (inc end)))
    (append-to-document document line grammar block-protocols)
    (set start (inc end)))
  (when (< start (length normalized-input)) # TODO: Is this necessary?
    (state/reset-cols)
    (def line (string/slice normalized-input start))
    (append-to-document document line grammar block-protocols))
  (util/close-children document block-protocols)
  document)

## Inlines

(defn- append-element [node element]
  (def children (util/children-of node))
  (def last-child (array/peek children))
  (if (bytes? element)
    (if (buffer? last-child)
      (buffer/push last-child element)
      (array/push children (buffer element)))
    (array/push children element)))

(defn- parse-inlines [text grammar protocols priorities]
  (state/reset-inline-globals)
  (defn priority [kind]
    (-> (get priorities :kinds) (get kind)))
  (defn match? [open-i close-i]
    (def opener (get state/delimiters open-i))
    (def closer (get state/delimiters close-i))
    (when (= (util/attribute opener :kind) (util/attribute closer :kind))
      (def match-fn (-> (get protocols (util/attribute opener :kind)) (get :match?)))
      (match-fn open-i close-i state/delimiters)))
  (defn match-up [open-i close-i]
    (def opener (get state/delimiters open-i))
    (def match-up-fn (-> (get protocols (util/attribute opener :kind)) (get :match-up)))
    (match-up-fn open-i close-i state/delimiters text))
  (defn matching-pos [closer close-i]
    (var open-i close-i)
    (while (def opener (get state/delimiters (-- open-i)))
      (when (and (= (util/attribute closer :kind)
                    (util/attribute opener :kind))
                 (zero? (util/attribute opener :count)))
        (break)))
    open-i)
  # Create inline elements
  (def elements (peg/match grammar text))
  # Match delimiters
  (each level (get priorities :levels)
    (var close-i 1)
    (while (def closer (get state/delimiters close-i))
      (var open-i close-i)
      (when (and (util/attribute closer :right?)
                 (not (zero? (util/attribute closer :count)))
                 (not (util/attribute closer :skip?))
                 (= level (priority (util/attribute closer :kind))))
        (def discards @[])
        (while (def opener (get state/delimiters (-- open-i)))
          (def opener-level (priority (util/attribute opener :kind)))
          (cond
            # If the opener level has higher priority than the current level,
            # we are either inside a pair or outside pair. If we are inside a
            # pair, this closing delimiter cannot match anything and should be
            # skipped. If we are outside a pair, we need to skip to the
            # matching delimiter of this pair before we continue searching.
            (and (> opener-level level)
                 (zero? (util/attribute opener :count))
                 (not (zero? (length (util/children-of opener)))))
            (if (= :open (util/type-of (last (util/children-of opener))))
              (do
                (util/attribute closer :skip? true)
                (++ close-i)
                (break))
              (set open-i (matching-pos opener open-i)))
            (= opener-level level)
            (if (match? open-i close-i)
              (when (match-up open-i close-i)
                (each discard discards (util/attribute discard :skip? true))
                (break))
              (array/push discards opener)))))
      (when (or (= open-i close-i) (< open-i 0))
        (++ close-i))))
  # Build tree of elements
  (def root [:root @{} @[]])
  (def ancestors @[])
  (var curr root)
  (each element elements
    (if (not= :delims (util/type-of element))
      (append-element curr element)
      (do
        # If there are any leftover characters in the delimiter we need to
        # decide either to append them to the parent either before or after
        # appending the inline elements. If the first child is :open, the
        # leftover characters should be appended first. Otherwise, the
        # characters should be appended after the inline elements.
        (def leftovers
          (unless (zero? (util/attribute element :count))
            (string/slice text
                          (util/attribute element :start-pos)
                          (util/attribute element :end-pos))))
        (def prepend-leftovers?
          (and (not= 0 (length (util/children-of element)))
               (= :open (util/type-of (first (util/children-of element))))))
        (when (and leftovers prepend-leftovers?)
          (append-element curr leftovers))
        (each delim (util/children-of element)
          (if (= :open (util/type-of delim))
            (do
              (array/push ancestors curr)
              (set curr [(util/attribute delim :kind) @{} @[]])
              (array/push (util/children-of (array/peek ancestors)) curr))
            (do
              (def close-fn (util/get-fn :close curr protocols))
              (set curr (close-fn curr delim ancestors)))))
        (when (and leftovers (not prepend-leftovers?))
          (append-element curr leftovers)))))
  # Return root
  (util/children-of root))

(defn- walk-tree [node grammar protocols priorities]
  (def children (util/children-of node))
  (if (util/attribute node :container?)
    (each child children
      (walk-tree child grammar protocols priorities))
    (when (util/attribute node :inlines?)
      (def text (-> (string/join children "\n") string/trim))
      (array/clear children)
      (array/concat children (parse-inlines text grammar protocols priorities)))))

(defn parse-all-inlines
  ```
  Parses all inline nodes within `document`

  Returns an updated `document`.
  ```
  [document grammar protocols priorities]
  (walk-tree document grammar (get protocols :inlines) priorities)
  document)
