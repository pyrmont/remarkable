(use ../globals)
(use ../utilities)


## Grammar

(defn- list-item [marker-width trailing-space &opt starts-blank?]
  (def start-pos col-pos)
  (+= col-edge marker-width)
  (set col-pos col-edge)
  (record-padding trailing-space)
  (if (empty? trailing-space)
    (++ col-pos)
    (+= col-pos (if (or starts-blank? (> (- col-edge col-pos) 4)) 1 (- col-edge col-pos))))
  [:list-item @{:container? true :open? true :width (- col-pos start-pos) :starts-blank? starts-blank?} @[]])


(defn- list [kind marker delim trailing-space &opt starts-blank?]
  (def start (when (= :ordinal kind) (scan-number marker)))
  (def marker-width (+ (length marker) (if (nil? delim) 0 1)))
  (def item (list-item marker-width trailing-space starts-blank?))
  [:list @{:kind kind :marker marker :delim delim :start start :tight? true :container? true :open? true} @[item]])


(def grammar
  ~{:list {:main    (cmt (* :marker :after) ,list)
           :marker  (+ :bullet :ordinal)
           :bullet  (* (constant :bullet) '(set "-+*") (constant nil))
           :ordinal (* (constant :ordinal) '(between 1 9 :d) '(set ".)"))
           :after   (+ (* '(any :space) :eol (constant true))
                       (* '(some :space)))}})


## Functions


(defn- list-blank [a-list parent functions]
  (def item (next-container a-list))
  (when (and (not (nil? item))
             (attribute item :starts-blank?)
             (zero? (length (children-of item))))
    (attribute item :open? false))
  (attribute a-list :has-blank? true)
  (next-container a-list))


(defn- list-equal? [a-list block]
  (and (= :list (type-of block))
       (= (attribute a-list :kind) (attribute block :kind))
       (or (and (= :ordinal (attribute a-list :kind))
                (= (attribute a-list :delim) (attribute block :delim)))
           (and (= :bullet (attribute a-list :kind))
                (= (attribute a-list :marker) (attribute block :marker))))))


(defn- list-needs-nl? [a-list]
  (or (attribute (next-container a-list) :starts-blank?)
      (and (= :ordinal (attribute a-list :kind))
           (not= 1 (attribute a-list :start)))))


# The current open block is a list so we need to handle the case where the
# line is a continuation of the list. To check this we need to descend through
# the open list items in the current list that are indented at least as much as
# the current line.
(defn- list-next-block [a-list line pos grammar functions]
  (def next-pos (dedent line pos))
  (var next-b nil)
  (var parent-list nil)
  (var parent-item nil)
  (var curr-list a-list)
  (var curr-item (next-container a-list))
  (while curr-item
    # Set start col of list item
    (def curr-width (attribute curr-item :width))

    # Break if there's not enough padding
    (def remaining-width (- col-edge col-pos))
    (if (> curr-width remaining-width)
      (break)
      (+= col-pos curr-width))

    # The parent list is at least equal to the current list
    (when parent-list
      (attribute parent-list :has-blank? false))
    (set parent-list curr-list)

    # Create continuation
    (def new-item [:list-item-continue (get curr-item 1) @[]])
    (def new-list [:list (get curr-list 1) [new-item]])

    # Set next-b and parent
    (if (nil? next-b)
      (set next-b new-list)
      (array/push (children-of parent-item) new-list))
    (set parent-item new-item)

    # Break if no more lists
    (def child (next-container curr-item))
    (when (or (nil? child) (not= :list (type-of child)))
      (break))

    # Prepare for next round of loop
    (set curr-list child)
    (set curr-item (next-container curr-list)))

  # Parse line from current position
  (def result (peg/match grammar line next-pos))

  # Make parent list loose if a blank line has come before
  (cond
    (and (nil? parent-list)
         (attribute a-list :has-blank?)
         (list-equal? a-list (get result 0)))
    (attribute a-list :tight? false)

    (and parent-list
         (attribute parent-list :has-blank?))
    (attribute parent-list :tight? false))

  # Return result
  (if (nil? next-b)
    result
    [next-b next-pos]))


(defn- list-item-equal? [an-item block]
  (= :list-item-continue (type-of block)))


(add-to rules
  {:blocks
    {:list       {:blank       list-blank
                  :equal?      list-equal?
                  :needs-nl?   list-needs-nl?
                  :next-block  list-next-block}
     :list-item  {:equal?      list-item-equal?}}})
