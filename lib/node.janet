# Node manipulation functions
#
# This module provides functions for inspecting and manipulating AST nodes.
# These are useful when writing custom block/inline protocols and renderers.

(defn type-of
  ```
  Gets the type of the node
  ```
  [node]
  (first node))

(defn attribute
  ```
  Gets or sets the attribute `attr` of `node`

  Whether this function gets or sets depends on whether `value` is provided.
  ```
  [node attr &opt value]
  (if (nil? value)
    (get (get node 1) attr)
    (put (get node 1) attr value)))

(defn children-of
  ```
  Gets the children of `node`
  ```
  [node]
  (last node))

(defn next-child
  ```
  Gets the last child of `node` if it is a container

  Returns nil if `node` is not a container.
  ```
  [node]
  (when (attribute node :container?)
    (last (children-of node))))

(defn next-container
  ```
  Gets the next open child of `node`

  Returns nil if the next child is not open.
  ```
  [node]
  (def child (next-child node))
  (when (and child (attribute child :open?))
    child))

(defn get-fn
  ```
  Gets a function called `name` associated with the type of `node`

  If there is no function called `name` associated with the type, instead gets
  the function called `name` from the default group.
  ```
  [name node functions]
  (or (get (get functions (type-of node)) name)
      (get (get functions 'default) name)))

(defn close-children
  ```
  Closes the children of `parent`
  ```
  [parent functions]
  (var prev parent)
  (while (def node (next-container prev))
    (def close-fn (get-fn :close node functions))
    (close-fn node prev)
    (set prev node)))

(defn last-descendant
  ```
  Gets the last descendant of `node`
  ```
  [node]
  (var curr-n (next-child node))
  (while (def next-n (next-child curr-n))
    (set curr-n next-n))
  curr-n)
