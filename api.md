# remarkable API

[extend/add-block](#extendadd-block), [extend/add-inline](#extendadd-inline), [extend/build-blocks-grammar](#extendbuild-blocks-grammar), [extend/build-inlines-grammar](#extendbuild-inlines-grammar), [extend/build-protocols](#extendbuild-protocols), [extend/make-container-protocol](#extendmake-container-protocol), [node/attribute](#nodeattribute), [node/children-of](#nodechildren-of), [node/close-children](#nodeclose-children), [node/get-fn](#nodeget-fn), [node/last-descendant](#nodelast-descendant), [node/next-child](#nodenext-child), [node/next-container](#nodenext-container), [node/type-of](#nodetype-of), [parse-md](#parse-md), [render-html](#render-html), [reset-state](#reset-state)

## extend/add-block

**function**  | [source][1]

```janet
(add-block block-type block-grammar &opt block-protocol &named first?)
```

Adds a custom block type

This function adds a custom block type to the parser. The block must have a
unique `block-type` and a `block-grammar` comprised of PEG combinators.
Optionally, a `block-protocol` table can be provided with protocol functions
for the block type.

If the block type has high precedence, the named parameter `:first?` can be
set to `true` and the block type be matched before existing blocks.

The block type, grammar, and protocol are stored in global state, which is
used by build-blocks-grammar and build-protocols to construct the final
grammar and protocols.

[1]: lib/extend.janet#L7


## extend/add-inline

**function**  | [source][2]

```janet
(add-inline inline-type inline-grammar &opt inline-protocol &named first? delimiters)
```

Adds a custom inline type

This function adds a custom inline type to the parser. The inline must have a
unique `inline-type` and a `inline-grammar` comprised of PEG combinators.
Optionally, an `inline-protocol` table can be provided with protocol functions
for the inline type.

If the inline type has high precedence, the named parameter `:first?` can be
set to `true` and the inline type be matched before existing inlines.

The named parameter `:delimiters` must be used to specify characters that start
this inline pattern if they are not existing delimiters (e.g. `_` and `*`).

The inline type, grammar, and protocol are stored in global state, which is
used by build-inlines-grammar and build-protocols to construct the final
grammar and protocols.

[2]: lib/extend.janet#L50


## extend/build-blocks-grammar

**function**  | [source][3]

```janet
(build-blocks-grammar)
```

Builds a complete block grammar from state/blocks and
state/custom-block-grammars

This function uses the custom grammar rules that were registered via
add-block, merging them with the base blocks grammar. Returns a grammar table
ready to be compiled with peg/compile

[3]: lib/extend.janet#L31


## extend/build-inlines-grammar

**function**  | [source][4]

```janet
(build-inlines-grammar)
```

Builds a complete inline grammar from state/inlines and
state/custom-inline-grammars

This function uses the custom grammar rules that were registered via
add-inline, merging them with the base inlines grammar. If custom inline
delimiters were specified, it creates a custom :char rule that excludes
those characters. Returns a grammar table ready to be compiled with peg/compile

[4]: lib/extend.janet#L79


## extend/build-protocols

**function**  | [source][5]

```janet
(build-protocols)
```

Builds complete protocols from state/protocols and custom protocols

This function uses the custom protocols that were registered via add-block
and add-inline, merging them with the base protocols. Returns a protocols
table ready to be used with parse-md.

[5]: lib/extend.janet#L106


## extend/make-container-protocol

**function**  | [source][6]

```janet
(make-container-protocol &opt overrides)
```

Creates a container block protocol with optional overrides

This is a convenience wrapper around container/make-protocol for creating
protocols for custom container blocks (blocks that can contain other blocks).

Container blocks share common behavior for managing child blocks. This function
returns base implementations that can be customized via the optional `overrides`
parameter.

Overrides:
- `:equal?`: Custom equality check (default: type equality)
- `:see-blank`: Called when blank line seen (default: close container)
- `:blank`: Handle blank lines (default: pass to next container)
- `:next-block`: Custom next block matching (default: delegate to grammar)
- `:needs-nl?`: Whether container needs newline before closing

[6]: lib/extend.janet#L127


## node/attribute

**function**  | [source][7]

```janet
(attribute node attr &opt value)
```

Gets or sets the attribute `attr` of `node`

Whether this function gets or sets depends on whether `value` is provided.

[7]: lib/node.janet#L13


## node/children-of

**function**  | [source][8]

```janet
(children-of node)
```

Gets the children of `node`

[8]: lib/node.janet#L24


## node/close-children

**function**  | [source][9]

```janet
(close-children parent functions)
```

Closes the children of `parent`

[9]: lib/node.janet#L63


## node/get-fn

**function**  | [source][10]

```janet
(get-fn name node functions)
```

Gets a function called `name` associated with the type of `node`

If there is no function called `name` associated with the type, instead gets
the function called `name` from the default group.

[10]: lib/node.janet#L52


## node/last-descendant

**function**  | [source][11]

```janet
(last-descendant node)
```

Gets the last descendant of `node`

[11]: lib/node.janet#L74


## node/next-child

**function**  | [source][12]

```janet
(next-child node)
```

Gets the last child of `node` if it is a container

Returns nil if `node` is not a container.

[12]: lib/node.janet#L31


## node/next-container

**function**  | [source][13]

```janet
(next-container node)
```

Gets the next open child of `node`

Returns nil if the next child is not open.

[13]: lib/node.janet#L41


## node/type-of

**function**  | [source][14]

```janet
(type-of node)
```

Gets the type of the node

[14]: lib/node.janet#L6


## parse-md

**function**  | [source][15]

```janet
(parse-md input &named blocks inlines protocols priorities)
```

Parses Markdown into an AST

This function transforms `input` into an AST. It optionally takes a number of
named parameters for extending the parser:

- `:blocks`: custom blocks PEG
- `:inlines`: custom inlines PEG
- `:protocols`: custom block/inline protocols
- `:priorities`: custom delimiter priorities for inlines

There are helper utilities for extension in lib/extend.janet.

[15]: lib/init.janet#L8


## render-html

**function**  | [source][16]

```janet
(render-html root &opt opts)
```

Renders an AST node to HTML

This functions transforms `root` into an HTML string. It can also be called
with a struct/table `opts` with the following keys:

- `:renderers`: custom renderer functions
- `:start-nl?`: start with newline
- `:inner?`: skip wrapper tags for paragraphs

Render functions have the signature `(defn my-renderer [node renderers opts] ...)`.

[16]: lib/init.janet#L31


## reset-state

**function**  | [source][17]

```janet
(reset-state)
```

Resets the global state to defaults

This function clears all custom grammars and protocols that were added via
the extend module, resetting the state to the default configuration. This is
primarily useful for testing when you want to ensure a clean state between
tests.

[17]: lib/init.janet#L47

