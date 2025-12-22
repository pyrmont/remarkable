# Remarkable

[![Test Status][icon]][status]

[icon]: https://github.com/pyrmont/remarkable/workflows/test/badge.svg
[status]: https://github.com/pyrmont/remarkable/actions?query=workflow%3Atest

Remarkable is a [CommonMark][] Markdown parser written in Janet. Remarkable is:

[CommonMark]: https://commonmark.org/

- **Compliant** - Passes all 652 examples from version 0.31.2 of the
  CommonMark spec
- **Flexible** - Includes an HTML renderer but can return a parse tree
- **Extensible** - Accepts user-provided grammars and protocols for blocks and
  inlines

## Rationale

Parsing and rendering Markdown is a trade-off between speed and flexibility.
For projects that require speed, Janet projects can use [Markable][], a Janet
wrapper around CommonMark's C reference implementation.

[Markable]: https://github.com/pyrmont/markable

But what if you value flexibility? Perhaps you are in a situation where you
cannot expect the user to perform a compile step as part of installation.
Perhaps you want to make a parser for your bespoke flavour of Markdown. Perhaps
you really, really like PEGs.

Remarkable might be the answer.

## Library

### Installing

Remarkable can be used as a modern bundle or a legacy bundle. Simply add the
dependency to your `info.jdn` or `project.janet` file:

```janet
  :dependencies ["https://github.com/pyrmont/remarkable"]
```

### Using

Basic usage:

```janet
(import remarkable)

(def markdown "# Hello World\n\nThis is **bold** text.")
(def ast (remarkable/parse-md markdown))
(def html (remarkable/render-html ast))
```

This returns:

```html
<h1>Hello World</h1>
<p>This is <strong>bold</strong> text.</p>
```

### Extensibility

Remarkable is designed to be extended with custom block and inline types. The
architecture provides two key extensibility features:

#### 1. Custom Types

Add custom block or inline types with composable extension functions:

```janet
(defn- note-block [content]
  [:note @{:container? true :open? true} @[content]])

(def note-grammar
  ~{:note (* "NOTE:" (/ '(to :eol) ,note-block))})

(def note-protocol
  {:blocks
    {:custom-block (remarkable/extend/make-container-protocol)}})

# Add type
(remarkable/extend/add-block :note note-grammar note-protocol)

# Build grammar and protocols
(def custom-grammar (remarkable/extend/build-blocks-grammar))
(def custom-protocols (remarkable/extend/build-protocols))

# Parse with custom types
(def ast (remarkable/parse-md text
           :blocks (peg/compile custom-grammar)
           :protocols custom-protocols))
```

#### 2. Custom Renderers

Add custom rendering logic without modifying core files:

```janet
(defn render-note [node renderers opts]
  (def [_ _ children] node)
  (string "<div class=\"note\">" (first children) "</div>"))

(def html (remarkable/render-html ast
            {:renderers {:note render-note}}))
```

See `test/extensibility.janet` for some examples. For detailed API
documentation, see the [API document](api.md).

## CLI Utility

Remarkable includes `remark`, a basic CLI utility that can render a Markdown
file into HTML.

### Using

```shell
$ remark in.md --output out.html
```

## Development

### Design

Remarkable uses a two-pass parsing approach:

1. **Block parsing** - Identifies block-level structures (paragraphs, lists,
   code blocks, etc.)
2. **Inline parsing** - Processes inline elements (emphasis, links, code spans,
   etc.) within blocks

The parser is modular, with separate files for each Markdown feature in
`lib/blocks/` and `lib/inlines/`.

### Testing

A JDN file with all 652 examples from v0.31.2 of the CommonMark specification
are included in the repository. To test Remarkable against these, run:

```bash
janet test/cmark_spec.janet      # Run all CommonMark tests
janet test/cmark_spec.janet 123  # Run specific example
```

To regenerate the examples from the official spec, run:

```bash
janet res/tools/make-spec.janet  # Downloads and generates examples from the spec
```

## Bugs

Found a bug? I'd love to know about it. The best way is to report your bug in
the [Issues][] section on GitHub.

[Issues]: https://github.com/pyrmont/remarkable/issues

## Licence

Remarkable is licensed under the MIT License. See [LICENSE][] for more details.

[LICENSE]: https://github.com/pyrmont/remarkable/blob/master/LICENSE
