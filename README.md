cl-markup - Simple markup parser
================================

This library implements a markup parser that is based on the
language defined in CommonMark. It is not, however, indented to be a
full implementation of the language.

Supported tags
--------------

The following markup tags are supported:

  - Bold: `*Bold*`.
  - Italics: `_Italics_`
  - URL's: `http://example.com`, `[[http://example.com]]` or
    `[[http://example.com][Link]]`
  - Blockquotes: `> Example`.
  - Code: `Code`
  - Code blocks: Enclose the block between triple-`.

Parser API
----------

To parse a string, use the function `MARKUP-PARAGRAPHS`. If set, the
keyword parameter `:ALLOW-NL` indicates that newlines should not be
collapsed and instead be preserved in the output.

The result of `MARKUP-PARAGRAPHS` is an alist representing the
structure of the document.

To render the parsed structure to HTML, use `RENDER-MARKUP` to render
to a string, or `RENDER-MARKUP-TO-STREAM` in order to write the
content to the given stream.
