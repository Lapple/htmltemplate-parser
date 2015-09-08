# HTMLTemplate Parser

![TravisCI](https://travis-ci.org/Lapple/htmltemplate-parser.svg)

[HTML::Template](http://search.cpan.org/~samtregar/HTML-Template/Template.pm)-like
templating language parser, that can parse a pragmatic subset of HTML/TMPL tag
combinations (see [#1](https://github.com/Lapple/htmltemplate-parser/issues/1))
and Perl expressions.

## Installation

    npm install htmltemplate-parser

## CLI

```bash
$ htmltemplate-parser --help
Usage: htmltemplate-parser [options]

Options:
  --path         file to parse                               [string] [required]
  --pretty       output with colors                                    [boolean]
  --ignore-html  treat HTML tags as text                               [boolean]
```

## JavaScript API

### `parse(string, options)` method

Parse the supplied template string and return the corresponding AST. Available
options:

- `ignoreHTMLTags`, (default `false`) – do not attempt to parse HTML tags, treat them as text,
- `reducePositionLookups`, (default `false`) – do not calculate line, column and offset for most nodes,
this would speedup parsing of large files.

### Example usage

```js
var fs = require('fs');
var inspect = require('util').inspect;

var parser = require('htmltemplate-parser');

var tmpl = fs.readFileSync('./example.inc', 'utf8');
var ast = parser.parse(tmpl);

console.log(inspect(ast, { colors: true, depth: Infinity }));
```
