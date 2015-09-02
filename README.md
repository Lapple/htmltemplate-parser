# HTMLTemplate Parser

![TravisCI](https://travis-ci.org/Lapple/htmltemplate-parser.svg)

[HTML::Template](http://search.cpan.org/~samtregar/HTML-Template/Template.pm)-like templating language parser, that only attempts to parse `TMPL_*` tags, treating surrounding HTML as plain text.

## Installation

    npm install htmltemplate-parser

## CLI

```bash
./bin/parse --path test/html_tags/template.tmpl
```

## API

```js
var fs = require('fs');
var inspect = require('util').inspect;

var parser = require('htmltemplate-parser');

var tmpl = fs.readFileSync('./example.inc', 'utf8');
var ast = parser.parse(tmpl);

console.log(inspect(ast, { colors: true, depth: Infinity }));
```
