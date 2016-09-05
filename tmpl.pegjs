{
  function join(s) {
    return s.join("");
  }

  function token(object, location) {
    var preventPositionCalculation = (
      options.reducePositionLookups &&
      (
        object.type === BLOCK_TYPES.TEXT ||
        object.type === BLOCK_TYPES.CONDITION_BRANCH ||
        object.type === BLOCK_TYPES.ALTERNATE_CONDITION_BRANCH ||
        object.type === ATTRIBUTE_TYPES.EXPRESSION ||
        object.type === ATTRIBUTE_TYPES.PAIR ||
        object.type === ATTRIBUTE_TYPES.SINGLE ||
        object.type === EXPRESSION_TOKENS.IDENTIFIER ||
        object.type === EXPRESSION_TOKENS.FUNCTION_IDENTIFIER ||
        object.type === EXPRESSION_TOKENS.LITERAL ||
        object.type === EXPRESSION_TYPES.UNARY ||
        object.type === EXPRESSION_TYPES.BINARY ||
        object.type === EXPRESSION_TYPES.TERNARY ||
        object.type === EXPRESSION_TYPES.MEMBER ||
        object.type === EXPRESSION_TYPES.CALL
      )
    );

    if (!preventPositionCalculation) {
      var l = location().start;

      object.position = {
        line: l.line,
        column: l.column
      };
    }

    return object;
  }

  function isTemplateTag(name) {
    return name.indexOf('TMPL_') === 0;
  }

  function withEntities(obj, stringEntities) {
    if (options.collectStringEntities) {
      obj.stringEntities = obj.stringEntities || {};

      for (var key in stringEntities) {
        obj.stringEntities[key] = stringEntities[key];
      }

    }
    return obj;
  }

  function buildTree(first, rest, builder) {
    var result = first, i;

    for (i = 0; i < rest.length; i++) {
      result = builder(result, rest[i]);
    }

    return result;
  }

  function buildBinaryExpression(first, rest) {
    return buildTree(first, rest, function(result, element) {
      return withEntities({
        type: EXPRESSION_TYPES.BINARY,
        operator: element[1],
        left: result,
        right: element[3]
      }, {
        before: element[0],
        after: element[2]
      });
    });
  }

  function extractThird(list) {
    return list[3];
  }

  var BLOCK_TYPES = {
    COMMENT: "Comment",
    TAG: "Tag",
    HTML_TAG: "HTMLTag",
    TEXT: "Text",
    CONDITION: "Condition",
    CONDITION_BRANCH: "ConditionBranch",
    ALTERNATE_CONDITION_BRANCH: "AlternateConditionBranch",
    INVALID_TAG: "InvalidTag",
    CONDITIONAL_WRAPPER_TAG: "ConditionalWrapperTag"
  };

  var ATTRIBUTE_TYPES = {
    EXPRESSION: "Expression",
    PAIR: "PairAttribute",
    SINGLE: "SingleAttribute",
    CONDITIONAL: "ConditionalAttribute"
  };

  var EXPRESSION_TOKENS = {
    IDENTIFIER: "Identifier",
    FUNCTION_IDENTIFIER: "FunctionIdentifier",
    LITERAL: "Literal"
  };

  var EXPRESSION_TYPES = {
    UNARY: "UnaryExpression",
    BINARY: "BinaryExpression",
    TERNARY: "ConditionalExpression",
    MEMBER: "MemberExpression",
    CALL: "CallExpression"
  };

  var RESERVED_OPERATOR_NAMES = [
    "and",
    "eq",
    "ge",
    "gt",
    "le",
    "lt",
    "ne",
    "or"
  ];

  function SyntaxError(message, location) {
    var l = location().start;

    this.name = "SyntaxError";
    this.message = message;
    this.line = l.line;
    this.column = l.column;
    this.offset = l.offset;
    this.expected = null;
    this.found = null;
  }

  SyntaxError.prototype = Error.prototype;
}

Content = (Comment / ConditionalWrapperTag / ConditionalTag / BlockTag / SingleTag / InvalidTag / Text)*

Comment
  = CommentTag
  / SingleLineComment

SingleTag =
  before:OpeningBracket
  name:$((SingleTMPLTagName / SingleHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  attributes:(a:HTMLAttribute* ! { return isTemplateTag(name); } { return a; } / TMPLAttribute*)
  after:ClosingBracket
  {
    return token(withEntities({
      type: isTemplateTag(name) ? BLOCK_TYPES.TAG : BLOCK_TYPES.HTML_TAG,
      name: name,
      attributes: attributes
    }, { before: before, after: after }), location);
  }

BlockTag = start:StartTag content:Content end:EndTag {
  if (start.name != end.name) {
    throw new SyntaxError("Expected a </" + start.name + "> but </" + end.name + "> found.", location);
  }

  return token(withEntities({
    type: isTemplateTag(start.name) ? BLOCK_TYPES.TAG : BLOCK_TYPES.HTML_TAG,
    name: start.name,
    attributes: start.attributes,
    content: content
  }, { start: start, end: end }), location);
}

ConditionalTag = start:ConditionStartTag content:Content elsif:ElsIfTag* otherwise:ElseTag? end:ConditionEndTag {
  if (start.name != end.name) {
    throw new SyntaxError("Expected a </" + start.name + "> but </" + end.name + "> found.", location);
  }

  var primaryCondition = token({
    type: BLOCK_TYPES.CONDITION_BRANCH,
    condition: start.condition,
    content: content
  }, location);

  var conditions = [primaryCondition].concat(elsif);

  return token({
    type: BLOCK_TYPES.CONDITION,
    name: start.name,
    conditions: conditions,
    otherwise: otherwise
  }, location);
}

InvalidTag = before:(OpeningEndBracket / OpeningBracket) name:UnknownTagName attributes:TMPLAttribute* after:ClosingBracket {
  return token(withEntities({
    type: BLOCK_TYPES.INVALID_TAG,
    name: name,
    attributes: attributes
  }, { before: before, after: after }), location);
}

ElsIfTag = condition:ElsIfStartTag content:Content {
  return token({
    type: BLOCK_TYPES.CONDITION_BRANCH,
    condition: condition,
    content: content
  }, location);
}

ElseTag = ElseStartTag content:Content {
  return token({
    type: BLOCK_TYPES.ALTERNATE_CONDITION_BRANCH,
    content: content
  }, location);
}

NonText
  = Comment
  / SingleTag
  / StartTag
  / EndTag
  / ConditionStartTag
  / ElsIfStartTag
  / ElseStartTag
  / ConditionEndTag
  / InvalidTag

Text = text:$(!NonText SourceCharacter)+ {
  return token({
    type: BLOCK_TYPES.TEXT,
    content: text
  }, location);
}

ConditionalWrapperTag =
  openCondition:ConditionStartTag __
  openWrapper:StartTag __
  ConditionEndTag
  content:Content
  closeCondition:ConditionStartTag __
  closeWrapper: EndTag __
  ConditionEndTag
  & {
    var areConditionValuesEqual = (
      typeof openCondition.condition.value === 'string' ?
        openCondition.condition.value === closeCondition.condition.value :
        openCondition.condition.value.value === openCondition.condition.value.value
    );

    return (
      openCondition.name === closeCondition.name &&
      openCondition.condition.type === closeCondition.condition.type &&
      openCondition.condition.name === closeCondition.condition.name &&
      areConditionValuesEqual &&
      openWrapper.name === closeWrapper.name
    );
  }
  {
    var condition = token({
      type: BLOCK_TYPES.CONDITION_BRANCH,
      condition: openCondition.condition,
      content: [
        token({
          type: BLOCK_TYPES.HTML_TAG,
          name: openWrapper.name,
          attributes: openWrapper.attributes
        }, location)
      ]
    }, location);

    return token({
      type: BLOCK_TYPES.CONDITIONAL_WRAPPER_TAG,
      name: openCondition.name,
      conditions: [condition],
      content: content
    }, location);
  }

StartTag =
  before:OpeningBracket
  name:$((BlockTMPLTagName / BlockHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  attributes:(a:HTMLAttribute* ! { return isTemplateTag(name); } { return a; } / TMPLAttribute*)
  after:ClosingBracket
  {
    return withEntities({
      name: name,
      attributes: attributes
    }, { before: before, after: after });
  }

// FIXME: Not capturing attributes on end tag for now.
EndTag =
  before:OpeningEndBracket
  name:$((BlockTMPLTagName / BlockHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  (a:HTMLAttribute* ! { return isTemplateTag(name); } { return a; } / TMPLAttribute*)
  after:ClosingBracket
  {
    return withEntities({ name: name }, { before: before, after: after });
  }

ConditionStartTag = OpeningBracket name:ConditionalTagName condition:TMPLAttribute* ClosingBracket {
  return {
    name: name,
    condition: condition[0] || null
  };
}

ElsIfStartTag = OpeningBracket ElsIfTagName condition:TMPLAttribute* ClosingBracket {
  return condition[0] || null;
}

ElseStartTag
  = OpeningBracket ElseTagName ClosingBracket

ConditionEndTag = OpeningEndBracket name:ConditionalTagName ClosingBracket {
  return { name: name };
}

SingleLineComment = char:CommentStart c:$(!LineTerminator SourceCharacter)* {
  return token(withEntities({
    type: BLOCK_TYPES.COMMENT,
    content: c
  }, { char: char }), location);
}

CommentTag = CommentTagStart content:$(!CommentTagEnd SourceCharacter)* CommentTagEnd {
  return token({
    type: BLOCK_TYPES.COMMENT,
    content: content
  }, location);
}

TMPLAttribute
  = WhiteSpace+ attrs:(AttributeWithValue / AttributeWithoutValue) { return attrs; }
  // Expressions don't require whitespace to be separated from tag names.
  / __ expression:(PerlExpressionLiteral / InvalidPerlExpressionLiteral) { return expression; }
  / __ comment:SingleLineComment { return comment; }

PerlExpressionLiteral =
  before:PerlExpressionStart
  e:(
    expression:PerlExpression {
      return {
        expression: expression,
        text: text()
      };
    }
  )
  after:PerlExpressionEnd
  {
    return token(
      withEntities({
        type: ATTRIBUTE_TYPES.EXPRESSION,
        content: e.expression,
        value: e.text
      }, {
        before: before,
        after: after
      }),
      location
    );
  }

InvalidPerlExpressionLiteral = PerlExpressionStart (!PerlExpressionEnd SourceCharacter)* PerlExpressionEnd {
  throw new SyntaxError('Illegal expression.', location);
}

// FIXME: Quote character escaping inside of an expression does not work at the
// moment.
PerlExpressionString
  = e:(
      "\"" __ expression:(!"\"" PerlExpression) __ "\"" { return { expression: expression[1], text: text() }; }
    / "'" __ expression:(!"'" PerlExpression) __ "'" { return { expression: expression[1], text: text() }; }
    )
    {
      return token({
        type: ATTRIBUTE_TYPES.EXPRESSION,
        content: e.expression,
        value: e.text
      }, location);
    }

AttributeWithValue =
  name:AttributeToken "="
  value:(
    // See PR #6, need to support `<TMPL_VAR EXPR="...">` syntax.
      e:PerlExpressionString & { return name === "EXPR"; } { return e; }
    / PerlExpressionLiteral
    / literal:(StringLiteral / NumericLiteral)
    / t:AttributeToken {
        return token({
          type: EXPRESSION_TOKENS.IDENTIFIER,
          name: t
        }, location);
      }
  )
  {
    return token({
      type: ATTRIBUTE_TYPES.PAIR,
      name: name,
      value: value
    }, location);
  }

// Predicate takes care of not matching self closing bracket in single HTML tags,
// e.g. `<input type="text" />`.
AttributeWithoutValue = name:(AttributeToken / StringLiteral) & { return name !== '/'; } {
  return token({
    type: ATTRIBUTE_TYPES.SINGLE,
    name: name,
    value: null
  }, location);
}

AttributeToken = n:$[a-zA-Z0-9\-_/:\.{}\$]+ {
  if (n.indexOf("$") > 0) {
    throw new SyntaxError("Unexpected $ in attribute name.", location);
  }

  return n;
}

WhitespaceSeparatedHTMLAttributes = first:PlainHTMLAttribute rest:(WhiteSpace+ PlainHTMLAttribute)* {
  var attrs = [first];

  for (var i = 0, len = rest.length; i < len; i += 1) {
    attrs.push(rest[i][1]);
  }

  return attrs;
}

HTMLAttribute
  = WhiteSpace+ attr:PlainHTMLAttribute {
      return attr;
    }
  / __ attr:ConditionalHTMLAttributes {
      return attr;
    }

PlainHTMLAttribute
  = HTMLAttributeWithValue
  / HTMLAttributeWithoutValue

HTMLAttributeWithValue = name:HTMLAttributeToken "=" value:(HTMLAttributeToken / QuotedContentString) {
  if (typeof value === 'string') {
    return token({
      type: ATTRIBUTE_TYPES.PAIR,
      name: name,
      value: value
    }, location);
  } else {
    return token({
      type: ATTRIBUTE_TYPES.PAIR,
      name: name,
      value: null,
      content: value
    }, location);
  }
}

HTMLAttributeWithoutValue = name:HTMLAttributeToken {
  return token({
    type: ATTRIBUTE_TYPES.SINGLE,
    name: name,
    value: null
  }, location);
}

// FIXME: Check the spec regarding this regexp.
HTMLAttributeToken = n:$[a-zA-Z0-9-]+ {
  return n;
}

ConditionalHTMLAttributes =
  start:ConditionStartTag __
  attrs:WhitespaceSeparatedHTMLAttributes
  elsif:(
    __ condition:ElsIfStartTag __ attrs:WhitespaceSeparatedHTMLAttributes {
      return token({
        type: BLOCK_TYPES.CONDITION_BRANCH,
        condition: condition,
        content: attrs
      }, location);
    }
  )*
  otherwise:(
    __ ElseStartTag __ attrs:WhitespaceSeparatedHTMLAttributes {
      return attrs;
    }
  )?
  __ end:ConditionEndTag
  {
    if (start.name != end.name) {
      throw new SyntaxError("Expected a </" + start.name + "> but </" + end.name + "> found.", location);
    }

    var primaryCondition = token({
      type: BLOCK_TYPES.CONDITION_BRANCH,
      condition: start.condition,
      content: attrs
    }, location);

    var conditions = [primaryCondition].concat(elsif);

    return token({
      type: ATTRIBUTE_TYPES.CONDITIONAL,
      name: start.name,
      conditions: conditions,
      otherwise: otherwise
    }, location);
  }

QuotedContentString
  = SingleQuotedContentString
  / DoubleQuotedContentString

SingleQuotedContentString = "'" content:(Comment / ConditionalTag / BlockTag / SingleTag / InvalidTag / SingleQuotedText)* "'" {
  if (content.length === 1 && content[0].type === BLOCK_TYPES.TEXT) {
    return content[0].content;
  }

  return content;
}

SingleQuotedText = text:$(!NonText (SingleStringCharacter / LineTerminator))+ {
  return token({
    type: BLOCK_TYPES.TEXT,
    content: text
  }, location);
}

DoubleQuotedContentString = "\"" content:(Comment / ConditionalTag / BlockTag / SingleTag / InvalidTag / DoubleQuotedText)* "\"" {
  if (content.length === 1 && content[0].type === BLOCK_TYPES.TEXT) {
    return content[0].content;
  }

  return content;
}

DoubleQuotedText = text:$(!NonText (DoubleStringCharacter / LineTerminator))+ {
  return token({
    type: BLOCK_TYPES.TEXT,
    content: text
  }, location);
}

// Operator precedence:
//  > **
//  >  ! ~ + -
//  >  =~ !~
//  <  * / %
//  <  + - .
//  -  < > <= >= lt gt le ge
//  -  == != <=> eq ne ~~
//  <  &&
//  <  ||
//  >  not
//  <  and
//  <  or xor
PerlExpression
  = test:LogicalStringOrExpression __ "?" __ consequent:PerlExpression __ ":" __ alternate:PerlExpression {
      return {
        type: EXPRESSION_TYPES.TERNARY,
        test: test,
        consequent: consequent,
        alternate: alternate
      };
    }
  / LogicalStringOrExpression

LogicalStringOrExpression = first:LogicalStringAndExpression rest:(__ ("xor" / "or") __ LogicalStringAndExpression)* {
  return buildBinaryExpression(first, rest);
}

LogicalStringAndExpression = first:UnaryStringNotExpression rest:(__ "and" __ UnaryStringNotExpression)* {
  return buildBinaryExpression(first, rest);
}

UnaryStringNotExpression
  = operator:"not" whitespace:__ argument:UnaryStringNotExpression {
      return withEntities({
        type: EXPRESSION_TYPES.UNARY,
        operator: operator,
        argument: argument,
        prefix: true
      }, {
        whitespace: whitespace
      });
    }
  / LogicalSymbolicOrExpression

LogicalSymbolicOrExpression = first:LogicalSymbolicAndExpression rest:(__ LogicalSymbolicOperator __ LogicalSymbolicAndExpression)* {
  return buildBinaryExpression(first, rest);
}

LogicalSymbolicAndExpression = first:EqualityExpression rest:(__ "&&" __ EqualityExpression)* {
  return buildBinaryExpression(first, rest);
}

EqualityExpression = first:ComparisonExpression rest:(__ EqualityOperator __ ComparisonExpression)* {
  return buildBinaryExpression(first, rest);
}

ComparisonExpression = first:AdditiveExpression rest:(__ ComparisonOperator __ AdditiveExpression)* {
  return buildBinaryExpression(first, rest);
}

AdditiveExpression = first:MultiplicativeExpression rest:(__ AdditiveOperator __ MultiplicativeExpression)* {
  return buildBinaryExpression(first, rest);
}

MultiplicativeExpression = first:MatchExpression rest:(__ MultiplicativeOperator __ MatchExpression)* {
  return buildBinaryExpression(first, rest);
}

MatchExpression = first:UnarySymbolicExpression rest:(__ MatchOperator __ UnarySymbolicExpression)* {
  return buildBinaryExpression(first, rest);
}

UnarySymbolicExpression
  = operator:UnarySymbolicOperator whitespace:__ argument:UnarySymbolicExpression {
      return withEntities({
        type: EXPRESSION_TYPES.UNARY,
        operator: operator,
        argument: argument,
        prefix: true
      }, {
        whitespace: whitespace
      });
    }
  / ExponentiationExpression

ExponentiationExpression
  = left:CallExpression __ operator:"**" __ right:ExponentiationExpression {
      return {
        type: EXPRESSION_TYPES.BINARY,
        operator: operator,
        left: left,
        right: right
      };
    }
  / CallExpression

CallExpression
  = callee:PerlFunctionIdentifier __ args:Arguments {
      return {
        type: EXPRESSION_TYPES.CALL,
        callee: callee,
        arguments: args
      };
    }
  / MemberExpression

MemberExpression
  = first:PrimaryExpressionWithComments
    rest:(
        __ ("->" __)? "{" __ property:(!NumericLiteral PerlExpression) __ "}" {
          return {
            property: property[1],
            computed: true
          };
        }
      / __ ("->" __)? "[" __ property:(!NumericLiteral PerlExpression) __ "]" {
          return {
            property: property[1],
            computed: true
          };
        }
      / __ ("->" __)? "{" __ value:PerlPropertyName __ "}" {
          var number = +value;

          return {
            property: token({
              type: EXPRESSION_TOKENS.LITERAL,
              value: isNaN(number) ? value : number
            }, location),
            computed: true
          };
        }
      / __ "->" __ value:PerlPropertyName __ {
          return {
            property: token({
              type: EXPRESSION_TOKENS.LITERAL,
              value: value
            }, location),
            computed: true
          };
        }
      / __ ("->" __)? "[" __ literal:NumericLiteral __ "]" {
          return {
            property: token({
              type: EXPRESSION_TOKENS.LITERAL,
              value: literal.value
            }, location),
            computed: true
          };
        }
    )*
    {
      return buildTree(first, rest, function(result, element) {
        return {
          type: EXPRESSION_TYPES.MEMBER,
          object: result,
          property: element.property,
          computed: element.computed
        };
      });
    }

// This is will add support for single-line comments inside of an expression,
// for now just stripping them away.
PrimaryExpressionWithComments
  = (__ SingleLineComment LineTerminator __)*
    e:PrimaryExpression
    (__ SingleLineComment LineTerminator __)*
    {
      return e;
    }

PrimaryExpression
  = PerlIdentifier
  / PerlLiteral
  / beforeOuter:$("(" __) e:PerlExpression afterOuter:$(__ ")") {
    return withEntities(e, { beforeOuter: beforeOuter, afterOuter: afterOuter });
  }

PerlIdentifier
  = name:$(
      [@%$]+ PerlIdentifierName
    / "__counter__"
    / "__first__"
    / "__last__"
    / "__even__"
    / "__odd__"
    / "last"
    / "next"
  )
  {
    return token({
      type: EXPRESSION_TOKENS.IDENTIFIER,
      name: name
    }, location);
  }

PerlFunctionIdentifier =
  name:PerlIdentifierName
  & {
    return RESERVED_OPERATOR_NAMES.indexOf(name) === -1;
  }
  {
    return token({
      type: EXPRESSION_TOKENS.FUNCTION_IDENTIFIER,
      name: name
    }, location);
  }

PerlIdentifierName
  = $([a-zA-Z_]+ [a-zA-Z0-9_/]*)

PerlPropertyName
  = $[a-zA-Z0-9_]+

PerlLiteral
  = PrimitivePerlLiteral
  / RegularExpressionLiteral

PrimitivePerlLiteral
  = StringLiteral / NumericLiteral

Arguments
  = single:CallExpression {
      return [single];
    }
  / "(" __ args:(ArgumentList __)? ")" {
      return (args && args[0]) ? args[0] : [];
    }

ArgumentList
  = first:PerlExpression rest:(__ "," __ PerlExpression)* {
      return [first].concat(
        rest.map(extractThird)
      );
    }

UnarySymbolicOperator
  = $("+" !"=")
  / $("-" !"=")
  / "~"
  / "!"

MatchOperator
  = "=~"
  / "!~"

MultiplicativeOperator
  = $("*" ![*=])
  / $("/" ![/=])
  / $("%" !"=")

AdditiveOperator
  = $("+" ![+=])
  / $("-" ![-=])
  / $("." !"=")

ComparisonOperator
  = ">="
  / ">"
  / "<="
  / "<"
  / "lt"
  / "gt"
  / "le"
  / "ge"

EqualityOperator
  = "=="
  / "!="
  / "<=>"
  / "eq"
  / "ne"
  / "~~"

LogicalSymbolicOperator
  = "||"
  / "//"

KnownTagName
  = BlockTMPLTagName
  / ConditionalTagName
  / ElsIfTagName
  / ElseTagName

UnknownTagName
  = $(!KnownTagName "TMPL_" TagNameCharacter+)

SingleTMPLTagName
  // The order here is important, longer tag name goes first.
  = "TMPL_INCLUDE"
  / "TMPL_VAR"
  / "TMPL_V"

BlockTMPLTagName
  = "TMPL_BLOCK"
  / "TMPL_FOR"
  / "TMPL_LOOP"
  / "TMPL_SETVAR"
  / "TMPL_WITH"
  / "TMPL_WS"
  / "TMPL_TEXT"

ConditionalTagName
  = "TMPL_IF"
  / "TMPL_UNLESS"

ElsIfTagName
  = "TMPL_ELSIF"

ElseTagName
  = "TMPL_ELSE"

CommentTagName
  = "TMPL_COMMENT"

BlockHTMLTagName =
  'abbr'
  / 'address'
  / 'article'
  / 'aside'
  / 'audio'
  / 'a'
  / 'bdi'
  / 'bdo'
  / 'big'
  / 'blockquote'
  / 'body'
  / 'button'
  / 'b'
  / 'canvas'
  / 'caption'
  / 'cite'
  / 'code'
  / 'colgroup'
  / 'datalist'
  / 'del'
  / 'details'
  / 'dd'
  / 'dfn'
  / 'div'
  / 'dl'
  / 'dt'
  / 'em'
  / 'fieldset'
  / 'figcaption'
  / 'figure'
  / 'footer'
  / 'form'
  / 'h1'
  / 'h2'
  / 'h3'
  / 'h4'
  / 'h5'
  / 'h6'
  / 'header'
  / 'head'
  / 'html'
  / 'hgroup'
  / 'iframe'
  / 'ins'
  / 'i'
  / 'kbd'
  / 'label'
  / 'legend'
  / 'li'
  / 'map'
  / 'mark'
  / 'menu'
  / 'meter'
  / 'nav'
  / 'noscript'
  / 'object'
  / 'ol'
  / 'optgroup'
  / 'option'
  / 'output'
  / 'pre'
  / 'progress'
  / 'p'
  / 'q'
  / 'rp'
  / 'rt'
  / 'ruby'
  / 'samp'
  / 'script'
  / 'section'
  / 'select'
  / 'small'
  / 'span'
  / 'strong'
  / 'style'
  / 'sub'
  / 'summary'
  / 'sup'
  / 's'
  / 'table'
  / 'tbody'
  / 'td'
  / 'textarea'
  / 'tfoot'
  / 'th'
  / 'thead'
  / 'time'
  / 'title'
  / 'tr'
  / 'ul'
  / 'u'
  / 'var'
  / 'video'

SingleHTMLTagName =
  'area'
  / 'base'
  / 'br'
  / 'col'
  / 'command'
  / 'embed'
  / 'hr'
  / 'input'
  / 'img'
  / 'keygen'
  / 'link'
  / 'meta'
  / 'param'
  / 'source'
  / 'track'
  / 'wbr'

WhiteSpaceControlStart "whitespace control character"
  = "-"
  / "~."
  / "~|"
  / "~"

WhiteSpaceControlEnd "whitespace control character"
  = "-"
  / ".~"
  / "|~"
  / "~"

CommentTagStart
  = OpeningBracket CommentTagName ClosingBracket

CommentTagEnd
  = OpeningEndBracket CommentTagName ClosingBracket

TagNameCharacter
  = [a-zA-Z_-]

StringLiteral "string"
  = value:SingleQuotedString {
      return token({
        type: EXPRESSION_TOKENS.LITERAL,
        value: value,
        raw: "'" + value + "'"
      }, location);
    }
  / value:DoubleQuotedString {
      return token({
        type: EXPRESSION_TOKENS.LITERAL,
        value: value,
        raw: '"' + value + '"'
      }, location);
    }

SingleQuotedString = "'" chars:SingleStringCharacter* "'" {
  return join(chars);
}

DoubleQuotedString = "\"" chars:DoubleStringCharacter* "\"" {
  return join(chars);
}

NumericLiteral "number"
  = DecimalIntegerLiteral "." DecimalDigit* ExponentPart? {
    return token({
      type: EXPRESSION_TOKENS.LITERAL,
      raw: parseFloat(text()),
      value: parseFloat(text())
    }, location);
  }
  / "." DecimalDigit+ ExponentPart? {
    return token({
      type: EXPRESSION_TOKENS.LITERAL,
      raw: parseFloat(text()),
      value: parseFloat(text())
    }, location);
  }
  / DecimalIntegerLiteral ExponentPart? {
    return token({
      type: EXPRESSION_TOKENS.LITERAL,
      raw: parseFloat(text()),
      value: parseFloat(text())
    }, location);
  }

DecimalIntegerLiteral
  = "0"
  / NonZeroDigit DecimalDigit*

DecimalDigit
  = [0-9]

NonZeroDigit
  = [1-9]

ExponentPart
  = ExponentIndicator SignedInteger

ExponentIndicator
  = "e"i

SignedInteger
  = [+-]? DecimalDigit+

RegularExpressionLiteral "regular expression"
  = operators:$RegularExpressionOperators "/" pattern:$RegularExpressionBody "/" flags:$RegularExpressionFlags {
      return token({
        type: EXPRESSION_TOKENS.LITERAL,
        regex: {
          pattern: pattern,
          flags: flags,
          operators: operators
        }
      }, location);
    }

RegularExpressionBody
  = RegularExpressionFirstChar RegularExpressionChar*

RegularExpressionFirstChar
  = ![*\\/[] RegularExpressionNonTerminator
  / RegularExpressionBackslashSequence
  / RegularExpressionClass

RegularExpressionChar
  = ![\\/[] RegularExpressionNonTerminator
  / RegularExpressionBackslashSequence
  / RegularExpressionClass

RegularExpressionBackslashSequence
  = "\\" RegularExpressionNonTerminator

RegularExpressionNonTerminator
  = !LineTerminator SourceCharacter

RegularExpressionClass
  = "[" RegularExpressionClassChar* "]"

RegularExpressionClassChar
  = ![\]\\] RegularExpressionNonTerminator
  / RegularExpressionBackslashSequence

RegularExpressionFlags
  = [a-z]*

RegularExpressionOperators
  = [a-z]*

WhiteSpace "whitespace"
  = "\t"
  / "\v"
  / "\f"
  / " "
  / "\u00A0"
  / "\uFEFF"
  / [\u0020\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]
  / LineTerminator

__ = $(WhiteSpace*)

CommentStart
  = "##"

SourceCharacter
  = .

LineTerminator "end of line"
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

OpeningBracket
  = $("<" WhiteSpaceControlStart? __)

OpeningEndBracket
  = $("<" WhiteSpaceControlStart? "/")

ClosingBracket
  = $(__ WhiteSpaceControlEnd? ("/>" / ">"))
  / !">" SourceCharacter+ {
    throw new SyntaxError("Expected a closing bracket.", location);
  }

PerlExpressionStart
  = $("[%" __)

PerlExpressionEnd
  = $(__ "%]")

SingleStringCharacter
  = !("'" / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" esc:CharacterEscapeSequence { return esc; }

DoubleStringCharacter
  = !("\"" / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" esc:CharacterEscapeSequence { return esc; }

CharacterEscapeSequence
  = SingleEscapeCharacter
  / NonEscapeCharacter

SingleEscapeCharacter
  = "'"
  / '"'
  / "\\"
  / "b"  { return "\b"; }
  / "f"  { return "\f"; }
  / "n"  { return "\n"; }
  / "r"  { return "\r"; }
  / "t"  { return "\t"; }
  / "v"  { return "\v"; }

NonEscapeCharacter
  = !(EscapeCharacter / LineTerminator) SourceCharacter { return text(); }

EscapeCharacter
  = SingleEscapeCharacter
  / DecimalDigit
  / "x"
  / "u"
