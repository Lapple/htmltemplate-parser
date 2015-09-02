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
        object.type === ATTRIBUTE_TYPES.SINGLE
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
    SINGLE: "SingleAttribute"
  };

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
  / FullLineComment
  / SingleLineComment

SingleTag =
  OpeningBracket
  name:$((SingleTMPLTagName / SingleHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  attributes:(a:HTMLAttributes* ! { return isTemplateTag(name); } { return a; } / TMPLAttributes*)
  ClosingBracket
  {
    return token({
      type: isTemplateTag(name) ? BLOCK_TYPES.TAG : BLOCK_TYPES.HTML_TAG,
      name: name,
      attributes: attributes
    }, location);
  }

BlockTag = start:StartTag content:Content end:EndTag {
  if (start.name != end) {
    throw new SyntaxError("Expected a </" + start.name + "> but </" + end + "> found.", location);
  }

  return token({
    type: isTemplateTag(start.name) ? BLOCK_TYPES.TAG : BLOCK_TYPES.HTML_TAG,
    name: start.name,
    attributes: start.attributes,
    content: content
  }, location);
}

ConditionalTag = start:ConditionStartTag content:Content elsif:ElsIfTag* otherwise:ElseTag? end:ConditionEndTag {
  if (start.name != end) {
    throw new SyntaxError("Expected a </" + start.name + "> but </" + end + "> found.", location);
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

InvalidTag = (OpeningEndBracket / OpeningBracket) name:UnknownTagName attributes:TMPLAttributes* ClosingBracket {
  return token({
    type: BLOCK_TYPES.INVALID_TAG,
    name: name,
    attributes: attributes
  }, location);
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
  openCondition:ConditionStartTag
  WhiteSpace*
  openWrapper:StartTag
  WhiteSpace*
  ConditionEndTag
  content:Content
  closeCondition:ConditionStartTag
  WhiteSpace*
  closeWrapper: EndTag
  WhiteSpace*
  ConditionEndTag
  & {
    return (
      openCondition.name === closeCondition.name &&
      openCondition.condition.type === closeCondition.condition.type &&
      openCondition.condition.name === closeCondition.condition.name &&
      openCondition.condition.value === closeCondition.condition.value &&
      openWrapper.name === closeWrapper
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
  OpeningBracket
  name:$((BlockTMPLTagName / BlockHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  attributes:(a:HTMLAttributes* ! { return isTemplateTag(name); } { return a; } / TMPLAttributes*)
  ClosingBracket
  {
    return {
      name: name,
      attributes: attributes
    };
  }

// FIXME: Not capturing attributes on end tag for now.
EndTag =
  OpeningEndBracket
  name:$((BlockTMPLTagName / BlockHTMLTagName ! { return options.ignoreHTMLTags; }) !TagNameCharacter+)
  // Matching either HTML attributes or TMPL attributes depending on tag name.
  (a:HTMLAttributes* ! { return isTemplateTag(name); } { return a; } / TMPLAttributes*)
  ClosingBracket
  {
    return name;
  }

ConditionStartTag = OpeningBracket name:ConditionalTagName condition:TMPLAttributes* ClosingBracket {
  return {
    name: name,
    condition: condition[0] || null
  };
}

ElsIfStartTag = OpeningBracket ElsIfTagName condition:TMPLAttributes* ClosingBracket {
  return condition[0] || null;
}

ElseStartTag
  = OpeningBracket ElseTagName ClosingBracket

ConditionEndTag = OpeningEndBracket name:ConditionalTagName ClosingBracket {
  return name;
}

SingleLineComment = CommentStart c:$(!LineTerminator SourceCharacter)* {
  return token({
    type: BLOCK_TYPES.COMMENT,
    content: c
  }, location);
}

FullLineComment = FullLineCommentStart c:$(!LineTerminator SourceCharacter)* {
  return token({
    type: BLOCK_TYPES.COMMENT,
    content: c
  }, location);
}

CommentTag = CommentTagStart content:$(!CommentTagEnd SourceCharacter)* CommentTagEnd {
  return token({
    type: BLOCK_TYPES.COMMENT,
    content: content
  }, location);
}

TMPLAttributes
  = WhiteSpace+ attrs:(AttributeWithValue / AttributeWithoutValue) { return attrs; }
  // Expressions don't require whitespace to be separated from tag names.
  / WhiteSpace* expression:PerlExpression { return expression; }

PerlExpression = PerlExpressionStart expression:$(!PerlExpressionEnd SourceCharacter)* PerlExpressionEnd {
  return token({
    type: ATTRIBUTE_TYPES.EXPRESSION,
    value: expression
  }, location);
}

AttributeWithValue = name:AttributeToken "=" value:(AttributeToken / PerlExpression / QuotedString) {
  return token({
    type: ATTRIBUTE_TYPES.PAIR,
    name: name,
    value: value
  }, location);
}

// Predicate takes care of not matching self closing bracket in single HTML tags,
// e.g. `<input type="text" />`.
AttributeWithoutValue = name:(AttributeToken / QuotedString) & { return name !== '/'; } {
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

HTMLAttributes
  = WhiteSpace+ attrs:(HTMLAttributeWithValue / HTMLAttributeWithoutValue) { return attrs; }

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

QuotedString
  = SingleQuotedString
  / DoubleQuotedString

SingleQuotedString = "'" chars:SingleStringCharacter* "'" {
  return join(chars);
}

DoubleQuotedString = "\"" chars:DoubleStringCharacter* "\"" {
  return join(chars);
}

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
  / 'article'
  / 'a'
  / 'big'
  / 'blockquote'
  / 'body'
  / 'button'
  / 'b'
  / 'code'
  / 'colgroup'
  / 'col'
  / 'dd'
  / 'div'
  / 'em'
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
  / 'head'
  / 'header'
  / 'hgroup'
  / 'html'
  / 'i'
  / 'label'
  / 'legend'
  / 'li'
  / 'main'
  / 'nav'
  / 'ol'
  / 'option'
  / 'pre'
  / 'p'
  / 'q'
  / 'section'
  / 'select'
  / 'small'
  / 'span'
  / 'strong'
  / 'style'
  / 'sub'
  / 'sup'
  / 'table'
  / 'tbody'
  / 'td'
  / 'textarea'
  / 'tfoot'
  / 'th'
  / 'thead'
  / 'title'
  / 'tr'
  / 'ul'
  / 'u'

SingleHTMLTagName =
  'base'
  / 'br'
  / 'dl'
  / 'dt'
  / 'hr'
  / 'input'
  / 'img'
  / 'link'
  / 'meta'

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

WhiteSpace "whitespace"
  = "\t"
  / "\v"
  / "\f"
  / " "
  / "\u00A0"
  / "\uFEFF"
  / [\u0020\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]
  / LineTerminator

FullLineCommentStart
  = LineTerminator (!CommentStart "#")

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
  = "<" (WhiteSpaceControlStart WhiteSpace*)?

OpeningEndBracket
  = "<" WhiteSpaceControlStart? "/"

ClosingBracket
  = WhiteSpace* WhiteSpaceControlEnd? ("/>" / ">")
  / !">" SourceCharacter+ {
    throw new SyntaxError("Expected a closing bracket.", location);
  }

PerlExpressionStart
  = "[%" WhiteSpace*

PerlExpressionEnd
  = WhiteSpace* "%]"

SingleStringCharacter
  = !("'" / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" esc:SingleEscapeCharacter { return esc; }

DoubleStringCharacter
  = !("\"" / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" esc:SingleEscapeCharacter { return esc; }

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
