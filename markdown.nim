import re, sha1, strutils

type
  TMarkdown* = String

let
  url = (re"""(?<!href="|src=")(https?:\/\/  (?:(?!\s) .)++)""",
         """<a href="$1">$1</a>""")
  link = (re""" \[ ([^\]]+) \]  \( ([^)]+) \) """,
         """<a href="$1">$2</a>""")
  image = (re"""  !\[ ([^\]]*) \]  \( ([^)]+) \) """,
         """<img src="$1" alt="$2">""")
  italics = (re"""
    \b _ ([\S\s]+?) _ \b |
    \b \* ([\S\s]+?) \* \b
    """,
    """<i>$1</i>"""
    )
  bold = (re"""
    \b __ ([\S\s]+?) __ \b |
    \b \*\* ([\S\s]+?) \*\* \b
    """,
    """<b>$1</b>""")
  strikethrough = (re""" ~~ ( (?:(?!~~) .)++ ) ~~ """,
    """<del>$1</del>""")

  # Replaced with SHA hash
  blockCode = re(""" ^```(?:.*$) ([\n\r]|.)+ ^``` """,
                 {reExtended, reStudy, reMultiLine})
  indentCode = re"""  # Includes trailing newline
    ( (?:
      (?:^|(?<=[\n\r]))  # Beginning of input or newline
      (?:[ ]{4}|\t) .*
      (?:$|[\n\r])       # End of input or newline
    )+ )
  """
  indentCodeLeadingWhitespace = re"""^[ ]{4}|\t"""
  inlineCode = re"""
    ``` ((?:(?!```) .)++) ``` |
    `` ((?:(?!``) .)++) ``    |
    ` ((?:(?!`) .)++) `
  """
  blockQuote = re"""  # Includes trailing newline
    ( (?:
      (?:^|(?<=[\n\r]))  # Beginning of input or newline
      [ ]* > .*
      (?:$|[\n\r])       # End of input or newline
    )+ )
  """
  blockQuoteLeadingSymbol = re"""^>[ ]*"""

  # Require further processing
  enumeratedList = re("""( (?:
    (?:^|(?<=[\n\r]))
    [ \t]* \d+\. .*
    (?:[\n\r]|$)
    )+ )""", {reExtended, reStudy, reMultiLine})
  enumeratedListLine = re""" ^ [ \t]* \d+\.[ \t]* (.*)$ """
  itemizedList = re("""
    ( (?:
      (?:^|(?<=[\n\r]))
      [ \t]* [-*] .*
      (?:[\n\r]|$)
    )+ )
  """, {reExtended, reStudy, reMultiLine})
  taskListLine = re""" ^ [ \t]* [-*] [ \t]* \[(.?)\] [ \t]* (.*)$ """
  itemizedListLine = re""" ^ [ \t]* [-*] [ \t]* (.*)$ """

proc parseMarkdown*(text: String): TMarkdown = text

proc findManipulateAndReplaceHash(
     md: var TMarkDown,
     hashToCodeMap: var Seq[Tuple[pattern: TRegex, repl: String]],
     findExpr: TRegex,
     transFunc: proc(v: String): String) =
  let matches = md.findAll(findExpr)
  for match in matches:
    var modifiedMatch = transFunc(match)
    let hash = sha1(modifiedMatch).toHex
    md = md.replace(match, hash)
    hashToCodeMap.add((re(hash), modifiedMatch))

proc toHtml*(md: TMarkdown): String =
  var md = md
  var hashToCodeMap: Seq[Tuple[pattern: TRegex, repl: String]] = @[]
  var hashToInlineCodeMap: Seq[Tuple[pattern: TRegex, repl: String]] = @[]

  findManipulateAndReplaceHash(md, hashToInlineCodeMap, inlineCode,
    proc(s: String): String = "<code>" & s & "</code>")
  findManipulateAndReplaceHash(md, hashToCodeMap, blockCode,
    proc(s: String): String = "<pre><code>\n" & s & "\n</code></pre>\n")
  findManipulateAndReplaceHash(md, hashToCodeMap, indentCode,
    proc(s: String): String = (
      var res = s.replace(indentCodeLeadingWhitespace, "");
      "<pre><code>\n" & res & "\n</code></pre>\n"
    ))

  for match in md.findAll(blockQuote):
    var strippedMatch = match.replace(blockQuoteLeadingSymbol)
    md = md.replace(match, "<blockquote>\n" & strippedMatch & "\n</blockquote>")

  md = md.parallelReplace([url, link, image, italics, bold, strikethrough])

  # XXX nested lists

  for match in md.findAll(enumeratedList):
    var res = "<ol>\n"
    for line in match.splitLines:
      res.add(
        line.replacef(enumeratedListLine, "<li>$1</li>\n")
        )
    res.add("</ol>")
    md = md.replace(match, res)

  for match in md.findAll(itemizedList):
    var res = "<ul>\n"
    for line in match.splitLines:
      res.add("<li>")
      if line =~ taskListLine:
        res.add("""<input type="checkbox" disabled="">""")
        if matches[0].len > 0 and matches[0][0] in {'x', 'X'}:
          res.add(""" checked=""/>""" & matches[1])
        else:
          res.add("""/>""" & matches[1])
      else:
        res.add(line.replacef(itemizedListLine, "$1"))
      res.add("</li>\n")
    res.add("</ul>\n")

    md = md.replace(match, res)

  md = md.parallelReplace(hashToCodeMap)
  md = md.parallelReplace(hashToInlineCodeMap)

  return md

echo parseMarkdown("""
 - [] ~~23~~
 - 32
 - [x] `23`
  """).toHtml()