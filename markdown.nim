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
    ( (?: ^ [ \t]* - .*$)+ |
      (?: ^ [ \t]* \* .*$)+ )
  """, {reExtended, reStudy, reMultiLine})
  taskList = re("""
    ( (?: ^ [ \t]*  - [ ]* \[[x ]\] .*$)+ |
      (?: ^ [ \t]* \* [ ]* \[[x ]\] .*$)+ )
  """, {reExtended, reStudy, reMultiLine})


proc parseMarkdown*(text: String): TMarkdown = text

proc findManipulateAndReplaceHash(
     md: var TMarkDown,
     hashToCodeMap: var Seq[Tuple[key, val: String]],
     findExpr: TRegex,
     transFunc: proc(v: String): String) =
  let matches = md.findAll(findExpr)
  for match in matches:
    var modifiedMatch = transFunc(match)
    let hash = sha1(modifiedMatch).toHex
    md = md.replace(match, hash)
    hashToCodeMap.add((match, hash))

proc toHtml*(md: TMarkdown): String =
  var md = md
  var hashToCodeMap: Seq[Tuple[key, val: String]] = @[]

  for rule in [blockCode, inlineCode]:
    findManipulateAndReplaceHash(md, hashToCodeMap, rule, proc(s: String): String = s)

  findManipulateAndReplaceHash(md, hashToCodeMap, indentCode,
    proc(s: String): String = (
      s.replace(indentCodeLeadingWhitespace, "")
    ))

  for match in md.findAll(blockQuote):
    var strippedMatch = match.replace(blockQuoteLeadingSymbol)
    md = md.replace(match, "<blockquote>\n" & strippedMatch & "\n</blockquote>")

  md = md.parallelReplace([url, link, image, italics, bold, strikethrough])

  for match in md.findAll(enumeratedList):
    var res = "<ol>\n"
    for line in match.splitLines:
      res.add(
        line.replacef(enumeratedListLine, "<li>$1</li>\n")
        )
    res.add("</ol>")
    md = md.replace(match, res)



  return md

echo parseMarkdown("""
 1. ~~23~~
 2. 32
 3. 23
  """).toHtml()