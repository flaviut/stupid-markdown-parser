import re, sha1, strutils, future

type
  Markdown* = string

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
  headers = lc[(re("""^ \#{""" & $x & """}    [[:blank:]]*+ ([^#\n\r]++) $""",
                {reExtended, reStudy, reMultiLine}),
               "<h" & $x & ">$1</h" & $x & ">") | (x <- 1..6), tuple[r: Regex, replacement: string]]

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

proc parseMarkdown*(text: string): Markdown = text

proc findManipulateAndReplaceHash(
     md: var Markdown,
     hashToCodeMap: var seq[tuple[pattern: Regex, repl: string]],
     findExpr: Regex,
     transFunc: proc(v: string): string) =
  let matches = md.findAll(findExpr)
  for match in matches:
    var modifiedMatch = transFunc(match)
    let hash = sha1(modifiedMatch).toHex
    md = md.replace(match, hash)
    hashToCodeMap.add((re(hash), modifiedMatch))

proc toHtml*(md: Markdown): string =
  var md = md
  var hashToCodeMap: seq[tuple[pattern: Regex, repl: string]] = @[]
  var hashToInlineCodeMap: seq[tuple[pattern: Regex, repl: string]] = @[]

  findManipulateAndReplaceHash(md, hashToInlineCodeMap, inlineCode,
    proc(s: string): string = "<code>" & s & "</code>")
  findManipulateAndReplaceHash(md, hashToCodeMap, blockCode,
    proc(s: string): string = "<pre><code>\n" &
                              s.splitLines[1..^2].join("n") &
                              "\n</code></pre>\n")
  findManipulateAndReplaceHash(md, hashToCodeMap, indentCode,
    proc(s: string): string = (
      var res = s.replace(indentCodeLeadingWhitespace, "");
      "<pre><code>\n" & res & "\n</code></pre>\n"
    ))

  for match in md.findAll(blockQuote):
    var strippedMatch = match.replace(blockQuoteLeadingSymbol)
    md = md.replace(match, "<blockquote>\n" & strippedMatch & "\n</blockquote>")

  md = md.parallelReplace(@[url, link, image, italics, bold, strikethrough] & headers)

  # XXX nested lists

  for match in md.findAll(enumeratedList):
    var res = "<ol>\n"
    for line in match.splitLines:
      if line =~ enumeratedListLine:
        res.add("<li>" & matches[0] & "</li>\n")
      else:
        # blank line? Skip
        continue
    res.add("</ol>")
    md = md.replace(match, res)

  for match in md.findAll(itemizedList):
    var res = "<ul>\n"
    for line in match.splitLines:
      res.add("<li>")
      if line =~ taskListLine:
        res.add("""<input type="checkbox" disabled="" """)
        if matches[0].len > 0 and matches[0][0] in {'x', 'X'}:
          res.add("""checked=""></input>""" & matches[1])
        else:
          res.add("""/></input>""" & matches[1])
      elif line =~ itemizedListLine:
        res.add(matches[0])
      else:
        # Blank line, just undo and skip
        res = res.substr(0, res.len-5)
        continue
      res.add("</li>\n")
    res.add("</ul>\n")

    md = md.replace(match, res)

  md = md.parallelReplace(hashToCodeMap)
  md = md.parallelReplace(hashToInlineCodeMap)

  return md

let input = stdin.readAll()
stdout.write(parseMarkdown(input).toHtml())
