module Language.Markdown.Tokens

import Text.Lexer

public export
data MarkdownTokenKind
  = MKNumberSign
  | MKAsterisk
  | MKText
  | MKSpace
  | MKBreak
  | MKBackQuote
  | MKUnderline

public export
Eq MarkdownTokenKind where
  (==) MKNumberSign MKNumberSign = True
  (==) MKAsterisk MKAsterisk = True
  (==) MKText MKText = True
  (==) MKSpace MKSpace = True
  (==) MKBreak MKBreak = True
  (==) MKBackQuote MKBackQuote = True
  (==) MKUnderline MKUnderline = True
  (==) _ _ = False

public export
Show MarkdownTokenKind where
  show MKNumberSign = "MKNumberSign"
  show MKAsterisk = "MKAsterisk"
  show MKText = "MKText"
  show MKSpace = "MKSpace"
  show MKBreak = "MKBreak"
  show MKBackQuote = "MKBackQuote"
  show MKUnderline = "MKUnderline"

public export
TokenKind MarkdownTokenKind where
  TokType MKText = String
  TokType MKSpace = String
  TokType MKAsterisk = String
  TokType MKNumberSign = String
  TokType _ = ()

  tokValue MKNumberSign _ = "#"
  tokValue MKAsterisk _ = "*"
  tokValue MKText s = s
  tokValue MKSpace _ = " "
  tokValue MKBreak _ = ()
  tokValue MKBackQuote _ = ()
  tokValue MKUnderline _ = ()


public export
MarkdownToken : Type
MarkdownToken = Token MarkdownTokenKind

public export
Show MarkdownToken where
    show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

