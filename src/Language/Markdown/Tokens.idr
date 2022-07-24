module Language.Markdown.Tokens

import Text.Lexer
import Data.String

public export
data MarkdownTokenKind
  = MKNumberSign
  | MKAsterisk
  | MKText
  | MKSpace
  | MKBreak
  | MKBackQuote
  | MKUnderline
  | MKCode

public export
Eq MarkdownTokenKind where
  (==) MKNumberSign MKNumberSign = True
  (==) MKAsterisk MKAsterisk = True
  (==) MKText MKText = True
  (==) MKSpace MKSpace = True
  (==) MKBreak MKBreak = True
  (==) MKBackQuote MKBackQuote = True
  (==) MKUnderline MKUnderline = True
  (==) MKCode MKCode = True
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
  show MKCode = "MKCode"

public export
TokenKind MarkdownTokenKind where
  TokType MKText = String
  TokType MKCode = String
  TokType MKSpace = String
  TokType MKAsterisk = String
  TokType MKNumberSign = String
  TokType _ = ()

  tokValue MKNumberSign _ = "#"
  tokValue MKAsterisk _ = "*"
  tokValue MKText s = s
  tokValue MKCode s = strSubstr 1 ((strLength s) - 2) s
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

