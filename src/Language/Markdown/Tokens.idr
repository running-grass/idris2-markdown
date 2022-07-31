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
  | MKSoftBreak
  | MKBackQuote
  | MKUnderline
  | MKDash

public export
Eq MarkdownTokenKind where
  (==) MKNumberSign MKNumberSign = True
  (==) MKAsterisk MKAsterisk = True
  (==) MKText MKText = True
  (==) MKSpace MKSpace = True
  (==) MKBreak MKBreak = True
  (==) MKSoftBreak MKSoftBreak = True
  (==) MKBackQuote MKBackQuote = True
  (==) MKUnderline MKUnderline = True
  (==) MKDash MKDash = True
  (==) _ _ = False

public export
Show MarkdownTokenKind where
  show MKNumberSign = "MKNumberSign"
  show MKAsterisk = "MKAsterisk"
  show MKText = "MKText"
  show MKSpace = "MKSpace"
  show MKBreak = "MKBreak"
  show MKSoftBreak = "MKSoftBreak"
  show MKBackQuote = "MKBackQuote"
  show MKUnderline = "MKUnderline"
  show MKDash = "MKDash"

public export
TokenKind MarkdownTokenKind where
  TokType MKText = String
  TokType MKDash = String
  TokType MKSpace = String
  TokType MKAsterisk = String
  TokType MKBackQuote = String
  TokType MKNumberSign = String
  TokType MKUnderline = String
  TokType _ = ()

  tokValue MKNumberSign _ = "#"
  tokValue MKAsterisk _ = "*"
  tokValue MKText s = s
  tokValue MKDash _ = "-"
  tokValue MKSpace _ = " "
  tokValue MKBreak _ = ()
  tokValue MKSoftBreak _ = ()
  tokValue MKBackQuote _ = "`"
  tokValue MKUnderline _ = "_"

public export
MarkdownToken : Type
MarkdownToken = Token MarkdownTokenKind

public export
Show MarkdownToken where
    show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

