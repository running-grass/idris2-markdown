module Language.Markdown.Lexer

import Data.Nat
import Text.Lexer

import Language.Markdown.Tokens

-- ignored : WithBounds MarkdownToken -> Bool
-- ignored (MkBounded (Tok CTIgnore _) _ _) = True
-- ignored _ = False

multNewLine : Recognise (isSucc (min (atLeast 2)))
multNewLine = count (atLeast 2) newline

markdownTokenMap : TokenMap MarkdownToken
markdownTokenMap = toTokenMap [
  (multNewLine,  MKBreak),
  (is ' ', MKSpace),
  (is '#', MKNumberSign),
  (is '*', MKAsterisk),
  (is '_', MKUnderline),
  (is '`', MKBackQuote),
  (any, MKText)
]

public export
lexMarkdown : String -> Maybe (List (WithBounds MarkdownToken))
lexMarkdown str =
  case lex markdownTokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing
