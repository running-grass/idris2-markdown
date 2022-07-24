module Language.Markdown.Lexer

import Data.Nat
import Text.Lexer
-- import 

import Language.Markdown.Tokens

-- ignored : WithBounds MarkdownToken -> Bool
-- ignored (MkBounded (Tok CTIgnore _) _ _) = True
-- ignored _ = False

multNewLine : Recognise (isSucc (min (atLeast 2)))
multNewLine = count (atLeast 2) newline

spaceLineBreak = space2 <+> newline1
  where
    space2 = count (atLeast 2) $ is ' '
    newline1 = (count (atLeast 1) newline)


atLeastSome : (n : Nat) -> Lexer -> Recognise (isSucc (min (atLeast n)))
atLeastSome n l = count (atLeast n) l

atLeast1 : Lexer -> Recognise (isSucc (min (atLeast 1)))
atLeast1 l = atLeastSome 1 l

someThen : (stopAfter : Lexer) -> (l : Lexer) -> Lexer
someThen stopAfter l = (someUntil stopAfter l) <+> stopAfter

surround1 : (start : Lexer) -> (end : Lexer) -> (l : Lexer) -> Lexer
surround1 start end l = start <+> someThen end l

quote1 : (q : Lexer) -> (l : Lexer) -> Lexer
quote1 q l = surround1 q q l


code : Lexer
code = quote1 (is '`') (isNot '`')

markdownTokenMap : TokenMap MarkdownToken
markdownTokenMap = toTokenMap [
  (multNewLine,  MKBreak),
  (spaceLineBreak,  MKBreak),
  (code, MKCode),
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
