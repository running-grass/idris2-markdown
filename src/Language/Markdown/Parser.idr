module Language.Markdown.Parser

import Language.Markdown.Data
import Text.Parser
import Data.List

import public Language.Markdown.Tokens


%default total

private 
concat1 : List1 String -> String
concat1 vals = foldl1 (++) vals

private
maybeNewline : Grammar state MarkdownToken False ()
maybeNewline = match MKBreak <|> eof

private
consBare : Inline -> List Inline -> List Inline
consBare (MBare xs1) (MBare xs2 :: rest) = MBare (xs1 ++ xs2) :: rest
consBare x xs = x :: xs

private
mergeBare : List Inline -> List Inline
mergeBare = foldr consBare []

fib : List Int -> Int
fib (x :: y :: xs) = x + y + fib xs
fib [] = 0
fib [x] = x
mutual
  private
  document : Grammar state MarkdownToken True Markdown
  document = do
    vals <- some (heading <|> line)
    pure $ MDoc $ forget vals

  textNumberSign : Grammar state MarkdownToken True Inline
  textNumberSign = pure $ MBare !(match MKNumberSign)

  textSpace : Grammar state MarkdownToken True Inline
  textSpace = pure $ MBare !(match MKSpace)

  private
  heading : Grammar state MarkdownToken True Block
  heading = do
    levels <- some $ match MKNumberSign
    _ <- some $ match MKSpace
    commit
    inlines <- some inlineComp
    maybeNewline
    pure $ MHeading (cast $ length levels) (mergeBare $ forget inlines)

  private
  line : Grammar state MarkdownToken True Block
  line = do 
    comps <- some (inlineComp <|> textNumberSign <|> textSpace)
    maybeNewline
    pure $ MLine $ mergeBare $ forget comps


  private
  inlineComp : Grammar state MarkdownToken True Inline
  inlineComp = bold <|> bare <|> textSpace <|> textNumberSign

  private
  bold : Grammar state MarkdownToken True Inline
  bold = do _ <- match MKAsterisk 
            _ <- match MKAsterisk 
            vals <- some $ match MKText
            _ <- match MKAsterisk
            _ <- match MKAsterisk
            pure $ MBold $ concat1 vals

  bare : Grammar state MarkdownToken True Inline
  bare = do vals <- some $ (match MKText <|> match MKSpace)
            pure $ MBare $ concat1 vals

export
parseMarkdown : List (WithBounds MarkdownToken) -> Maybe Markdown
parseMarkdown toks = case parse document toks of
                      Right (j, []) => Just j
                      _ => Nothing
