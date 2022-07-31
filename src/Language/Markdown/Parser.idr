module Language.Markdown.Parser

import Language.Markdown.Data
import Text.Parser
import Data.List
import Data.Nat


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

private
softbreak : Grammar state MarkdownToken True String
softbreak = do
  match MKSoftBreak
  pure " "

-- 水平线
private
horizontal : Grammar state MarkdownToken (isSucc (min (atLeast 3)) || Delay True) Block
horizontal = do
  _ <- many $ match MKSpace
  _ <- count (atLeast 3) $ match MKDash
  _ <- many $ match MKSpace
  maybeNewline <|> match MKSoftBreak
  pure $ MHorizontal

spaceLine : Grammar state MarkdownToken True Block
spaceLine = do 
  _ <- some $ match MKBreak
  pure $ MSpaceLine

mutual
  private
  document : Grammar state MarkdownToken True Markdown
  document = do
    vals <- some (heading <|> horizontal <|> line <|> spaceLine)
    pure $ MDoc $ forget vals

  textNumberSign : Grammar state MarkdownToken True Inline
  textNumberSign = pure $ MBare !(match MKNumberSign)

  textSpace : Grammar state MarkdownToken True Inline
  textSpace = pure $ MBare !(match MKSpace <|> softbreak)

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
    comps <- some inlineComp
    maybeNewline
    pure $ MLine $ mergeBare $ forget comps
  
  private
  inlineComp : Grammar state MarkdownToken True Inline
  inlineComp = code <|> codeFail 
            <|> bold <|> boldFail
            <|> italic <|> italicFail 
            <|> textSpace <|> textNumberSign 
            <|> bare

  private
  code : Grammar state MarkdownToken True Inline
  code = do 
    _ <- match MKBackQuote
    vals <- some $ match MKText <|> match MKAsterisk <|> match MKNumberSign <|> match MKSpace
    _ <- match MKBackQuote
    pure $ MCode $ concat1 vals

  private
  codeFail : Grammar state MarkdownToken True Inline
  codeFail = do
    _ <- match MKBackQuote
    pure $ MBare "`"

  private
  italic : Grammar state MarkdownToken True Inline
  italic = do _ <- match MKAsterisk 
              vals <- some $ match MKText
              _ <- match MKAsterisk
              -- _ <- (eof <|> (nextIs "lala" isT))
              pure $ MItalic $ concat1 vals
  private
  italicFail : Grammar state MarkdownToken True Inline
  italicFail = do
    _ <- match MKAsterisk
    pure $ MBare "*"

  private
  bold : Grammar state MarkdownToken True Inline
  bold = do _ <- match MKAsterisk 
            _ <- match MKAsterisk 
            vals <- some $ match MKText
            _ <- match MKAsterisk
            _ <- match MKAsterisk
            pure $ MBold $ concat1 vals
  
  private
  boldFail : Grammar state MarkdownToken True Inline
  boldFail = do
    _ <- match MKAsterisk
    _ <- match MKAsterisk
    pure $ MBare "**"

  bare : Grammar state MarkdownToken True Inline
  bare = do vals <- some $ (match MKText <|> match MKSpace <|> match MKDash)
            pure $ MBare $ concat1 vals

export
parseMarkdown : List (WithBounds MarkdownToken) -> Maybe Markdown
parseMarkdown toks = case parse document toks of
                      Right (j, []) => Just j
                      _ => Nothing
