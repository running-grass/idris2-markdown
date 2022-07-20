module Language.Markdown.Parser

import Language.Markdown.Data
import Text.Parser
import Data.List

import public Language.Markdown.Tokens


%default total

private 
concat1 : List1 String -> String
concat1 vals = foldl1 (++) vals

mutual
  private
  document : Grammar state MarkdownToken True Markdown
  document = do
    vals <- some (line <|> heading)
    pure $ MDoc $ forget vals

  private
  heading : Grammar state MarkdownToken True Block
  heading = do
    comps <- some $ match MKNumberSign
    _ <- some $ match MKSpace
    strs <- some $ match MKText
    pure $ MHeading (cast $ length comps) (concat1 strs)

  private
  line : Grammar state MarkdownToken True Block
  line = do 
          comps <- some inlineComp
          pure $ MLine $ forget comps


  private
  inlineComp : Grammar state MarkdownToken True Inline
  inlineComp = bold <|> bare

  private
  bold : Grammar state MarkdownToken True Inline
  bold = do match MKAsterisk 
            match MKAsterisk 
            vals <- some $ match MKText
            match MKAsterisk
            match MKAsterisk
            pure $ MBold $ concat1 vals

  bare : Grammar state MarkdownToken True Inline
  bare = do vals <- some $ match MKText
            pure $ MBare $ concat1 vals

export
parseMarkdown : List (WithBounds MarkdownToken) -> Maybe Markdown
parseMarkdown toks = case parse document toks of
                      Right (j, []) => Just j
                      _ => Nothing
