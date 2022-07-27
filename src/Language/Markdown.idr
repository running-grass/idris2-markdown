module Language.Markdown

import Data.String

import Text.Parser


import public Language.Markdown.Lexer
import public Language.Markdown.Parser
import public Language.Markdown.Data

public export
parse : String -> Maybe Markdown
parse x = parseMarkdown !(lexMarkdown x)