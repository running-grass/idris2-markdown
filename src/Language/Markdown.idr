module Language.Markdown

import Data.String

import Language.Markdown.Lexer
import Language.Markdown.Parser
import Text.Parser

import public Language.Markdown.Data

public export
parse : String -> Maybe Markdown
parse x = parseMarkdown !(lexMarkdown x)
