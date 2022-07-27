module Language.Markdown.Data

import Data.List

%default total

-- 行内元素
public export
data Inline = MBold String | MBare String | MCode String | MItalic String

public export
Show Inline where
  show (MBold s) = "MBold(" ++ s ++ ")"
  show (MItalic s) = "MItalic(" ++ s ++ ")"
  show (MBare s) = "MBare(" ++ s ++ ")"
  show (MCode s) = "MCode(" ++ s ++ ")"

export
Eq Inline where
  (MBold x) == (MBold y) =  x == y
  (MItalic x) == (MItalic y) =  x == y
  (MBare x) == (MBare y) =  x == y
  (MCode x) == (MCode y) =  x == y
  _ == _ = False

-- 块级元素
public export
data Block = MLine (List Inline) | MHeading Int (List Inline)

public export
Show Block where
  show (MLine s) = "MLine(" ++ show s ++ ")"
  show (MHeading n s) = "MHeading " ++ show n ++ " (" ++ show s ++ ")"

export
Eq Block where
  (MLine x) == (MLine y) =  x == y
  (MHeading s x) == (MHeading t y) =  s == t && x == y 
  _ == _ = False

public export
data Markdown = MDoc (List Block)

public export
Show Markdown where
  show (MDoc bs) = "MDoc(" ++ show bs ++ ")"  

export
Eq Markdown where
  (MDoc x) == (MDoc y) =  x == y