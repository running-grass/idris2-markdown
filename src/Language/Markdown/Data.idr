module Language.Markdown.Data

import Data.List

%default total

public export
data Inline = MBold String | MBare String

public export
Show Inline where
  show (MBold s) = "MBold(" ++ s ++ ")"
  show (MBare s) = "MBare(" ++ s ++ ")"

export
Eq Inline where
  (MBold x) == (MBold y) =  x == y
  (MBare x) == (MBare y) =  x == y
  _ == _ = False

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