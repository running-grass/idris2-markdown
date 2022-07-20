module Language.Markdown

import Data.String

||| Markdown的语法节点
|||
public export
data MDNode = Heading Int String
            | Paragraph String

public export
Eq MDNode where
  Paragraph s1 == Paragraph s2 = s1 == s2
  Heading n1 s1 == Heading n2 s2 = n1 == n2 && s1 == s2
  _ == _ = False

public export
Show MDNode where 
  show (Paragraph str) = "Paragraph " ++ show str
  show (Heading n str) = "Heading " ++ show n ++ " " ++ show str 



tailWith : Int -> String -> String
tailWith start str = strSubstr start (strLength str) str

||| 从字符串解析Markdown结构
|||
||| ```idris example
||| parseMD "# 我是一级标题"
||| ```
public export
parseMD : String -> MDNode
parseMD str = case break (== ' ') str of
   ("#", tstr) =>  Heading 1 $ trim tstr
   ("##", tstr) =>  Heading 2 $ trim tstr
   ("###", tstr) =>  Heading 3 $ trim tstr
   ("####", tstr) =>  Heading 4 $ trim tstr
   ("#####", tstr) =>  Heading 5 $ trim tstr
   _ => Paragraph str
