module Main

import Tester
import Tester.Runner

import Language.Markdown
-- import Text.Markdown.Lexer

tests : List Test
tests = [
    test "测试一级标题" $ assertEq (parseMD "# 1st heading") (Heading 1 "1st heading")
    , test "测试二级标题" $ assertEq (parseMD "## 2st heading") (Heading 2 "2st heading")
    , test "测试三级标题" $ assertEq (parseMD "### 3st heading") (Heading 3 "3st heading")
    , test "测试四级标题" $ assertEq (parseMD "#### 4st heading") (Heading 4 "4st heading")
    , test "测试五级标题" $ assertEq (parseMD "##### 5st heading") (Heading 5 "5st heading")

    , test "测试前空格" $ assertEq (parseMD " ## 2st heading") (Paragraph " ## 2st heading")
    , test "测试后空格" $ assertEq (parseMD "## 2st heading   ") (Heading 2 "2st heading")
]

main : IO ()
main = do
    success <- runTests tests
    if success
        then putStrLn "All tests passed"
        else putStrLn "Not all tests passed"

-- main : IO ()
-- main = putStrLn "你好啊，Hello from Idris2!"
