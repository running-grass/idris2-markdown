module Main

import Tester
import Tester.Runner

import Language.Markdown
import Language.Markdown.Data
import Language.Markdown.Parser
-- import Text.Markdown.Lexer

tests : List Test
tests = [
    test "测试一级标题" $ assertEq (parse "# 1stheading") (Just $ MDoc ([MHeading 1 "1stheading"]))
    , test "测试二级标题" $ assertEq (parse "## 2stheading") (Just $ MDoc ([MHeading 2 "2stheading"]))
    , test "测试三级标题" $ assertEq (parse "### 3stheading") (Just $ MDoc ([MHeading 3 "3stheading"]))
    , test "测试四级标题" $ assertEq (parse "#### 4stheading") (Just $ MDoc ([MHeading 4 "4stheading"]))
    , test "测试五级标题" $ assertEq (parse "##### 5stheading") (Just $ MDoc ([MHeading 5 ("5stheading")]))

    -- , test "测试前空格" $ assertEq (parse " ## 2stheading") (Paragraph " ## 2stheading"]))
    -- , test "测试后空格" $ assertEq (parse "## 2stheading   ") (Just MDoc([MHeading 2 "2stheading"]))
]

main : IO ()
main = do
    success <- runTests tests
    if success
        then putStrLn "All tests passed"
        else putStrLn "Not all tests passed"

-- main : IO ()
-- main = putStrLn "你好啊，Hello from Idris2!"
