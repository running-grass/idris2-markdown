module Main

import Tester
import Tester.Runner

import Language.Markdown
import Language.Markdown.Data
import Language.Markdown.Parser
-- import Text.Markdown.Lexer

tests : List Test
tests = [
    test "测试一级标题" $ assertEq (parse "# 1 stheading") (Just $ MDoc ([MHeading 1 "1 stheading"]))
    , test "测试二级标题" $ assertEq (parse "## 2 stheading") (Just $ MDoc ([MHeading 2 "2 stheading"]))
    , test "测试三级标题" $ assertEq (parse "### 3 stheading") (Just $ MDoc ([MHeading 3 "3 stheading"]))
    , test "测试四级标题" $ assertEq (parse "#### 4 stheading") (Just $ MDoc ([MHeading 4 "4 stheading"]))
    , test "测试五级标题" $ assertEq (parse "##### 5 stheading") (Just $ MDoc ([MHeading 5 ("5 stheading")]))

    , test "测试后空格" $ assertEq (parse "  ## 2 stheading") (Just $ MDoc [MLine $ [MBare "  ## 2 stheading"]])
    , test "测试后空格" $ assertEq (parse "## 2 stheading   ") (Just $ MDoc [MHeading 2 "2 stheading   "])
]

main : IO ()
main = do
    success <- runTests tests
    if success
        then putStrLn "All tests passed"
        else putStrLn "Not all tests passed"

-- main : IO ()
-- main = putStrLn "你好啊，Hello from Idris2!"
