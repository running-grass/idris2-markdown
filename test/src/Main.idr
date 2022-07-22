module Main

import Tester
import Tester.Runner

import Language.Markdown
import Language.Markdown.Data
import Language.Markdown.Parser

private
asserBlocks : String -> List Block -> TestFunc ()
asserBlocks str blks = assertEq (parse str) (Just $ MDoc blks)


private
testHeading : List Test
testHeading = [
    test "测试一级标题" $ asserBlocks  "# 1 stheading" [MHeading 1 [MBare "1 stheading"]]
    , test "测试二级标题" $ asserBlocks "## 2 stheading" [MHeading 2 [MBare "2 stheading"]]
    , test "测试三级标题" $ asserBlocks "### 3 stheading" [MHeading 3 [MBare "3 stheading"]]
    , test "测试四级标题" $ asserBlocks "#### 4 stheading" [MHeading 4 [MBare "4 stheading"]]
    , test "测试五级标题" $ asserBlocks "##### 5 stheading" [MHeading 5 [MBare "5 stheading"]]

    , test "测试后空格" $ asserBlocks "  ## 2 stheading" [MLine $ [MBare "  ## 2 stheading"]]
    , test "测试后空格" $ asserBlocks "## 2 stheading   " [MHeading 2 [MBare "2 stheading   "]]
    , test "测试标题中的多余的#" $ asserBlocks "## I am ###" [MHeading 2 [MBare "I am ###"]]

]


private
-- 测试粗体的解析
testBold : List Test
testBold = [
    test "测试普通粗体" $ asserBlocks "**bold**" [MLine [MBold "bold"]]
    , test "测试多个粗体" $ asserBlocks "**bold** normal **bold** " [MLine [MBold "bold",MBare " normal ",MBold "bold", MBare " "]]
    , test "测试标题中的粗体" $ asserBlocks "## I am **BOLD**" [MHeading 2 [MBare "I am ", MBold "BOLD"]]
]

public export
main : IO ()
main = do
    success <- runTests $ testHeading ++ testBold
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"