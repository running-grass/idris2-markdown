module Test.Parser

import Tester
import Tester.Runner

import Language.Markdown
import Language.Markdown.Data
import Language.Markdown.Parser

private
assertBlocks : String -> List Block -> TestFunc ()
assertBlocks str blks = assertEq (parse str) (Just $ MDoc blks)

private 
assertLine : String -> List Inline -> TestFunc ()
assertLine str ins = assertBlocks str [MLine ins]

private
testHeading : List Test
testHeading = [
    test "测试一级标题" $ assertBlocks  "# 1 stheading" [MHeading 1 [MBare "1 stheading"]]
    , test "测试二级标题" $ assertBlocks "## 2 stheading" [MHeading 2 [MBare "2 stheading"]]
    , test "测试三级标题" $ assertBlocks "### 3 stheading" [MHeading 3 [MBare "3 stheading"]]
    , test "测试四级标题" $ assertBlocks "#### 4 stheading" [MHeading 4 [MBare "4 stheading"]]
    , test "测试五级标题" $ assertBlocks "##### 5 stheading" [MHeading 5 [MBare "5 stheading"]]

    , test "测试后空格" $ assertBlocks "  ## 2 stheading" [MLine $ [MBare "  ## 2 stheading"]]
    , test "测试后空格" $ assertBlocks "## 2 stheading   " [MHeading 2 [MBare "2 stheading   "]]
    , test "测试标题中的多余的#" $ assertBlocks "## I am ###" [MHeading 2 [MBare "I am ###"]]
    , test "测试标题匹配失败" $ assertBlocks "##2 stheading" [MLine [MBare "##2 stheading"]]
]

-- 测试粗体的解析
private
testBold : List Test
testBold = [
    test "测试普通粗体" $ assertBlocks "**bold**" [MLine [MBold "bold"]]
    , test "测试粗体失败" $ assertBlocks "**bold" [MLine [MBare "**bold"]]
    , test "测试粗体失败1" $ assertBlocks "**bold*" [MLine [MBare "**bold*"]]
    , test "测试粗体失败2" $ assertBlocks "*bold**" [MLine [MItalic "bold", MBare("*")]]
    , test "测试多个粗体" $ assertBlocks "**bold** normal **bold** " [MLine [MBold "bold",MBare " normal ",MBold "bold", MBare " "]]
    , test "测试标题中的粗体" $ assertBlocks "## I am **BOLD**" [MHeading 2 [MBare "I am ", MBold "BOLD"]]
]

-- 测试行内代码
private
testCode : List Test
testCode = [
    test "测试单独代码" $ assertBlocks "`code`" [MLine [MCode "code"]]
    , test "测试代码匹配失败" $ assertBlocks "no`code" [MLine [MBare "no`code"]]
    , test "测试多个代码" $ assertBlocks "我有个`code`，还有个`code2`" [MLine [MBare"我有个", MCode "code", MBare "，还有个", MCode "code2"]]
    , test "测试标题中的代码" $ assertBlocks "## 标题中有个`code`" [MHeading 2 [MBare"标题中有个", MCode "code"]]
    , test "测试代码中的特殊字符" $ assertBlocks "`## ** code ** ## $$`" [MLine [MCode "## ** code ** ## $$"]]
]

-- 测试斜体
private
testItalic : List Test
testItalic = [
    test "测试简单斜体" $ assertLine "*italic*" [MItalic "italic"]
    , test "测试失败斜体1" $ assertLine "*italic" [MBare "*italic"]
    , test "测试失败斜体2" $ assertLine "italic*" [MBare "italic*"]
    , test "测试失败斜体3" $ assertLine "ita*lic" [MBare "ita*lic"]
    , test "测试连续斜体" $ assertLine "*italic**it2*" [MItalic "italic", MItalic "it2"]
    , test "测试连续斜体2" $ assertLine "*italic* *it2*" [MItalic "italic", MBare " ", MItalic "it2"]
    , test "测试粗体斜体" $ assertLine "**bold***italic* *it2*" [MBold "bold", MItalic "italic", MBare " ", MItalic "it2"]
    , test "测试标题中斜体" $ assertBlocks "## 我是*斜体*" [MHeading 2 [MBare "我是", MItalic "斜体"]]
]

-- 测试行内代码
private
testHor : List Test
testHor = [
    test "测试水平线" $ assertBlocks "---" [MHorizontal]
    , test "测试水平线加空格" $ assertBlocks "---     " [MHorizontal]
    , test "测试水平线加后空行" $ assertBlocks "---\n" [MHorizontal]
    , test "测试水平线加前软换行" $ assertBlocks "\n---" [MLine [MBare " ---"]]
    , test "测试水平线加前硬换行" $ assertBlocks "\n\n---" [MSpaceLine, MHorizontal]
    , test "测试水平线加前空格" $ assertBlocks "  ---" [MHorizontal]
    , test "测试水平线多加-" $ assertBlocks "-------  " [MHorizontal]
    , test "测试水平线加后字符" $ assertBlocks "---abcd" [MLine [MBare "---abcd"]]
]

export 
testParser : List Test
testParser = testHeading ++ testBold ++ testCode ++ testItalic ++ testHor


private
main : IO ()
main = do
    success <- runTests $ testParser
    if success
        then putStrLn "解析器工作正常"
        else putStrLn "解析器出错"