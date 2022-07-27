.PHONY: help
help:
	echo "help me"

.PHONY: test
test: 
	pack run test/test.ipkg

.PHONY: test-lexer
test-lexer: 
	pack exec test/src/Test/Lexer.idr

.PHONY: test-parser
test-parser: 
	pack exec test/src/Test/Parser.idr

.PHONY: build
build: 
	pack build markdown.ipkg