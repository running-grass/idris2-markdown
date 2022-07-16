.PHONY: help
help:
	echo "help me"

.PHONY: test
test: 
	pack run test/test.ipkg

.PHONY: build
build: 
	pack build markdown.ipkg