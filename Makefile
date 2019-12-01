.PHONY: test

test:
	cabal new-test

build:
	cabal new-build

run: build
	cabal new-exec adv