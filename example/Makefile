.PHONY: run test

all:
	@echo "Usage: 'make run', 'make test', or 'make test ONLY=string-to-match'"

test:
	stack runghc -- -isrc test/Spec.hs -m "$(ONLY)"

run:
	wai-devel -p src/Main.hs -f main
