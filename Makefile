format:
	find ./*/src -name "*.hs" -exec ormolu -i {} \;

.PHONY: format
