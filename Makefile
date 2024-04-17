steno-tools-bin = target/release/steno-tools
rust-sources = $(shell find src/ -name '*.rs')

generated-dict = generated/generated.json
pronunciation-dicts = $(wildcard sources/pronunciation/*)
pronunciation-dicts-flags = $(patsubst %,-p%,$(pronunciation-dicts))
bad-pronunciations = sources/bad-pronunciations
wordlist = sources/wordlist.txt
theory = theory.yaml
overrides = sources/overrides.yaml

$(generated-dict): $(steno-tools-bin) $(pronunciation-dicts) \
	$(bad-pronunciations) $(wordlist.txt) $(theory) $(overrides)
	$(steno-tools-bin) generate-outlines -t $(theory) \
		$(pronunciation-dicts-flags) \
		-d $(bad-pronunciations) \
		-w $(wordlist) \
		-O $(overrides) \
		-o $@

$(steno-tools-bin): $(rust-sources) Cargo.toml Cargo.lock
	cargo build --release
