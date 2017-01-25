.PHONY: all src clean clean-example

all: src

src:
	@$(MAKE) -C src

clean:
	@$(MAKE) -C src clean

clean-example:
	@$(MAKE) -C example clean

clean-all: clean clean-example
