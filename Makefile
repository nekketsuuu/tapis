SHELL := /bin/bash

.PHONY: all bin
all: bin
bin:
	@$(MAKE) -C src

## Examples & benchmark
TESTS = test01 test02 test03 test04 test05 test06 test07 \
ack addsub addsub2 fact fact2 fib
TOOL = ./UltimateAutomizer.sh # Change as you like

PHONY: reset_test reset_test_force
test: bin reset_test $(TESTS:%=example/%.c)
reset_test:
	@echo "Reset testlog..."
	@echo "" > testlog
test-force: bin reset_test_force $(TESTS:%=example/%.c)
reset_test_force:
	@echo "Delete all C files..."
	@rm -f $(TESTS:%=example/%.c)
	@echo "Reset testlog..."
	@echo "" > testlog
example/%.c: example/%.pi
	@echo -n $<
	@echo $< >> testlog
	@(time ./piterm -tool=${TOOL} $<) &>>testlog || rm -f $@
	@tail -n5 testlog | head -n1
	@echo "" >> testlog

## clean
.PHONY: clean clean-example
clean:
	@$(MAKE) -C src clean
clean-example:
	@rm -f $(TESTS:%=example/%.c)
	@rm -f $(TESTS:%=example/%.bc) $(TESTS:%=example/%.kittel) $(TESTS:%=example/%.kittel.proof) $(TESTS:example/%=%.t2)
	@rm -f $(TESTS:example/%=%.ll)
clean-all: clean clean-example
