SHELL := /bin/bash

.PHONY: all bin
all: bin
bin:
	@"$(MAKE)" -C src

## Examples & benchmark
TESTS = test01 test02 test03 test04 test05 test06 test07 \
ack addsub addsub2 fact fact2 fib \
typical/simple1 typical/simple2 typical/simple3 typical/simple4 \
typical/simple5 typical/simple6 typical/simple7 typical/simple8 \
typical/client-server typical/client-server-wrong typical/cobegin \
typical/demo2 typical/ds-ex5-1 typical/ds-ex5-2 \
typical/factorial typical/factorial-rec typical/factorial-rec2 \
typical/lock typical/lock-AB typical/lock-AC typical/lock-AD \
typical/stable

TOOL = ./UltimateAutomizer.sh # Change as you like

PHONY: reset_test
test: bin reset_test $(TESTS:%=example/%.c)
reset_test:
	@echo "Reset testlog..."
	@echo "Current tool is ${TOOL}" > testlog
	@echo "Delete all C files..."
	@rm -f $(TESTS:%=example/%.c)
test-rest: bin $(TESTS:%=example/%.c)
example/%.c: example/%.pi
	@echo -n $<
	@echo $< >> testlog
	@(time ./piterm -tool="${TOOL}" $<) &>>testlog || :
	@tail -n5 testlog | head -n1
	@echo "" >> testlog

## clean
.PHONY: clean clean-example
clean:
	@"$(MAKE)" -C src clean
clean-example:
	@rm -f $(TESTS:%=example/%.c)
	@rm -f $(TESTS:%=example/%.bc) $(TESTS:%=example/%.kittel) $(TESTS:%=example/%.kittel.proof) $(TESTS:example/%=%.t2)
	@rm -f $(TESTS:example/%=%.ll)
clean-all: clean clean-example
