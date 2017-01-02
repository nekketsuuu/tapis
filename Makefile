.PHONY: all src clean

all: src

src:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean

clean-all: clean
	rm -f test/*.c test/*.bc test/*.kittel test/*.kittel.proof test/*.t2 test/*.ll
