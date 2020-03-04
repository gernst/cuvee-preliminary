SCALA=$(wildcard cuvee/src/cuvee/*.scala cuvee/test/src/cuvee/**.scala)

JAR=./out/cuvee/assembly/dest/out.jar
BIN=./cuvee.sh
MILL=./mill -s -disable-ticker

.PHONY: all test clean

all: $(BIN)

test:
	@echo "test"; $(MILL) cuvee.test

clean:
	@echo "clean"; $(MILL) clean

$(BIN): $(JAR)
	@echo "$@"; echo "#!/bin/sh" > $@; echo "java -jar $(JAR) \$$@" >> $@; chmod +x $(BIN)

$(JAR): $(SCALA)
	@echo "$@"; $(MILL) cuvee.assembly
