SCALA=$(wildcard cuvee/src/cuvee/*.scala)

JAR=./out/cuvee/assembly/dest/out.jar
BIN=./Cuvee
MILL=./mill -s -disable-ticker

.PHONY: all test

all: $(BIN)

test:
	@echo "test"; $(MILL) cuvee.test

$(BIN): $(JAR)
	@echo "$@"; echo "#!/bin/sh" > $@; echo "java -jar $(JAR) \$$@" >> $@; chmod +x $(BIN)

$(JAR): $(SCALA)
	@echo "$@"; $(MILL) cuvee.assembly
