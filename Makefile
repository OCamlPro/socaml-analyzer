configure:
      ocp-build configure

build:
      ocp-build build

test:
      ocp-build test

all: build test

clean:
      ocp-build clean

.PHONY: configure all clean build test
