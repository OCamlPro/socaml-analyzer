configure:
	ocp-build configure

all:
	ocp-build build
	ocp-build test

clean:
	ocp-build clean

.PHONY: configure all clean
