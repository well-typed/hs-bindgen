#CLANG_ROOT = /usr/lib/llvm15
#export PATH := $(CLANG_ROOT)/bin:$(PATH)

all: experiment exdriver
.PHONY: all

clean:
	rm -f experiment exdriver
.PHONY: clean

experiment: experiment.cpp
	clang++ \
		$(shell llvm-config --cxxflags) \
		$(shell llvm-config --ldflags) \
		$(shell llvm-config --libs) \
		-lclang-cpp \
		-o experiment \
		experiment.cpp

exdriver: exdriver.cpp
	clang++ \
		$(shell llvm-config --cxxflags) \
		$(shell llvm-config --ldflags) \
		$(shell llvm-config --libs) \
		-lclang-cpp \
		-o exdriver \
		exdriver.cpp
