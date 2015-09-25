# Keep the -I... in sync with default.nix.
CXXFLAGS += -std=c++11 -fPIC -I../../../cppop/include

.PHONY: clean

libcppop-tests-basic.so: basic.o constants.o functions.o ptrctr.o
	$(CXX) -shared $^ -o $@

clean:
	-rm basic.cpp basic.hpp *.o *.so
