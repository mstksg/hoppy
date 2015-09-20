CXXFLAGS += -std=c++11 -fPIC

.PHONY: clean

libcppop-tests-basic.so: basic.o functions.o ptrctr.o
	$(CXX) -shared $^ -o $@

clean:
	-rm basic.cpp basic.hpp *.o *.so
