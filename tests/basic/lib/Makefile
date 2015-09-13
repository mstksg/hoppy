CXXFLAGS += -std=c++11 -fPIC

libcppop-tests-basic.so: basic.o functions.o
	$(CXX) -shared $^ -o $@
