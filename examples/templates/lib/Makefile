CXXFLAGS += -std=c++11 -fPIC

libcppop-example-templates.so: instances.o std.o
	$(CXX) -shared $^ -o $@
