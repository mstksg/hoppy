CXXFLAGS += -std=c++11 -fPIC

libhoppy-example-templates.so: instances.o std.o
	$(CXX) -shared $^ -o $@
