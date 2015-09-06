CXXFLAGS += -fPIC

libcppop-example-templates.so: instances.o std.o
	$(CXX) -shared $^ -o $@
