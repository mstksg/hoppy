# Keep the -I... in sync with default.nix.
CXXFLAGS += -std=c++11 -fPIC -I../../../hoppy/include

.PHONY: clean

libhoppy-tests-stl.so: std.o stl.o
	$(CXX) -shared $^ -o $@

clean:
	-rm stl.cpp stl.hpp *.o *.so
