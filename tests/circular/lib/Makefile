# Keep the -I... in sync with default.nix.
CXXFLAGS += -std=c++11 -fPIC -I../../../cppop/include

.PHONY: clean

libcppop-tests-circular.so: flob.o flobm.o flub.o flubm.o
	$(CXX) -shared $^ -o $@

clean:
	-rm {flob,flub}m.{cpp,hpp} *.o *.so
