This test suite tests the logic around enum evaluation.

This suite also serves as a test that enum evaluation includes the sources
directory of the C++ gateway package on the header search path during its
compilation.  This is done by Hoppy automatically so that bindings don't have to
resort to custom compiler configuration or other workarounds to use evaluation
on enums declared within the C++ gateway package.
