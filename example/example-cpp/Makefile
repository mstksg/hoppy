# This file is part of Hoppy.
#
# Copyright 2015-2018 Bryan Gardiner <bog@khumba.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.PHONY: all

OS=$(shell ghc -e ":m +System.Info" -e "putStrLn os")

CC = g++
CFLAGS = -O2

all: cpp/libexample.so

cpp/libexample.so: cpp/utils.o cpp/gen_utils.o cpp/gen_std.o
ifeq ($(OS),darwin)
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -install_name @rpath/libexample.so -o $@ $^
else
	$(CC) $(CFLAGS) -dynamic -shared -fPIC -o $@ $^
endif

%.o: %.cpp
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

clean:
	rm -f cpp/*.o cpp/libexample.so

install:
ifeq ($(OS),darwin)
	install cpp/libexample.so "$(libdir)"
else
	install -t "$(libdir)" cpp/libexample.so
endif
