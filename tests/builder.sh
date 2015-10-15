#!/bin/sh

source $stdenv/setup
mkdir $out
for dep in $nativeBuildInputs; do
    name=${dep##*-hoppy-tests-}
    name=${name%%-[0-9]*}
    ln -s $dep $out/$name
done
