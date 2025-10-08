#!/bin/bash

cd test
#ls -ltr

cp *.md ../doc/strehl/
cp *.png ../doc/strehl/

meld . ../doc/strehl/

