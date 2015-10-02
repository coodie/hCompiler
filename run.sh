#!/bin/bash
file=$1
filename=${file%.*}
oend=".o"
filenameobj=$filename$oend

if [ -e $filename ] 
then
    rm ./$filename ./$filenameobj
fi

./Main -S < $file | as --32 -o $filenameobj

if [ -e $filenameobj ] 
then
    gcc -m32 $filenameobj -o $filename
fi

if [ -e $filename ] 
then
    ./$filename
fi
