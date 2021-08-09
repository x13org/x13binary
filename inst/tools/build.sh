#!/bin/sh
#
# (Minimal, SunOS etc compatible) Build script for x13asHTML
#
# Copyright (C) 2015 - 2021  Dirk Eddelbuettel
#
# Released under GPL (>= 2)

set -e
set -u

srctgz="https://www2.census.gov/software/x-13arima-seats/x13as/unix-linux/program-archives/x13as_htmlsrc-v1-1-b57.tar.gz"
file=`basename ${srctgz}`
echo "${file}"

cwd=`pwd`
td=`mktemp -d -p /tmp x13dirXXXXXX`
cd ${td}
wget ${srctgz}
ls -l ${file}
gunzip -v ${file}
tarfile=`basename ${file} .gz`
ls -l ${tarfile}
tar xvf ${tarfile}
#ls
#make -f makefile.gf
#for f in *.f; do
#    gfortran -c -O1 ${f}
#done
#rm -f getarg.o gettim.o setarg.o
#gfortran -static -o x13asHTML *.o -s -lm -lc
gmake -f makefile.gf x13asHTMLsv11b57
echo ""
echo "Done in build directory ${td}"
ls -l x13asHTMLsv11b57
cd ${cwd}
mv -v ${td}/x13asHTMLsv11b57 ../bin/x13ashtml
rm -rf ${td}
