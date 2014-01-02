echo DONT FORGET TO CHANGE VERSION increment on Makefile.Am
echo "libcgicc_la_LDFLAGS = -lstdc++ -version-info 5:2:0 "
echo The version info format is CURRENT:REVISION:AGE. 
echo "- If you have not changed the interface (bug fixes) bump the version to CURRENT : REVISION+1 : AGE "
echo "- If you have augmented the interface (new functions)bump the version to CURRENT+1 : 0 : AGE+1 "
echo "- If you have broken old interface (e.g. removed functions) bump the version to CURRENT+1 : 0 : 0" 
rm -rf rpge-$1/*
rm -rf rpge-$1
cd /tmp
rm -rf rpge-$1/*
rm -rf rpge-$1
cd -
mkdir /tmp/rpge-$1
./autogen
cp -Rf * /tmp/rpge-$1  
mv /tmp/rpge-$1 .
cd rpge-$1
echo "s/NOTVERSIONNEDPACKAGE/"$1"/" >tempGen.sed
echo "Writing"
sed -f tempGen.sed configure.in>temp2
rm configure.in
sed -f tempGen.sed configure>temp21
rm configure
mv temp21 configure
chmod +x configure
mv temp2 configure.in
sed -f tempGen.sed Makefile.in>temp22
rm temp2 temp21 temp22
sed -f tempGen.sed doxygen.conf>temp3
rm doxygen.conf
mv temp3 doxygen.conf
rm tempGen.se

rm INSTALL
rm m4/ltsugar.m4
rm m4/lt~obsolete.m4
rm m4/ltoptions.m4
rm m4/ltversion.m4
rm m4/libtool.m4

./autogen

./configure
make distclean

cd ..


tar -czf rpge-$1.tar.gz rpge-$1

