aclocal --install -I m4
libtoolize --install
autoheader
automake -acf
autoconf
./configure $@

