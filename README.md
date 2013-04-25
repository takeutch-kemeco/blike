## c-blike version 1.5.3

### ビルド、インストール方法

        ./autogen.sh
        ./configure --prefix=/usr
        make
        sudo make install

備考: ビルド、インストール作業には gcc, make, gnu/binutils, autoconf, automake, libtool, pkg-config が必要です。

### アンインストール方法
        sudo make uninstall

### サンプルのコンパイル方法
        cd samples
        ./build.sh

### たとえば自作のhello.cのコンパイル方法
        gcc `pkg-config blike --libs --cflags` -o hello hello.c
