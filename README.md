## c-blike version 1.5.3

### ビルド、インストール方法

        ./autogen.sh
        ./configure --prefix=/usr
        make
        sudo make install

備考: ビルド、インストール作業には gcc, make, gnu/binutils, autoconf, automake, libtool, pkg-config が必要です。
X11環境での描画には GTK+3、非X11環境でのフレームバッファー表示には linux/fb.h による /dev/fb0 を用います。

### アンインストール方法

        sudo make uninstall

### サンプルのコンパイル方法
X11環境（GTK+3を利用）でのアプリとしてビルドする場合は以下のようにします: 

        cd samples
        ./build.sh

非X11環境（フレームバッファーを利用）でのアプリとしてビルドする場合は以下のようにします:
（プリフィックスが lfb- のバイナリーが生成されます）

        cd samples
        ./build-lfb.sh

### たとえば自作のhello.cのコンパイル方法
X11環境（GTK+3を利用）でのアプリとしてビルドする場合:

        gcc `pkg-config blike --libs --cflags` -o hello hello.c

非X11環境（フレームバッファーを利用）でのアプリとしてビルドする場合:

        gcc -lblike-common -lblike-drvlfb -lm -o hello hello.c
