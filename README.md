## c-blike version 1.5.4

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

現状、非X11環境（フレームバッファーを利用）で動作させる場合は、キーボード入力を利用できません。
まだ実装していないからです。
誰か暇な人いたら、その機能を追加するパッチを書いて提供してくれると光栄です。そんな奴がいるとはまったく期待していませんが。

### Haskell (GHC) から 利用する場合の例
samples/ghc 以下に、Haskell (GHC) から blike を利用する場合のサンプルがあります:

        cd samples/ghc
        make

ここにある blike.hs は、FFI による blike 関数のラッパーです。
これは blike 関数を同名の Haskell 関数としてラップしてある状態です。
ビルドの書式は同梱の Makefile を参考にしてください。

hs_test000a.hs は test000a.c に相当するサンプルです。（ハローワールド）
blike は blMain が存在するため、いくらか無駄に複雑になってしまってますが、ここの書式は定形化できるので自動化して隠蔽することも可能だろうと期待してます。

blike.hs では blike.h で紹介されてる関数のほとんどをラップ済みですが、一部の関数は未実装です。
現在の blike.hs で未実装の関数は bl_printf, bl_scanf, bl_malloc, bl_getGrpB の４つです。それ以外は全て実装済みです。

blike0.h で紹介されてるドライバー系関数 (プリフィックスが bld_ の関数) へのラッパーは未実装です。

### 最近修正したバグ
bl_gets() が正常動作せずに、メモリー違反で落ちてしまうバグを修正しました。
これは common/gets.c の方を直接修正してしまったので、公式版の c_blike_01f の動作とは異なってしまってるはずです。

### 現在判明してるバグ
bl_gets() でカーソルキーを使って入力した場合に、ウインドウ幅以上の文字列を入力する場合に、カーソルの位置が変になってしまいます。
誰か暇な人いたら、修正パッチを書いて提供してくれると光栄です。そんな奴がいるとはまったく期待していませんが。
