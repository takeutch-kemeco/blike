## c_blike_01f_linux version 1.6.0

### ビルド、インストール方法（Debian系、Ubuntu等）

        sudo apt update
        sudo apt install build-essential
        sudo apt install libgtk-4-dev

        ./autogen.sh
        ./configure --prefix=/usr
        make
        sudo make install

備考: ビルド、インストール作業には gcc, make, gnu/binutils, autoconf, automake, libtool, pkg-config, libgtk-4-dev が必要です。

### アンインストール方法

        sudo make uninstall

### サンプルのコンパイル方法
X11環境（GTK4を利用）でのアプリとしてビルドする場合は以下のようにします:

        cd samples
        make clean
        make

### たとえば自作のhello.cのコンパイル方法
X11環境（GTK4を利用）でのアプリとしてビルドする場合:

        gcc `pkg-config blike --libs --cflags` hello.c -o hello

### Haskell (GHC) から 利用する場合の例
samples/ghc 以下に、Haskell (GHC) から blike を利用する場合のサンプルがあります:

        cd samples/ghc
        make clean
        make

このビルドにはghcが必要です。

ここにある blike.hs は、FFI による blike 関数のラッパーです。
これは blike 関数を同名の Haskell 関数としてラップしてある状態です。
ビルドの書式は同梱の Makefile を参考にしてください。

hs_test000a.hs は test000a.c に相当するサンプルです。（ハローワールド）

blike.hs では blike.h で紹介されてる関数のほとんどをラップ済みですが、一部の関数は未実装です。
現在の blike.hs で未実装の関数は bl_printf, bl_scanf, bl_malloc, bl_getGrpB の４つです。

現状、Haskell(GHC)で動作させる場合は、キーボード入力を利用できません。

blikedrv.h で紹介されてるドライバー系関数 (プリフィックスが bld_ の関数) へのラッパーは未実装です。

***
## linux fb 版について
現状、正常に動作しません。
また、メンテナンスされていません。 

***
## オリジナルとは異なる c_blike_01f_linux 固有の機能

### 詳細なキーボード入力
キーボードからの入力を、より詳細に読み取れるので、ゲーム的な操作に向いてます。
各キーを押した瞬間および放した瞬間を、各キー毎に独立して判断することが可能です。
これによって、キーリピートに依存せずに、純粋にキー押し・キー放しのタイミングによる操作を提供できます。
たとえば、ブロック崩しのバーの横移動などを、キーリピートに依存せずに行なえるので、スムーズな動きや操作を提供できます。

使用方法のドキュメントはありません。
ソースを直接読むか、もしくはサンプルコード sample/test_keyboard_state.c を参考にしてください。

### マウス入力のサポート
2 ボタンマウスの各ボタンの押し・放し、およびポインタ移動を利用した操作を提供できます。
これはバッファーに情報を貯める方式ではなく、コールバック関数による方式です。
押し・放し・ポインタ移動、これら３つの場合に、それぞれ対応した関数をコールバック関数として登録しておくことで、
マウス情報に応じた動作を行ないます。

使用方法のドキュメントはありません。
ソースを直接読むか、もしくはサンプルコード sample/mouse00.c を参考にしてください。

### タブレット入力のサポート
現状、GTK4への移行中のため、一時的にタブレット入力のサポートが正常動作しません。

ワコムなどのタブレットによる入力をサポートします。
これはプログラム的には筆圧および傾き情報を持ったマウスとみなせます。使用方法もマウスと同じです。

使用方法のドキュメント化は行なっていません。
ソースを直接読むか、もしくはサンプルコード sample/mouse02.c を参考にしてください。

***
## ai_teto
### ai_teto のコンパイル方法

        cd samples/ai_teto
        make clean
        make

### ai_teto 操作方法

        ←: 右移動
        →: 左移動
        ↓: 下移動
        f: 右回転
        d: 左回転
        スペース: ハードドロップ
        
        q: ゲームをリセット
        
        k: 操作を機械に交代（機械がプレイしてくれます）
        n: 操作を人間に交代（自分でプレイします）
        
        ウインドウを閉じると終了です

