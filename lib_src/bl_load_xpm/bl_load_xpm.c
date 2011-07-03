// copyright 2011 Takeutch Kemeco
// license KL-01



#include "blike.h"	// setPix(), bl_malloc(), bl_free(), printf()

// bl_malloc(), bl_free() が01eに無いので
extern void* bld_malloc(unsigned int bytes);
extern void bld_free(void* p, unsigned int bytes);

static
void bl_load_xpm_memcopy(void* dst, void* src, unsigned int len)
{
	int i = len;
	unsigned char* p = src;
	unsigned char* q = dst;
	
	while(i-->0) {
		*q++ = *p++;
	}
}





// bl_load_xpm() 以下の、一連の処理の際に用いるワーキングメモリー
struct bl_load_xpm_XPM {
	unsigned int width;
	unsigned int height;

	unsigned int* pallete;
	unsigned int pallete_len;

	char* ch;
	unsigned int ch_len;
};






// struct bl_load_xpm_XPM の内容を確認するため
// デバッグ用
static
void bl_load_xpm_print_xpm(struct bl_load_xpm_XPM* a)
{
	printf(
		"width=[%d], heght=[%d], pallete_len=[%d], ch_len=[%d]\n",
		a->width,
		a->height,
		a->pallete_len,
		a->ch_len
	);
}






// xpm_str からヘッダー部分を読み出す際に用いる関数群

// 文字0~9が続く間、１０進数として読み取る。（結果は a への参照渡し）
// 文字0~9以外が登場した時点のポインタを返す。
static
char* bl_load_xpm_atoi10(unsigned int* a, char* str)
{
	*a = 0;
	
	while(1) {
		if(*str >= '0' && *str <='9') {
			*a *= 10;
			*a += *str - '0';
		}
		else {
			return str;
		}
		
		str++;
	}
}

// 文字0~9が登場しない間、ポインタを進める。
// 文字0~9が登場した時点のポインタを返す。
static
char* bl_load_xpm_next_str(char* str)
{
	while(1) {
		if(*str >= '0' && *str <= '9') {
			return str;
		}
		
		str++;
	}
}

// xpm_str からヘッダー部分を数値型に変換して struct bl_load_xpm_XPM に保存する。
static
void bl_load_xpm_read_header(struct bl_load_xpm_XPM* a, char** xpm_str)
{
	char* p = xpm_str[0];
	
	p = bl_load_xpm_next_str(p);
	p = bl_load_xpm_atoi10(&(a->width), p);  

	p = bl_load_xpm_next_str(p);
	p = bl_load_xpm_atoi10(&(a->height), p);

	p = bl_load_xpm_next_str(p);
	p = bl_load_xpm_atoi10(&(a->pallete_len), p);

	p = bl_load_xpm_next_str(p);
	p = bl_load_xpm_atoi10(&(a->ch_len), p);
}






// xpm_str からパレット部分を読み出す際に用いる関数群

// 文字 0~9 or a~f が続く間、１６進数として読み取る。（結果は a への参照渡し）
// 文字 0~9 or a~f 以外が登場した時点のポインタを返す。
static
char* bl_load_xpm_atoi16(unsigned int* a, char* str)
{
	*a = 0;
	
	while(1) {
		if(*str >= '0' && *str <='9') {
			*a *= 16;
			*a += *str - '0';
		}
		else if(*str >= 'a' && *str <='f') {
			*a *= 16;
			*a += 10 + (*str - 'a');
		}
		else if(*str >= 'A' && *str <='F') {
			*a *= 16;
			*a += 10 + (*str - 'A');
		}
		else {
			return str;
		}
		
		str++;
	}
}

// ch_len 文字分だけ、 str から ch へ文字をコピーする。
// 備考：最後に '\0' は付加しないので注意。
static
void bl_load_xpm_read_ch(char* ch, unsigned int ch_len, char* str)
{
	int i = 0;
	while(i < ch_len) {
		ch[i] = str[i];
		
		i++;
	}
}

// str の先頭から ch_len 文字分スキップし、
// 最初に登場した'#'以降の文字列を１６進数として読み取る。（結果はpalleteへの参照渡し）
// 備考：'#'が登場しないなど、エラーの場合は、0x000000（黒）とする。
static
void bl_load_xpm_read_pallete(unsigned int* pallete, unsigned int ch_len, char* str)
{
	char* p = str + ch_len;
	
	while(*p != '#') {
		if(*p == 0) {
			*pallete = 0x000000;
			return;
		}
		
		p++;
	}

	p++;
	bl_load_xpm_atoi16(pallete, p);
}

// xpm_str のヘッダをスキップし、
// pallete_len 行分、パレットを読み取る。
static
void bl_load_xpm_read_pallete_all(struct bl_load_xpm_XPM* a, char** xpm_str)
{
	int i = 0;
	char** p = xpm_str + 1;
	
	char* cur_ch = a->ch;
	unsigned int* cur_pallete = a->pallete;
	
	while(i < a->pallete_len) {
		bl_load_xpm_read_ch(cur_ch, a->ch_len, p[i]);
		bl_load_xpm_read_pallete(cur_pallete, a->ch_len, p[i]);
		
		cur_ch += a->ch_len;
		cur_pallete++;
		
		i++;
	}
}






// 置換を高速に行うためにピクセル部、パレット部の値をハッシュ値に変換して検索に用いるための関数群。



// strからハッシュ値を得る
// hash_len は、ハッシュの最大数。ただし、マスクに用いるので ((1 << x) -1) の数でなければならない。（たとえば 0xFFFFなど）
static
unsigned int bl_load_xpm_calc_hash(char* str, unsigned int len, unsigned int hash_len)
{
	unsigned int hash = 0;
	
	int i = 0;
	while(i < len) {
		hash += ((unsigned int)(str[i])) << i;
		
		i++;
	}
	
	return hash & hash_len;
}



// HashUnitに関する操作のための関数群。

// 一つのハッシュ値に対して、数値群を対応させる
struct bl_load_xpm_HashUnit {
	unsigned int** pallete_list;
	char** ch_list;
	
	unsigned int cur_list_len;
	unsigned int list_len;
};

// list_len個まで要素と関連づけられるHashUnitをメモリー確保する
static
struct bl_load_xpm_HashUnit* bl_load_xpm_new_HashUnit(unsigned int list_len)
{
	struct bl_load_xpm_HashUnit* a = (struct bl_load_xpm_HashUnit*)bld_malloc(sizeof(*a));
	a->list_len = list_len;
	a->cur_list_len = 0;
	a->pallete_list = (unsigned int**)bld_malloc(sizeof(unsigned int*) * a->list_len);
	a->ch_list = (char**)bld_malloc(sizeof(char*) * a->list_len);
	
	return a;
}

// HashUnitのメモリーを開放する
static
void bl_load_xpm_free_HashUnit(struct bl_load_xpm_HashUnit* a)
{
	bld_free((void*)(a->ch_list), sizeof(char*) * a->list_len);
	bld_free((void*)(a->pallete_list), sizeof(unsigned int*) * a->list_len);
	bld_free((void*)a, sizeof(*a));
}

// HashUnitに関連付けられる要素数を２倍にする
static
struct bl_load_xpm_HashUnit* bl_load_xpm_realloc_HashUnit(struct bl_load_xpm_HashUnit* a)
{
	struct bl_load_xpm_HashUnit* new_a = bl_load_xpm_new_HashUnit((a->list_len) << 1);

	new_a->cur_list_len = a->cur_list_len; 
	
	bl_load_xpm_memcopy(
		(void*)(new_a->ch_list),
		(void*)(a->ch_list),
		sizeof(char*) * a->cur_list_len
	);
	
	bl_load_xpm_memcopy(
		(void*)(new_a->pallete_list),
		(void*)(a->pallete_list),
		sizeof(unsigned int*) * a->cur_list_len
	);
	
	bl_load_xpm_free_HashUnit(a);
	
	return new_a;
}

// HashUnitに新たな要素を関連付ける
static
void bl_load_xpm_push_HashUnit(struct bl_load_xpm_HashUnit* a, char* ch, unsigned int* pallete)
{
	if((a->cur_list_len + 1) >= a->list_len) {
		bl_load_xpm_realloc_HashUnit(a);
	}
	
	
	a->ch_list[a->cur_list_len] = ch;
	a->pallete_list[a->cur_list_len] = pallete;
	
	a->cur_list_len++;
}



// HashListに関する操作のための関数群。

// HashUnitの配列
struct bl_load_xpm_HashList {
	struct bl_load_xpm_HashUnit** unit;
	unsigned int hash_len;
};

// hash_len個の HashUnitの配列で構成された HashListを得る
// 備考：hash_len はマスクに用いるので ((1 << x) -1) の数でなければならない。（たとえば 0xFFFFなど）
static
struct bl_load_xpm_HashList* bl_load_xpm_new_HashList(unsigned int hash_len)
{
	struct bl_load_xpm_HashList* a = (struct bl_load_xpm_HashList*)bld_malloc(sizeof(*a));
	a->hash_len = hash_len;
	
	a->unit = (struct bl_load_xpm_HashUnit**)bld_malloc(sizeof(*(a->unit)) * hash_len);
	
	struct bl_load_xpm_HashUnit** p = a->unit;
	int i = 0;
	while(i < hash_len) {
		p[i] = bl_load_xpm_new_HashUnit(0x10);	// デフォルトで unit あたり 要素0x10個分のメモリーを確保
		
		i++;
	}
	
	return a;
}

// HashList のメモリを開放
static
void bl_load_xpm_free_HashList(struct bl_load_xpm_HashList* a)
{
	struct bl_load_xpm_HashUnit** p = a->unit;
	int i = 0;
	while(i < a->hash_len) {
		bl_load_xpm_free_HashUnit(p[i]);
		
		i++;
	}
	
	bld_free((void*)(a->unit), sizeof(*(a->unit)) * a->hash_len);
	bld_free((void*)a, sizeof(*a));
}

// HashList へ、 XPM の ch, pallete を関連付ける
static
void bl_load_xpm_push_HashList(struct bl_load_xpm_HashList* a, struct bl_load_xpm_XPM* xpm)
{
	char* cur_ch = xpm->ch;
	unsigned int* cur_pallete = xpm->pallete;
	
	int i = 0;
	while(i < xpm->pallete_len) {
		unsigned int hash = bl_load_xpm_calc_hash(cur_ch, xpm->ch_len, a->hash_len);
		bl_load_xpm_push_HashUnit(a->unit[hash], cur_ch, cur_pallete);
		
		cur_ch += xpm->ch_len;
		cur_pallete++;
		
		i++;
	}
}






// xpm_str のピクセル部分をパレット数値へ置換する処理に用いる関数群

// 文字列 a と b を len 文字分だけ比較し、一致したら1(true)を返す。（異なる場合は0(false)を返す）
// 備考：標準関数のstrncmp()とは戻り値が逆なので注意。
static
int bl_load_xpm_strncmp(char* a, char* b, int len)
{
	int i = 0;
	while(i < len) {
		if(*a++ != *b++) {
			return 0;
		}
		
		i++;
	}
	
	return 1;
}

// str から ch_len文字分読み込んで、対応するパレットを検索して、x,y位置に色を描画する。
static
void bl_load_xpm_put_pixel(struct bl_load_xpm_HashList* a, struct bl_load_xpm_XPM* xpm, char* str, int x, int y)
{
	unsigned int hash = bl_load_xpm_calc_hash(str, xpm->ch_len, a->hash_len);

	char** cur_ch = a->unit[hash]->ch_list;
	unsigned int** cur_pallete = a->unit[hash]->pallete_list;
	
	int i = 0;
	while(i < a->unit[hash]->cur_list_len) {
		if(bl_load_xpm_strncmp(str, cur_ch[i], xpm->ch_len) == 1) {
			setPix(x, y, *(cur_pallete[i]));
			return;
		}
		
		i++;
	}
}

// str から ch_len毎に文字を読み込んで、対応するパレットを検索して、１ライン分描画する。
static
void bl_load_xpm_put_pixel_line(struct bl_load_xpm_HashList* a, struct bl_load_xpm_XPM* xpm, char* str, int offset_x, int offset_y)
{
	char* p = str;
	int i = 0;
	while(i < xpm->width) {
		bl_load_xpm_put_pixel(a, xpm, p, offset_x + i, offset_y);

		p += xpm->ch_len;
		i++;
	}
}

// xpm_str のピクセルデータと対応するパレットを検索し、画像を描画する。
static
void bl_load_xpm_put_pixel_all(struct bl_load_xpm_HashList* a, struct bl_load_xpm_XPM* xpm, char** xpm_str, int offset_x, int offset_y)
{
	char** p = xpm_str + 1 + xpm->pallete_len;
	int i = 0;
	while(i < xpm->height) {
		bl_load_xpm_put_pixel_line(a, xpm, p[i], offset_x, offset_y + i);
		
		i++;
	}
}






void bl_load_xpm(char** xpm_str, int x, int y)
{
	struct bl_load_xpm_XPM* xpm = (struct bl_load_xpm_XPM*)bld_malloc(sizeof(*xpm));
	
	bl_load_xpm_read_header(xpm, xpm_str);
	xpm->ch		= (char*)bld_malloc((sizeof(char) * (xpm->ch_len + 1)) * xpm->pallete_len);
	xpm->pallete	= (unsigned int*)bld_malloc(sizeof(unsigned int) * xpm->pallete_len);
	

	bl_load_xpm_read_pallete_all(xpm, xpm_str);
	
	struct bl_load_xpm_HashList* hash_list = bl_load_xpm_new_HashList(0xFFFF);
	bl_load_xpm_push_HashList(hash_list, xpm);
	
	
	bl_load_xpm_put_pixel_all(hash_list, xpm, xpm_str, x, y);

	
	bl_load_xpm_free_HashList(hash_list);

	bld_free((void*)(xpm->pallete), sizeof(unsigned int) * xpm->pallete_len);
	bld_free((void*)(xpm->ch), (sizeof(char) * (xpm->ch_len + 1)) * xpm->pallete_len);
	bld_free((void*)xpm, sizeof(*xpm));
}



