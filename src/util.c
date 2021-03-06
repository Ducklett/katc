typedef char i8;
typedef short i16;
typedef int i32;
typedef long i64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

// already defined on tcc
#undef CHAR_MAX
#undef CHAR_MIN
#undef INT_MAX
#undef INT_MIN

#define CHAR_MAX 255
#define CHAR_MIN 0
#define INT_MAX +2147483647
#define INT_MIN -2147483648

#define I8_MAX  +127
#define I8_MIN  -128
#define I16_MAX +32767
#define I16_MIN -32768
#define I32_MAX +2147483647
#define I32_MIN -2147483648
#define I64_MAX +9223372036854775807
#define I64_MIN -9223372036854775808

#define U8_MAX  255
#define U8_MIN  0
#define U16_MAX 65535
#define U16_MIN 0
#define U32_MAX 4294967295
#define U32_MIN 0
#define U64_MAX 18446744073709551615
#define U64_MIN 0

#define TERMRED "\033[0;31m"
#define TERMBOLDRED "\033[1;31m"
#define TERMGREEN "\033[0;32m"
#define TERMBOLDGREEN "\033[1;32m"
#define TERMYELLOW "\033[0;33m"
#define TERMBOLDYELLOW "\033[01;33m"
#define TERMBLUE "\033[0;34m"
#define TERMBOLDBLUE "\033[1;34m"
#define TERMMAGENTA "\033[0;35m"
#define TERMBOLDMAGENTA "\033[1;35m"
#define TERMCYAN "\033[0;36m"
#define TERMBOLDCYAN "\033[1;36m"
#define TERMRESET "\033[0m"

#if BENCHMARK
#define benchmark_start() clock_t t = clock();
#define benchmark_end(name) t = clock() - t; fprintf(stderr, "%s%s: %.0fms%s\n", TERMGREEN, name, ((double)t) / CLOCKS_PER_SEC * 1000, TERMRESET); 
#else
#define benchmark_start() {}
#define benchmark_end(name) {}
#endif

void panic(const char *message, ...) {
	fprintf(stderr, "%s", TERMRED);

    va_list args;
    va_start(args, message);
    vfprintf(stderr, message, args);
    va_end(args);

	fprintf(stderr, "%s\n", TERMRESET);
	exit(1);
}

char* string_concat(const char *s1, const char *s2)
{
	char *result = malloc(strlen(s1) + strlen(s2) + 1);
	if (result == NULL) panic("memory allocation failed\n");
	strcpy(result, s1);
	strcat(result, s2);
	return result;
}

char* read_file(const char* filename, u64* length) {
	char * buffer = 0;
	FILE * f = fopen (filename, "rb");

	if (f) {
		fseek (f, 0, SEEK_END);
		*length = ftell (f);
		fseek (f, 0, SEEK_SET);
		buffer = malloc (*length+1);
		if (buffer == NULL) panic("memory allocation failed");
		if (buffer) {
			fread (buffer, 1, *length, f);
		}
		fclose (f);
		buffer[*length] = '\0';
	}

	if (!buffer) panic("Failed to read file: %s", filename);

	return buffer;
}

char* escape_string_c(char *str) {
	int escapedCharacters = 0;
	int i = 0;
	bool endFound = false;
	while (!endFound) {
		switch(str[i]) {
			case '\0': endFound=true; break;

			case '\r':
			case '\n':
			case '\\':
			case '\"':
				escapedCharacters++; /* FALLTHROUGH */
		
			default: i++;
		}
	}

	int length = i + escapedCharacters + 1;
	int oldlength = i;

	char *allocatedText = (char*)malloc(sizeof(char) * length);
	if (allocatedText == NULL) panic("memory allocation failed");

	int index = 0;
	for (int j=0; j < oldlength; j++) {
		switch(str[j]) {
			case '\r':
				allocatedText[index++] = '\\';
				allocatedText[index++] = 'r';
				break;
			case '\n':
				allocatedText[index++] = '\\';
				allocatedText[index++] = 'n';
				break;
			case '\\':
				allocatedText[index++] = '\\';
				allocatedText[index++] = '\\';
				break;
			case '\"':
				allocatedText[index++] = '\\';
				allocatedText[index++] = '"';
				break;
		
			default: allocatedText[index++] = str[j];
		}
	}

	allocatedText[length-1] = '\0';
	return allocatedText;
}

// (temporary) globals

bool feature_constantfolding = true;

arena_t* string_arena = NULL;
arena_t* parser_arena = NULL;
arena_t* binder_arena = NULL;