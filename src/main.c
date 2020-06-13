#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "syntax/data.c"
#include "syntax/lexer.c"

int main()
{
	char text[] = "10 + 69420";

	lexer l = {
		.text = text,
		.text_length = sizeof(text),
		.index = 0,
	};

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);
	while (l.index < l.text_length)
	{
		token t = lexer_lex_token(&l, &diagnostics);

		char *tokenText = (char *)malloc(sizeof(char) * (t.text_length) + 1);
		tokenText[t.text_length] = '\0';
		strncpy(tokenText, text + t.text_start, sizeof(char) * t.text_length);

		printf("{ kind: %s, text: %s }\n", syntaxKindText[t.kind], tokenText);
	}

	for (int i = 0; i < diagnostics.index; i++)
	{
		diagnostic d = diagnostics.diagnostics[i];
		printf(diagnosticText[d.kind], text[d.start], d.start, d.length);
	}
	return 0;
}
