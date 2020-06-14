#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "syntax/data.c"
#include "syntax/lexer.c"

typedef struct parser {
	lexer lexer;
	node syntaxNodes[1024];
	binaryExpressionNode binaryExpressions[1024];
	int nodeIndex;
	int binaryExpressionIndex;
} parser;

node parser_next_token(parser *p, diagnosticContainer *d) {
	while(true) {
		node t = lexer_lex_token(&p->lexer, d);
		if (t.kind == whitespaceToken || t.kind == newlineToken) continue;
		return t;
	}
}

node parser_match_token(parser *p, diagnosticContainer *d, enum syntaxKind expectedKind) {
	while(true) {
		node t = lexer_lex_token(&p->lexer, d);
		if (t.kind == whitespaceToken || t.kind == newlineToken) continue;
		if (t.kind != expectedKind) {
			report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, expectedKind, 0);
			t.kind=errorToken;
		}
		return t;
	}
}

parser_parse(parser *p, diagnosticContainer *d) {
	node t;
	while(true) {
		t = parser_next_token(&p->lexer, d);

		if (t.kind == endOfFileToken) break;

		if (t.kind == numberLiteral) {
			node operator = parser_next_token(&p->lexer, d);

			// not a binary expression, return self
			if (t.kind == endOfFileToken) {
				p->syntaxNodes[p->nodeIndex++] = t;
				return;
			}

			int precedence = getOperatorPrecedence(operator.kind);

			// not an operator
			if (precedence == -1) {
				report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, plusOperator, 0);
			}

			node right = parser_match_token(&p->lexer, d, numberLiteral);

			binaryExpressionNode exprData = {
				.left = t,
				.operator = operator,
				.right = right,
			};

			p->binaryExpressions[p->binaryExpressionIndex++] = exprData;

			node exprNode = {
				.kind = binaryExpression,
				.text_start = t.text_start,
				.text_length = (right.text_start-t.text_start)+right.text_length,
				.data = &exprData, 
			};

			p->syntaxNodes[p->nodeIndex++] = exprNode;
		} else {
			// bad tokens are already reported by the lexer
			if (t.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, numberLiteral, 0);
		}
	}
}

int main()
{
	char text[] = "10 + 20";

	lexer l = {
		.text = text,
		.text_length = sizeof(text),
		.index = 0,
	};

	parser p = {
		.lexer = l,
		.nodeIndex = 0,
		.binaryExpressionIndex = 0,
	};

	diagnosticContainer diagnostics = {0};

	printf("Input: %s\n", text);
	parser_parse(&p, &diagnostics);


	if (diagnostics.index==0) {
		for (int i=0;i<p.nodeIndex;i++) {
			node t = p.syntaxNodes[i];
			char *tokenText = (char *)malloc(sizeof(char) * (t.text_length) + 1);
			tokenText[t.text_length] = '\0';
			strncpy(tokenText, text + t.text_start, sizeof(char) * t.text_length);

			printf("{ kind: %s, text: '%s' }\n", syntaxKindText[t.kind], tokenText);
		}
	} else {
		for (int i = 0; i < diagnostics.index; i++)
		{
			diagnostic d = diagnostics.diagnostics[i];
			switch (d.kind) {
				case badTokenDiagnostic: printf(diagnosticText[d.kind], text[d.start], d.start, d.length); break;
				case unexpectedTokenDiagnostic: printf(diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.start, d.length); break;
			}
		}
	}

	return 0;
}
