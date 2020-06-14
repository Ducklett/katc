#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "syntax/data.c"
#include "syntax/lexer.c"

typedef struct parser {
	lexer lexer;
	node root;
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
			// badToken already gets reported by the lexer
			if (t.kind != badToken) report_diagnostic(d, unexpectedTokenDiagnostic, t.text_start, t.text_length, t.kind, expectedKind, 0);
			t.kind=errorToken;
		}
		return t;
	}
}

node parser_parse_binary_expression(parser *p, diagnosticContainer *d, int parentPrecedence) {
	node left = parser_match_token(p, d, numberLiteral);

	if (left.kind == endOfFileToken || left.kind == errorToken) return left;

	while (true) {
		node operator = parser_next_token(p, d);

		// not a binary expression, return self
		if (operator.kind == endOfFileToken) return left;

		int precedence = getOperatorPrecedence(operator.kind);

		// not an operator
		if (precedence == -1) {
			report_diagnostic(d, unexpectedTokenDiagnostic, operator.text_start, operator.text_length, operator.kind, endOfFileToken, 0);
			return left;
		}

		if (precedence <= parentPrecedence) {
			// go back to before the operator was lexed
			// TODO: don't re-lex and handle multi-character operators
			p->lexer.index--;
			return left;
		}

		node right = parser_parse_binary_expression(p, d, precedence);

		binaryExpressionNode exprData = { left, operator, right, };

		int index = p->binaryExpressionIndex;
		p->binaryExpressions[p->binaryExpressionIndex++] = exprData;

		node exprNode = {
			.kind = binaryExpression,
			.text_start = left.text_start,
			.text_length = (right.text_start - left.text_start) + right.text_length,
			.data = &(p->binaryExpressions[index]), 
		};

		left = exprNode;
	}

	return left;
}

parser_parse(parser *p, diagnosticContainer *d) {
	p->root = parser_parse_binary_expression(p, d,-2);
	parser_match_token(p, d, endOfFileToken);
}

void print_ast(char *text, node *root, int indent) {
	if (root->data == 0) {
		char *tokenText = (char*)malloc(sizeof(char) * (root->text_length) + 1);
		tokenText[root->text_length] = '\0';
		strncpy(tokenText, text + root->text_start, sizeof(char) * root->text_length);

		printf ("%*s(%s :: %s)\n", indent, "", tokenText, syntaxKindText[root->kind]);

		free(tokenText);
	} else {
		printf ("%*s(%s\n", indent, "", syntaxKindText[root->kind]);

		// binary expression
		binaryExpressionNode n = (binaryExpressionNode)*root->data;
		print_ast(text, &(n.left), indent + 4);
		print_ast(text, &n.operator, indent + 4);
		print_ast(text, &n.right, indent + 4);

		printf ("%*s)\n", indent, "");
	}
}

int main()
{
	char text[] = "10 + 10 * 20 + 2 * 20 + 10 * 50 * 60 % 30";

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
		print_ast(text, &p.root, 0);
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
