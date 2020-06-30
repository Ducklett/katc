static const char *diagnosticText[] = {
	"Unexpected character '%c', expected '%c' (%d,%d)\n",
	"bad token '%c' (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
	"error parsing primary expression: '%s' is not valid here. (%d,%d)\n",
	"undefined unary operator '%s' for value of type '%s' (%d,%d)\n",
	"undefined binary operator '%s' for values of type '%s' and '%s' (%d,%d)\n",
	"variable '%s' is already declared in this scope. (%d,%d)\n",
	"variable '%s' is undefined. (%d,%d)\n",
	"cannot assign expression of type '%s' to variable '%s' of type '%s'. (%d,%d)\n",
	"variable '%s' cannot be void. (%d,%d)\n",
	"cannot convert expression of type '%s' to the expected type '%s'. (%d,%d)\n",
	"unresolved type '%s'. (%d,%d)\n",
	"case statements should have at least one branch. (%d,%d)\n",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u32 param1, u32 param2, u32 param3) {
	if (d->index >= 10) return;

	diagnostic dia = { kind, span, param1, param2, param3, };
	d->diagnostics[d->index++] = dia;
}

void print_diagnostics(diagnosticContainer *diagnostics, char* sourceText) {
	for (int i = 0; i < diagnostics->index; i++) {
		diagnostic d = diagnostics->diagnostics[i];
		fprintf(stderr, TERMRED);
		switch (d.kind) {
		case unexpectedCharacterDiagnostic: {
			char param1 = d.param1;
			if (param1 == '\r' || param1 == '\n' || param1 == '\0') param1 = ' ';
			fprintf(stderr, diagnosticText[d.kind], param1, d.param2, d.span.start, d.span.length); 
		} break;
		case badTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case unexpectedTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.span.start, d.span.length); break;
		case illegalPrimaryExpressionDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], d.span.start, d.span.length); break;
		case undefinedUnaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		case undefinedBinaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astTypeText[d.param2], astTypeText[d.param3], d.span.start, d.span.length); break;
		case redeclarationOfVariableDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1, d.span.start, d.span.length); break;
		case referenceToUndefinedVariableDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span);
			fprintf(stderr, diagnosticText[d.kind], identifierText, d.span.start, d.span.length);
			free(identifierText);
		} break;
		case cannotAssignDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span);
			fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param2], identifierText, astTypeText[d.param1], d.span.start, d.span.length);
			free(identifierText);
		} break;
		case variableCannotBeVoidDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span);
			fprintf(stderr, diagnosticText[d.kind], identifierText, d.span.start, d.span.length);
			free(identifierText);
		} break;
		case cannotConvertDiagnostic: fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		case unresolvedTypeDiagnostic: {
			char* typeText = ast_substring(sourceText, d.span);
			fprintf(stderr, diagnosticText[d.kind], typeText, d.span.start, d.span.length);
			free(typeText);
		} break;
		case emptyCaseStatementDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		default: {
			fprintf(stderr, "Unhandled case %s in print_diagnostics%s\n", diagnosticMetaText[d.kind], TERMRESET);
			exit(1);
		}
		}
		fprintf(stderr, TERMRESET);

		int lineStart;
		for (lineStart = d.span.start; sourceText[lineStart] != '\n' && lineStart > 0; lineStart--) {}
		for (int i=lineStart;i<d.span.start;i++) fprintf(stderr, "%c", sourceText[i]);
		for (int i=0;i<d.span.length;i++) fprintf(stderr, "%s%c%s", TERMRED, sourceText[d.span.start + i], TERMRESET);
		for (int i=(d.span.start + d.span.length); sourceText[i] != '\n' && sourceText[i] != '\0';i++) fprintf(stderr, "%c", sourceText[i]);
		fprintf(stderr, "\n");
	}
}
