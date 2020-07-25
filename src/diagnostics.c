static const char *diagnosticText[] = {
	"Unexpected character '%c', expected '%c' (%d,%d)\n",
	"unterminated comment, exprected */ (%d,%d)\n",
	"bad token '%c' (%d,%d)\n",
	"character literals cannot be empty (%d,%d)\n",
	"character literals cannot contain more than one character (%d,%d)\n",
	"number literals should not have leading zeroes (%d,%d)\n",
	"'%c' is an invalid hexadecimal number (%d,%d)\n",
	"'%c' is an invalid binary number (%d,%d)\n",
	"unexpected token of kind '%s', expected '%s' (%d,%d)\n",
	"unexpected token of kind '%s', expected an assignment operator (=, +=, -=, *=, /=, %=, <<=, >>=, &=, ^=, |=) (%d,%d)\n",
	"'%s' cannot be used in %s. (%d,%d)\n",
	"a value of type '%s' is not legal in a range expression. (%d,%d)\n",
	"unexpected token '%s' while parsing primary expression. (%d,%d)\n",
	"increment/decrement operators can only be used on variables. (%d,%d)\n",
	"statements of this kind are not allowed here. (%d,%d)\n",
	"only constant expressions are allowed here. (did you reference a variable?) (%d,%d)\n",
	"value is too %s for %s and will %s, use an explicit cast if this is intentional. (%d,%d)\n",
	"undefined unary operator '%s' for value of type '%s' (%d,%d)\n",
	"undefined binary operator '%s' for values of type '%s' and '%s' (%d,%d)\n",
	"variable '%s' is not initialized. (%d,%d)\n",
	"symbol '%s' is already declared in this scope. (%d,%d)\n",
	"symbol '%s' is undefined. (%d,%d)\n",
	"cannot assign expression of type '%s' to variable '%s' of type '%s'. (%d,%d)\n",
	"'%s' is constant and cannot be assigned to. (%d,%d)\n",
	"variable '%s' cannot be void. (%d,%d)\n",
	"unresolved type '%s'. (%d,%d)\n",
	"case statements should have at least one branch. (%d,%d)\n",
	"cast should take exactly one argument. (%d,%d)\n",
	"cast from '%s' to '%s' doesn't exist. (%d,%d)\n",
	"cannot implicitly convert from '%s' to '%s', an explicit conversion exists. (are you missing a cast?) (%d,%d)\n",
	"a value of type '%s' cannot be used in switch statements. (%d,%d)\n",
	"duplicate value in switch case. (%d,%d)\n",
	"argument count does not match in call to '%s'. (%d,%d)\n",
};

void report_diagnostic(diagnosticContainer *d, enum diagnosticKind kind, textspan span, u64 param1, u64 param2, u64 param3) {
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
		case unterminatedCommentDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case badTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case charEmptyDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case charTooLongDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case leadingZerosOnBase10NumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case invalidHexadecimalNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case invalidBinaryNumberDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], sourceText[d.span.start], d.span.start, d.span.length); break;
		case unexpectedTokenDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.span.start, d.span.length); break;
		case notAnAssignmentOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], d.span.start, d.span.length); break;
		case notAllowedInContextDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], syntaxKindText[d.param2], d.span.start, d.span.length); break;
		case illegalRangeDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param1], d.span.start, d.span.length); break;
		case illegalPrimaryExpressionDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], d.span.start, d.span.length); break;
		case illegalIncrementOrDecrementDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case statementNotAllowedHereDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case nonConstantDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case valueOutOfBoundsDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param2 ? "big" : "small", astTypeText[d.param1], d.param2 ? "overflow" : "underflow", d.span.start, d.span.length); break;
		case undefinedUnaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		case undefinedBinaryOperatorDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], syntaxKindText[d.param1], astTypeText[d.param2], astTypeText[d.param3], d.span.start, d.span.length); break;
		case variableNotInitializedDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1, d.span.start, d.span.length); break;
		case redeclarationOfSymbolDiagnostic:
			fprintf(stderr, diagnosticText[d.kind], d.param1, d.span.start, d.span.length); break;
		case referenceToUndefinedVariableDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, d.span.start, d.span.length);
			free(identifierText);
		} break;
		case cannotAssignDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param2], identifierText, astTypeText[d.param1], d.span.start, d.span.length);
			free(identifierText);
		} break;
		case cannotAssignConstantDiagnostic: {
			char* identifierText = ast_substring(sourceText, *(textspan*)d.param1, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, d.span.start, d.span.length);
			free(identifierText);
		} break;
		case variableCannotBeVoidDiagnostic: {
			char* identifierText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], identifierText, d.span.start, d.span.length);
			free(identifierText);
		} break;
		case unresolvedTypeDiagnostic: {
			char* typeText = ast_substring(sourceText, d.span, NULL);
			fprintf(stderr, diagnosticText[d.kind], typeText, d.span.start, d.span.length);
			free(typeText);
		} break;
		case emptyCaseStatementDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case oneArgumentCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case illegalCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		case illegalImplicitCastDiagnostic: fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param1], astTypeText[d.param2], d.span.start, d.span.length); break;
		case invalidSwitchTypeDiagnostic: fprintf(stderr, diagnosticText[d.kind], astTypeText[d.param1], d.span.start, d.span.length); break;
		case duplicateSwitchValueDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.span.start, d.span.length); break;
		case argCountDoensntMatchDiagnostic: fprintf(stderr, diagnosticText[d.kind], d.param1, d.span.start, d.span.length); break;
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
