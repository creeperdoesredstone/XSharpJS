const isDigit = (ch) => ch >= "0" && ch <= "9";
const isLetter = (ch) => (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z");
const isAlphaNumeric = (ch) => isDigit(ch) || isLetter(ch);

const KEYWORDS = [
	"const",
	"var",
	"for",
	"start",
	"end",
	"step",
	"while",
	"if",
	"elseif",
	"else",
	"include",
	"sub",
];
const DATA_TYPES = ["int", "bool"];

const TT = {
	LT: 0,
	LE: 1,
	EQ: 2,
	NE: 3,
	GT: 4,
	GE: 5,
	ADD: 6,
	SUB: 7,
	INC: 8,
	DEC: 9,
	AND: 10,
	OR: 11,
	NOT: 12,
	XOR: 13,
	LPR: 14,
	RPR: 15,
	LBR: 16,
	RBR: 17,
	LSQ: 18,
	RSQ: 19,
	COL: 20,
	ASSIGN: 21,
	COMMA: 22,
	MUL: 23,
	DIV: 24,
	RSHIFT: 25,
	LSHIFT: 26,
	ABS: 27,
	SIGN: 28,
	AT: 29,
	NUM: 30,
	IDENTIFIER: 31,
	KEYWORD: 32,
	NEWLINE: 33,
	ADDTO: 34,
	SUBBY: 35,
	EOF: 36,

	str: (val) => {
		return TT_STR[val] ?? "UNKNOWN";
	},
};

const TT_STR = Object.fromEntries(
	Object.entries(TT).map(([key, val]) => [val, key])
);

class Position {
	constructor(index, line, col, fn, ftxt) {
		this.index = index;
		this.line = line;
		this.col = col;
		this.fn = fn;
		this.ftxt = ftxt;
	}

	advance(currentChar) {
		this.index += 1;
		this.col += 1;

		if (currentChar === "\n") {
			this.line += 1;
			this.col = 0;
		}

		return this;
	}

	copy() {
		return new Position(
			this.index,
			this.line,
			this.col,
			this.fn,
			this.ftxt
		);
	}
}

const errorWithArrows = (ftxt, startPos, endPos) => {
	const lines = ftxt.split("\n");
	const startLine = startPos.line;
	const endLine = endPos.line;

	const relevantLines = lines.slice(startLine, endLine + 1);

	const arrows = relevantLines.map((line, idx) => {
		let arrowLine = "";
		if (idx === 0 && startLine === endLine) {
			arrowLine =
				" ".repeat(startPos.col) +
				"^".repeat(endPos.col - startPos.col + 1);
		} else if (idx === 0) {
			arrowLine =
				" ".repeat(startPos.col) +
				"^".repeat(line.length - startPos.col);
		} else if (idx === relevantLines.length - 1) {
			arrowLine = "^".repeat(endPos.col + 1);
		} else {
			arrowLine = "^".repeat(line.length);
		}
		return arrowLine;
	});

	return relevantLines.join("\n") + "\n" + arrows.join("\n");
};

class CustomError {
	constructor(startPos, endPos, errorName, details) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.errorName = errorName;
		this.details = details;
	}

	toString() {
		return `File ${this.startPos.fn}, line ${
			this.startPos.line + 1
		}, column ${this.startPos.col + 1}:\n\n${errorWithArrows(
			this.startPos.ftxt,
			this.startPos,
			this.endPos
		)}\n\n${this.errorName}: ${this.details}`;
	}
}

class UnknownImport extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "UnknownImport", details);
	}
}

class UnknownCharacter extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "UnknownCharacter", details);
	}
}

class CodingError extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "CodingError", details);
	}
}

class CustomTypeError extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "TypeError", details);
	}
}

class SymbolError extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "SymbolError", details);
	}
}

class InvalidSyntax extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "InvalidSyntax", details);
	}
}

class ExpectedCharacter extends CustomError {
	constructor(startPos, endPos, details) {
		super(startPos, endPos, "ExpectedCharacter", details);
	}
}

class Token {
	constructor(startPos, endPos, tokenType, value = null) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.tokenType = tokenType;
		this.value = value;
	}

	toString() {
		return this.value !== null && this.value !== undefined
			? `${TT.str(this.tokenType)}:${this.value}`
			: `${TT.str(this.tokenType)}`;
	}

	equals(other) {
		if (!(other instanceof Token)) return false;
		return this.tokenType === other.tokenType && this.value === other.value;
	}

	notEquals(other) {
		return !this.equals(other);
	}
}

class CompileResult {
	constructor() {
		this.value = null;
		this.error = null;
	}

	register(res) {
		if (res.error) this.error = res.error;
		return res.value;
	}

	success(value) {
		this.value = value;
		return this;
	}

	fail(error) {
		this.error = error;
		return this;
	}
}

class Lexer {
	constructor(fn, ftxt) {
		this.fn = fn;
		this.ftxt = ftxt;

		this.pos = new Position(-1, 0, -1, fn, ftxt);
		this.libraries = [];
		this.currentChar = null;
		this.advance();
	}

	advance() {
		this.pos.advance(this.currentChar);
		this.currentChar =
			this.pos.index >= this.ftxt.length
				? null
				: this.ftxt[this.pos.index];
	}

	processIncludes() {
		const textLines = this.ftxt.split(/[\;\n]/);
		const res = new CompileResult();

		let libraries = [];

		textLines.forEach((line) => {
			let codeLine = line.split("//")[0].trim();
			if (codeLine.startsWith("include ")) {
				libraries.push(line.slice(8));
			}
		});

		libraries.forEach((lib) => {
			if (lib == "operations") {
				this.libraries.push("operations");
			} else {
				const index = this.ftxt.indexOf(lib);
				const lineCount = (this.ftxt.slice(0, index).match(/\n/g) || [])
					.length;
				const startPos = new Position(
					index,
					lineCount,
					8,
					this.fn,
					this.ftxt
				);
				const endPos = new Position(
					index + lib.length,
					lineCount,
					8 + lib.length,
					this.fn,
					this.ftxt
				);

				return res.fail(new UnknownImport(startPos, endPos, lib));
			}
		});

		this.libraries = libraries;

		return res;
	}

	lex() {
		let tokens = [];
		const res = new CompileResult();

		res.register(this.processIncludes());
		if (res.error) return res;

		this.pos = new Position(-1, 0, -1, this.fn, this.ftxt);
		this.advance();

		while (this.currentChar != null) {
			let startPos = this.pos.copy();
			if (
				this.ftxt
					.split("\n")
					[this.pos.line].trim()
					.startsWith("include ")
			) {
				this.advance();
				continue;
			}

			if (this.currentChar.match(/[ \t]/)) {
				this.advance();
			} else if (this.currentChar.match(/[\;\n]/)) {
				let char = this.currentChar;
				this.advance();
				tokens.push(
					new Token(startPos, this.pos.copy(), TT.NEWLINE, char)
				);
			} else if (this.currentChar == "&") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.AND));
			} else if (this.currentChar == "|") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.OR));
			} else if (this.currentChar == "~") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.NOT));
			} else if (this.currentChar == "^") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.XOR));
			} else if (this.currentChar == "#") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.ABS));
			} else if (this.currentChar == "$") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.SIGN));
			} else if (this.currentChar == "@") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.AT));
			} else if (this.currentChar == "(") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.LPR));
			} else if (this.currentChar == ")") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.RPR));
			} else if (this.currentChar == "[") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.LSQ));
			} else if (this.currentChar == "]") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.RSQ));
			} else if (this.currentChar == "{") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.LBR));
			} else if (this.currentChar == "}") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.RBR));
			} else if (this.currentChar == ":") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.COL));
			} else if (this.currentChar == ",") {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.COMMA));
			} else if (this.currentChar == "+") {
				this.advance();
				if (this.currentChar == "+") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.INC));
				} else if (this.currentChar == "=") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.ADDTO));
				} else {
					tokens.push(new Token(startPos, this.pos.copy(), TT.ADD));
				}
			} else if (this.currentChar == "-") {
				this.advance();
				if (this.currentChar == "-") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.DEC));
				} else if (this.currentChar == "=") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.SUBBY));
				} else {
					tokens.push(new Token(startPos, this.pos.copy(), TT.SUB));
				}
			} else if (this.currentChar == "=") {
				this.advance();
				if (this.currentChar == "=") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.EQ));
				} else {
					tokens.push(
						new Token(startPos, this.pos.copy(), TT.ASSIGN)
					);
				}
			} else if (this.currentChar == "<") {
				this.advance();
				if ("<=".indexOf(this.currentChar) != -1) {
					let char = this.currentChar;
					this.advance();
					tokens.push(
						new Token(
							startPos,
							this.pos.copy(),
							char == "<" ? TT.LSHIFT : TT.LE
						)
					);
				} else {
					tokens.push(new Token(startPos, this.pos.copy(), TT.LT));
				}
			} else if (this.currentChar == ">") {
				this.advance();
				if (">=".indexOf(this.currentChar) != -1) {
					let char = this.currentChar;
					this.advance();
					tokens.push(
						new Token(
							startPos,
							this.pos.copy(),
							char == ">" ? TT.RSHIFT : TT.GE
						)
					);
				} else {
					tokens.push(new Token(startPos, this.pos.copy(), TT.GT));
				}
			} else if (this.currentChar == "!") {
				this.advance();
				if (this.currentChar != "=") {
					return res.fail(
						new ExpectedCharacter(
							startPos,
							this.pos.copy(),
							"'=' (after '!')"
						)
					);
				}
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.NE));
			} else if (isDigit(this.currentChar)) {
				let numStr = "";
				while (this.currentChar != null && isDigit(this.currentChar)) {
					numStr += this.currentChar;
					this.advance();
				}
				tokens.push(
					new Token(
						startPos,
						this.pos.copy(),
						TT.NUM,
						parseInt(numStr, 10)
					)
				);
			} else if (isLetter(this.currentChar)) {
				let idStr = "";
				while (
					this.currentChar != null &&
					(isAlphaNumeric(this.currentChar) ||
						this.currentChar == "_")
				) {
					idStr += this.currentChar;
					this.advance();
				}
				tokens.push(
					new Token(
						startPos,
						this.pos.copy(),
						KEYWORDS.includes(idStr) || DATA_TYPES.includes(idStr)
							? TT.KEYWORD
							: TT.IDENTIFIER,
						idStr
					)
				);
			} else if (
				this.currentChar == "*" &&
				this.libraries.includes("operations")
			) {
				this.advance();
				tokens.push(new Token(startPos, this.pos.copy(), TT.MUL));
			} else {
				let char = this.currentChar;
				this.advance();
				return res.fail(
					new UnknownCharacter(startPos, this.pos.copy(), `'${char}'`)
				);
			}
		}

		tokens.push(new Token(this.pos.copy(), this.pos.copy(), TT.EOF));
		return res.success(tokens);
	}
}

class Statements {
	constructor(startPos, endPos, body) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.body = body;
	}

	toString() {
		return this.body.map((stmt) => stmt.toString()).join("\n");
	}
}

class NumericLiteral {
	constructor(startPos, endPos, value) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.value = value.value;
	}

	toString() {
		return `NUM[${this.value}]`;
	}
}

class ArrayLiteral {
	constructor(startPos, endPos, elements) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.elements = elements;
		this.length = elements.length;
	}

	validateArray() {
		const res = new CompileResult();
		if (!this.length) return res.success(null);
		console.log(this.elements[0]);

		const firstElementType =
			this.elements[0] instanceof ArrayLiteral ? "array" : "expression";
		for (let i = 0; i < this.length; i++) {
			const element = this.elements[i];
			const elementType =
				element instanceof ArrayLiteral ? "array" : "expression";
			if (elementType !== firstElementType) {
				return res.fail(
					new CustomError(
						element.startPos,
						element.endPos,
						"ArrayError",
						`Element [${i}] is not an ${firstElementType}.`
					)
				);
			}
		}

		return res.success(null);
	}
}

class Identifier {
	constructor(startPos, endPos, symbol) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.symbol = symbol.value;
	}

	toString() {
		return `IDEN[${this.symbol}]`;
	}
}

class BinaryOperation {
	constructor(startPos, endPos, left, opToken, right) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.left = left;
		this.opToken = opToken;
		this.right = right;
	}

	toString() {
		return `(${this.left}, ${this.opToken}, ${this.right})`;
	}
}

class UnaryOperation {
	constructor(startPos, endPos, opToken, value) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.opToken = opToken;
		this.value = value;
	}

	toString() {
		return `(${this.opToken}, ${this.value})`;
	}
}

class VarDeclaration {
	constructor(startPos, endPos, symbol, dataType, value) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.symbol = symbol;
		this.dataType = dataType;
		this.value = value;
	}

	toString() {
		let result = `VAR ${this.symbol}: ${this.dataType.value}`;
		if (this.value) {
			result += ` = ${this.value}`;
		}
		return result;
	}
}

class ConstDefinition {
	constructor(startPos, endPos, symbol, value) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.symbol = symbol;
		this.value = value;
	}

	toString() {
		return `CONST ${this.symbol} = ${this.value}`;
	}
}

class CastExpression {
	constructor(startPos, endPos, dataType, value) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.dataType = dataType;
		this.value = value;
	}

	toString() {
		return `CAST ${this.value} to ${this.dataType}`;
	}
}

class ForLoop {
	constructor(
		startPos,
		endPos,
		identifier,
		startValue,
		endValue,
		stepValue,
		body,
		endToken,
		stepToken
	) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.identifier = identifier;
		this.startValue = startValue;
		this.endValue = endValue;
		this.stepValue = stepValue;
		this.body = body;
		this.endToken = endToken;
		this.stepToken = stepToken;
	}
}

class WhileLoop {
	constructor(startPos, endPos, condition, body) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.condition = condition;
		this.body = body;
	}
}

class IfStatement {
	constructor(startPos, endPos, cases, elseCase) {
		this.startPos = startPos;
		this.endPos = endPos;
		this.cases = cases;
		this.elseCase = elseCase;
	}
}

class Parser {
	constructor(tokens) {
		this.tokens = tokens;
		this.idx = -1;
		this.currentTok = null;
		this.advance();
	}

	advance() {
		this.idx++;
		if (this.idx < this.tokens.length) {
			this.currentTok = this.tokens.at(this.idx);
		}
	}

	binaryOp(funcLeft, funcRight, types) {
		const res = new CompileResult();
		let opToken;
		let right;

		let left = res.register(funcLeft.call(this));
		if (res.error) return res;

		while (types.includes(this.currentTok.tokenType)) {
			opToken = this.currentTok;
			this.advance();

			right = res.register(funcRight.call(this));
			if (res.error) return res;

			left = new BinaryOperation(
				left.startPos,
				right.endPos,
				left,
				opToken,
				right
			);
		}

		return res.success(left);
	}

	failCurrentTok(details) {
		return new InvalidSyntax(
			this.currentTok.startPos,
			this.currentTok.endPos,
			details
		);
	}

	endOfLine() {
		return [TT.NEWLINE, TT.EOF].includes(this.currentTok.tokenType);
	}

	parse() {
		const res = new CompileResult();

		const ast = res.register(this.statements());
		return res.success(ast);
	}

	statements(end = [TT.EOF]) {
		const res = new CompileResult();
		const body = [];
		const endTokenTypes = end;
		endTokenTypes.push(TT.NEWLINE);
		let moreStatements = true;

		while (this.currentTok.tokenType == TT.NEWLINE) {
			this.advance();
		}

		let startPos, endPos, stmt;

		startPos = this.currentTok.startPos;
		while (moreStatements) {
			if (end.includes(this.currentTok.tokenType)) {
				endPos = this.currentTok.endPos;
				moreStatements = false;
				break;
			}

			stmt = res.register(this.statement());
			if (res.error) return res;

			if (!endTokenTypes.includes(this.currentTok.tokenType)) {
				return res.fail(this.failCurrentTok("Expected an operation."));
			}

			body.push(stmt);

			while (this.currentTok.tokenType == TT.NEWLINE) {
				this.advance();
			}
		}

		return res.success(new Statements(startPos, endPos, body));
	}

	statement() {
		if (this.currentTok.tokenType == TT.KEYWORD) {
			switch (this.currentTok.value) {
				case "var":
					return this.varDeclaration();
				case "const":
					return this.constDefinition();
				case "for":
					return this.forLoop();
				case "while":
					return this.whileLoop();
				case "if":
					return this.ifStatement();
			}
		}
		return this.expr();
	}

	varDeclaration() {
		const res = new CompileResult();
		const startPos = this.currentTok.startPos;
		this.advance();

		const symbol = res.register(this.literal());
		if (res.error) return res;

		if (!(symbol instanceof Identifier)) {
			return res.fail(
				new InvalidSyntax(
					symbol.startPos,
					symbol.endPos,
					"Expected an identifier after 'var' keyword."
				)
			);
		}

		if (this.currentTok.tokenType != TT.COL) {
			return res.fail(
				this.failCurrentTok("Expected ':' after identifier.")
			);
		}
		this.advance();

		if (
			this.currentTok.tokenType != TT.KEYWORD ||
			!DATA_TYPES.includes(this.currentTok.value)
		) {
			return res.fail(
				this.failCurrentTok(
					`Expected data types (${DATA_TYPES.join(", ")}).`
				)
			);
		}

		const dataType = this.currentTok.value;
		this.advance();

		let value = null;
		if (this.currentTok.tokenType == TT.ASSIGN) {
			this.advance();
			value = res.register(this.expr());
			if (res.error) return res;
		}

		if (!this.endOfLine()) {
			return res.fail(
				this.failCurrentTok(
					"Expected a newline or EOF after declaration."
				)
			);
		}

		return res.success(
			new VarDeclaration(
				startPos,
				value == null ? dataType.endPos : value.endPos,
				symbol,
				dataType,
				value
			)
		);
	}

	constDefinition() {
		const res = new CompileResult();
		const startPos = this.currentTok.startPos;
		this.advance();

		const symbol = res.register(this.literal());
		if (res.error) return res;

		if (!(symbol instanceof Identifier)) {
			return res.fail(
				new InvalidSyntax(
					symbol.startPos,
					symbol.endPos,
					"Expected an identifier after 'const' keyword."
				)
			);
		}

		if (this.currentTok.tokenType != TT.ASSIGN) {
			return res.fail(
				this.failCurrentTok("Expected '=' after identifier.")
			);
		}
		this.advance();

		let value = res.register(this.expr());
		if (res.error) return res;

		return res.success(
			new ConstDefinition(startPos, value.endPos, symbol, value)
		);
	}

	forLoop() {
		const res = new CompileResult();
		const startPos = this.currentTok.startPos;
		this.advance();

		if (this.currentTok.tokenType !== TT.LPR)
			return res.fail(
				this.failCurrentTok("Expected '(' after 'for' keyword.")
			);
		this.advance();

		if (this.currentTok.tokenType !== TT.IDENTIFIER)
			return res.fail(
				this.failCurrentTok("Expected an identifier after '('.")
			);
		const identifier = res.register(this.literal());
		if (res.error) return res;
		if (this.currentTok.tokenType !== TT.ASSIGN)
			return res.fail(
				this.failCurrentTok("Expected '=' after iterator.")
			);
		this.advance();
		const startValue = res.register(this.expr());
		if (res.error) return res;

		if (this.currentTok.notEquals(new Token(null, null, TT.NEWLINE, ";"))) {
			return res.fail(
				this.failCurrentTok("Expected ';' after start value.")
			);
		}
		this.advance();
		if (
			this.currentTok.notEquals(
				new Token(null, null, TT.IDENTIFIER, identifier.symbol)
			)
		) {
			return res.fail(
				this.failCurrentTok(
					`Expected '${identifier.symbol}' after ';'.`
				)
			);
		}
		this.advance();
		if (![TT.LT, TT.LE, TT.GT, TT.GE].includes(this.currentTok.tokenType)) {
			return res.fail(
				this.failCurrentTok("Expected '<', '<=', '>', or '>='.")
			);
		}
		const endToken = this.currentTok;
		this.advance();

		const endValue = res.register(this.expr());
		if (res.error) return res;

		if (this.currentTok.notEquals(new Token(null, null, TT.NEWLINE, ";"))) {
			return res.fail(
				this.failCurrentTok("Expected ';' after start value.")
			);
		}
		this.advance();
		if (
			this.currentTok.notEquals(
				new Token(null, null, TT.IDENTIFIER, identifier.symbol)
			)
		) {
			return res.fail(
				this.failCurrentTok(
					`Expected '${identifier.symbol}' after ';'.`
				)
			);
		}
		this.advance();
		if (![TT.ADDTO, TT.SUBBY].includes(this.currentTok.tokenType)) {
			return res.fail(
				this.failCurrentTok(
					`Expected '+=' or '-=' after '${identifier.symbol}'.`
				)
			);
		}
		const stepToken = this.currentTok;
		this.advance();
		const stepValue = res.register(this.expr());
		if (res.error) return res;

		if (this.currentTok.tokenType !== TT.RPR) {
			console.log(this.currentTok);
			return res.fail(
				this.failCurrentTok("Expected ')' after step value.")
			);
		}
		this.advance();
		if (this.currentTok.tokenType !== TT.LBR) {
			return res.fail(this.failCurrentTok("Expected '{' after ')'."));
		}
		this.advance();

		const body = res.register(this.statements([TT.EOF, TT.RBR]));
		if (res.error) return res;

		if (this.currentTok.tokenType !== TT.RBR) {
			return res.fail(
				this.failCurrentTok("Expected '}' after code block.")
			);
		}
		const endPos = this.currentTok.endPos;
		this.advance();
		if (!this.endOfLine()) {
			return res.fail(
				this.failCurrentTok("Expected a newline or EOF after '}'.")
			);
		}

		return res.success(
			new ForLoop(
				startPos,
				endPos,
				identifier,
				startValue,
				endValue,
				stepValue,
				body,
				endToken,
				stepToken
			)
		);
	}

	whileLoop() {
		const startPos = this.currentTok.startPos;
		const res = new CompileResult();
		this.advance();

		if (this.currentTok.tokenType !== TT.LPR) {
			return res.fail(
				this.failCurrentTok("Expected '(' after 'while' keyword.")
			);
		}
		this.advance();

		const condition = res.register(this.expr());
		if (res.error) return res;

		if (this.currentTok.tokenType !== TT.RPR) {
			return res.fail(
				this.failCurrentTok("Expected ')' after 'while' keyword.")
			);
		}
		this.advance();

		if (this.currentTok.tokenType !== TT.LBR) {
			return res.fail(this.failCurrentTok("Expected '{' after ')'."));
		}
		this.advance();

		const body = res.register(this.statements([TT.EOF, TT.RBR]));
		if (res.error) return res;

		if (this.currentTok.tokenType !== TT.RBR) {
			return res.fail(
				this.failCurrentTok("Expected '}' after code block.")
			);
		}
		const endPos = this.currentTok.endPos;
		this.advance();
		if (!this.endOfLine()) {
			return res.fail(
				this.failCurrentTok("Expected a newline or EOF after '}'.")
			);
		}

		return res.success(new WhileLoop(startPos, endPos, condition, body));
	}

	ifStatement() {
		const startPos = this.currentTok.startPos;
		const res = new CompileResult();
		this.advance();

		const cases = [];
		let elseCase = null;

		if (this.currentTok.tokenType != TT.LPR) {
			return res.fail(
				this.failCurrentTok("Expected '(' after 'if' keyword.")
			);
		}
		this.advance();

		let condition = res.register(this.expr());
		if (res.error) return res;

		if (this.currentTok.tokenType != TT.RPR) {
			return res.fail(
				this.failCurrentTok("Expected ')' after condition.")
			);
		}
		this.advance();

		if (this.currentTok.tokenType != TT.LBR) {
			return res.fail(this.failCurrentTok("Expected '{' after ')'."));
		}
		this.advance();

		let body = res.register(this.statements([TT.EOF, TT.RBR]));
		if (res.error) return res;

		if (this.currentTok.tokenType != TT.RBR) {
			return res.fail(
				this.failCurrentTok("Expected '}' after code block.")
			);
		}
		this.advance();
		cases.push([condition, body]);

		while (this.currentTok.tokenType == TT.NEWLINE) this.advance();

		const elseIfToken = new Token(null, null, TT.KEYWORD, "elseif");
		const elseToken = new Token(null, null, TT.KEYWORD, "else");
		let endPos;

		while (this.currentTok.equals(elseIfToken)) {
			this.advance();
			if (this.currentTok.tokenType != TT.LPR) {
				return res.fail(
					this.failCurrentTok("Expected '(' after 'elseif' keyword.")
				);
			}
			this.advance();

			let condition = res.register(this.expr());
			if (res.error) return res;

			if (this.currentTok.tokenType != TT.RPR) {
				return res.fail(
					this.failCurrentTok("Expected ')' after condition.")
				);
			}
			this.advance();

			if (this.currentTok.tokenType != TT.LBR) {
				return res.fail(this.failCurrentTok("Expected '{' after ')'."));
			}
			this.advance();

			let body = res.register(this.statements([TT.EOF, TT.RBR]));
			if (res.error) return res;

			if (this.currentTok.tokenType != TT.RBR) {
				return res.fail(
					this.failCurrentTok("Expected '}' after code block.")
				);
			}
			endPos = this.currentTok.endPos;
			this.advance();
			cases.push([condition, body]);

			while (this.currentTok.tokenType == TT.NEWLINE) this.advance();
		}

		if (this.currentTok.equals(elseToken)) {
			this.advance();
			if (this.currentTok.tokenType != TT.LBR) {
				return res.fail(
					this.failCurrentTok("Expected '{' after else keyword.")
				);
			}
			this.advance();
			elseCase = res.register(this.statements([TT.EOF, TT.RBR]));
			if (res.error) return res;

			if (this.currentTok.tokenType != TT.RBR) {
				return res.fail(
					this.failCurrentTok("Expected '}' after code block.")
				);
			}
			endPos = this.currentTok.endPos;
			this.advance();
		}

		if (!this.endOfLine()) {
			return res.fail(
				this.failCurrentTok(
					"Expected a newline or EOF after code block."
				)
			);
		}

		return res.success(new IfStatement(startPos, endPos, cases, elseCase));
	}

	expr() {
		return this.assignment();
	}

	assignment() {
		const res = new CompileResult();
		const left = res.register(this.comparison());
		if (res.error) return res;

		if (
			[TT.ASSIGN, TT.ADDTO, TT.SUBBY].includes(this.currentTok.tokenType)
		) {
			if (!(left instanceof Identifier)) {
				return res.fail(
					new InvalidSyntax(
						left.startPos,
						left.endPos,
						"A value can only be assigned to an identifier."
					)
				);
			}

			const opToken = this.currentTok;
			this.advance();
			const right = res.register(this.assignment());
			if (res.error) return res;

			return res.success(
				new BinaryOperation(
					left.startPos,
					right.endPos,
					left,
					opToken,
					right
				)
			);
		}

		return res.success(left);
	}

	comparison() {
		return this.binaryOp(this.bitwise, this.bitwise, [
			TT.LT,
			TT.LE,
			TT.GT,
			TT.GE,
			TT.EQ,
			TT.NE,
		]);
	}

	bitwise() {
		return this.binaryOp(this.additive, this.additive, [
			TT.AND,
			TT.OR,
			TT.XOR,
			TT.RSHIFT,
			TT.LSHIFT,
		]);
	}

	additive() {
		return this.binaryOp(this.multiplicative, this.multiplicative, [
			TT.ADD,
			TT.SUB,
		]);
	}

	multiplicative() {
		return this.binaryOp(this.unary, this.unary, [TT.MUL]);
	}

	unary() {
		const res = new CompileResult();
		let opToken;

		if (
			[TT.ADD, TT.SUB, TT.NOT, TT.AT, TT.ABS, TT.SIGN].includes(
				this.currentTok.tokenType
			)
		) {
			opToken = this.currentTok;
			this.advance();

			let value = res.register(this.unary());
			if (res.error) return res;

			if (opToken.tokenType == TT.AT && !(value instanceof Identifier)) {
				return res.fail(
					new InvalidSyntax(
						value.startPos,
						value.endPos,
						"Expected an identifier after '@' operator."
					)
				);
			}

			return res.success(
				new UnaryOperation(
					opToken.startPos,
					value.endPos,
					opToken,
					value
				)
			);
		}

		let value = res.register(this.access());
		if (res.error) return res;

		if ([TT.INC, TT.DEC].includes(this.currentTok.tokenType)) {
			opToken = this.currentTok;
			this.advance();

			return res.success(
				new UnaryOperation(
					value.startPos,
					opToken.endPos,
					opToken,
					value
				)
			);
		}

		return res.success(value);
	}

	access() {
		return this.call();
	}

	call() {
		return this.literal();
	}

	literal() {
		const tok = this.currentTok;
		const res = new CompileResult();
		this.advance();

		switch (tok.tokenType) {
			case TT.NUM:
				return res.success(
					new NumericLiteral(tok.startPos, tok.endPos, tok)
				);
			case TT.IDENTIFIER:
				return res.success(
					new Identifier(tok.startPos, tok.endPos, tok)
				);
			case TT.LPR:
				if (
					this.currentTok.tokenType == TT.KEYWORD &&
					DATA_TYPES.includes(this.currentTok.value)
				) {
					return this.castExpression(tok.startPos);
				}

				while (
					this.currentTok.equals(
						new Token(null, null, TT.NEWLINE, "\n")
					)
				)
					this.advance();
				const expression = res.register(this.expr());
				if (res.error) return res;

				while (
					this.currentTok.equals(
						new Token(null, null, TT.NEWLINE, "\n")
					)
				)
					this.advance();

				if (this.currentTok.tokenType != TT.RPR) {
					return res.fail(
						new InvalidSyntax(
							this.currentTok.startPos,
							this.currentTok.endPos,
							"Expected ')'."
						)
					);
				}

				this.advance();
				return res.success(expression);
			case TT.LSQ:
				while (
					this.currentTok.equals(
						new Token(null, null, TT.NEWLINE, "\n")
					)
				)
					this.advance();

				const elements = [];
				if (this.currentTok.tokenType === TT.RSQ) {
					return res.success(
						new ArrayLiteral(
							tok.startPos,
							this.currentTok.endPos,
							elements
						)
					);
				}

				let element = res.register(this.expr());
				if (res.error) return res;
				elements.push(element);

				while (
					this.currentTok.equals(
						new Token(null, null, TT.NEWLINE, "\n")
					)
				)
					this.advance();

				while (this.currentTok.tokenType === TT.COMMA) {
					this.advance();
					while (
						this.currentTok.equals(
							new Token(null, null, TT.NEWLINE, "\n")
						)
					)
						this.advance();

					element = res.register(this.expr());
					if (res.error) return res;
					elements.push(element);

					while (
						this.currentTok.equals(
							new Token(null, null, TT.NEWLINE, "\n")
						)
					)
						this.advance();
				}

				if (this.currentTok.tokenType !== TT.RSQ) {
					return res.fail(
						this.failCurrentTok("Expected ',' or ']'.")
					);
				}
				const endPos = this.currentTok.endPos;
				this.advance();

				const array = new ArrayLiteral(tok.startPos, endPos, elements);
				res.register(array.validateArray());
				if (res.error) return res;
				return res.success(array);
			default:
				return res.fail(
					new InvalidSyntax(
						tok.startPos,
						tok.endPos,
						`Expected a number, an identifier, '(', '[' found token ${tok} instead.`
					)
				);
		}
	}

	castExpression(startPos) {
		const res = new CompileResult();
		const dataType = this.currentTok.value;
		this.advance();

		if (this.currentTok.tokenType != TT.RPR) {
			return res.fail(
				new InvalidSyntax(
					this.currentTok.startPos,
					this.currentTok.endPos,
					`Expected ')' after '${dataType}'.`
				)
			);
		}
		this.advance();

		const value = res.register(this.expr());
		if (res.error) return res;

		return res.success(
			new CastExpression(startPos, value.endPos, dataType, value)
		);
	}
}

class Environment {
	constructor() {
		this.symbols = new Map([
			["true", [-1, "bool"]],
			["false", [0, "bool"]],
			["N_BITS", [16, "int"]],
		]);
		this.definedVars = [];
		this.constants = ["true", "false", "N_BITS"];
		this.assignAddress = 16;
	}

	defineSymbol(symbol, varType, dataType, value = null) {
		const res = new CompileResult();

		if (this.symbols.has(symbol.symbol)) {
			return res.fail(
				new SymbolError(
					symbol.startPos,
					symbol.endPos,
					`Symbol '${symbol.symbol}' is already defined.`
				)
			);
		}

		if (varType == "const") {
			this.constants.push(symbol.symbol);
		}

		this.symbols.set(symbol.symbol, [
			value != null ? value : this.assignAddress,
			dataType,
		]);
		if (varType != "const") {
			this.assignAddress++;
		}
		return res.success([null, dataType]);
	}

	getSymbol(symbol) {
		const res = new CompileResult();
		const symbolStr = symbol.symbol;

		if (!this.symbols.has(symbolStr)) {
			return res.fail(
				new SymbolError(
					symbol.startPos,
					symbol.endPos,
					`Symbol '${symbolStr}' is undefined.`
				)
			);
		}
		return res.success(this.symbols.get(symbolStr));
	}
}

class Compiler {
	constructor() {
		this.MAX_RAM_ADDR = 2047;
		this.X_ADDR = this.MAX_RAM_ADDR + 1;
		this.Y_ADDR = this.MAX_RAM_ADDR + 2;
		this.INPUT_ADDR = this.MAX_RAM_ADDR + 3;
		this.KNOWN_VALUES = [-2, -1, 0, 1];

		this.instructions = [];
		this.allocatedRegs = new Set();
		this.availableRegs = new Set();

		for (let i = 0; i < 16; i++) {
			this.availableRegs.add(i);
		}

		this.tabs = 0;
		this.methodNames = Object.getOwnPropertyNames(
			Object.getPrototypeOf(this)
		).filter(
			(name) => typeof this[name] === "function" && name !== "constructor"
		);
	}

	allocate(node) {
		const res = new CompileResult();
		if (!this.availableRegs) {
			return res.fail(
				new CodingError(
					node.startPos,
					node.endPos,
					"Unable to allocate a register for the result."
				)
			);
		}

		const freeRegister = [...this.availableRegs][0];
		this.allocatedRegs.add(freeRegister);
		this.availableRegs.delete(freeRegister);

		return res.success(`r${freeRegister}`);
	}

	free(register) {
		const registerValue = parseInt(register.slice(1));
		this.allocatedRegs.delete(registerValue);
		this.availableRegs.add(registerValue);
	}

	makeNewJump() {
		this.jumps++;
		return this.jumps - 1;
	}

	write(inst) {
		let toWrite = "";
		for (let i = 0; i < this.tabs; i++) {
			toWrite += "\t";
		}
		this.instructions.push(toWrite + inst);
	}

	loadImmediate(value) {
		let toWrite = "";
		for (let i = 0; i < this.tabs; i++) {
			toWrite += "\t";
		}
		this.instructions.push(toWrite + `LDIA ${value}`);
	}

	comment(comment) {
		this.instructions[this.instructions.length - 1] += ` // ${comment}`;
	}

	compile(ast) {
		const res = new CompileResult();
		const env = new Environment();

		this.jumps = 0;
		this.tabs = 0;

		if (!ast.body) return res.success("HALT");

		res.register(this.visit(ast, env));
		if (res.error) return res;
		this.write("HALT");

		const optimized = this.optimizeInstructions(this.instructions);
		return res.success(optimized.join("\n"));
	}

	optimizeNode(node) {
		const isAddSub = (type) => type === TT.ADD || type === TT.SUB;

		if (node instanceof BinaryOperation) {
			if (
				node.opToken.tokenType === TT.ASSIGN &&
				node.right instanceof BinaryOperation &&
				isAddSub(node.right.opToken.tokenType) &&
				node.left instanceof Identifier &&
				node.right.left instanceof Identifier &&
				node.left.symbol === node.right.left.symbol
			) {
				return new BinaryOperation(
					node.startPos,
					node.endPos,
					node.left,
					new Token(
						node.opToken.startPos,
						node.opToken.endPos,
						node.right.opToken.tokenType === TT.ADD
							? TT.ADDTO
							: TT.SUBBY
					),
					node.right.right
				);
			}
			if (
				isAddSub(node.opToken.tokenType) &&
				node.right instanceof NumericLiteral
			) {
				if (node.right.value == 0) return node.left;
				else if (node.right.value == 1)
					return new UnaryOperation(
						node.left.startPos,
						node.right.endPos,
						new Token(
							node.opToken.startPos,
							node.opToken.endPos,
							node.opToken.tokenType === TT.ADD ? TT.INC : TT.DEC
						),
						node.left
					);
			}
			if (
				node.opToken.tokenType === TT.ADD &&
				node.left instanceof NumericLiteral
			) {
				if (node.left.value == 0) return node.left;
				else if (node.left.value == 1)
					return new UnaryOperation(
						node.left.startPos,
						node.right.endPos,
						new Token(
							node.opToken.startPos,
							node.opToken.endPos,
							TT.INC
						),
						node.right
					);
			}
			if (
				node.opToken.tokenType === TT.SUB &&
				node.left instanceof NumericLiteral &&
				node.left.value == 0
			) {
				return new UnaryOperation(
					node.left.startPos,
					node.right.endPos,
					new Token(
						node.opToken.startPos,
						node.opToken.endPos,
						TT.SUB
					),
					node.right
				);
			}
		}
		return node;
	}

	optimizeInstructions(instructions) {
		const optimized = [];
		let prev = null;
		let lastLoaded = null;
		let afterJump = false;
		const removedLabels = new Map();

		for (let i = 0; i < instructions.length; i++) {
			let line = instructions[i];
			let trimmed = line.trim();

			if (!trimmed || trimmed.startsWith("//")) continue;

			if (trimmed === prev) continue;

			if (trimmed.startsWith("LDIA")) {
				let value = trimmed.split(" ")[1];

				if (removedLabels.has(value)) {
					value = removedLabels.get(value);
				}

				if (value === lastLoaded) {
					if (prev && !prev.startsWith(".")) continue;
				}
				lastLoaded = value;
			} else if (trimmed.startsWith("COMP")) {
				if (trimmed === "COMP M D" && prev && prev === "COMP D M") {
					continue;
				}
			} else lastLoaded = null;

			if (prev && prev.startsWith(".")) afterJump = false;
			if (afterJump && !trimmed.startsWith(".")) continue;
			afterJump = prev && prev.endsWith("JMP");

			if (trimmed.startsWith(".") && prev && prev.startsWith(".")) {
				removedLabels.set(prev, trimmed);
				console.log(removedLabels);
				continue;
			}

			optimized.push(line);
			prev = trimmed;
		}

		return optimized;
	}

	handleComparison(operation, leftRegister) {
		const res = new CompileResult();
		this.loadImmediate(leftRegister);
		this.write("COMP M-D D");

		const boolJmp = this.makeNewJump();
		this.loadImmediate(`.true${boolJmp}`);
		this.write(`COMP D J${operation}`);
		this.loadImmediate(`.end${boolJmp}`);
		this.write("COMP 0 D JMP");
		this.write(`.true${boolJmp}`);
		this.write("COMP -1 D");
		this.write(`.end${boolJmp}`);

		return res.success([null, "bool"]);
	}

	validateVariable(identifier, env) {
		const res = new CompileResult();
		if (
			identifier instanceof Identifier &&
			!env.definedVars.includes(identifier.symbol) &&
			!env.constants.includes(identifier.symbol)
		) {
			return res.fail(
				new SymbolError(
					identifier.startPos,
					identifier.endPos,
					`Symbol '${identifier.symbol}' isn't assigned a value yet.`
				)
			);
		}
		return res.success(null);
	}

	noVisitMethod(node, env) {
		return new CompileResult().fail(
			new CodingError(
				node.startPos,
				node.endPos,
				`No visit${node.constructor.name} method defined.`
			)
		);
	}

	visit(node, env) {
		const optimizedNode = this.optimizeNode(node);
		const visitMethodName = `visit${optimizedNode.constructor.name}`;
		const visitMethod = this.methodNames.includes(visitMethodName)
			? this[visitMethodName]
			: this.noVisitMethod;

		return visitMethod.call(this, optimizedNode, env);
	}

	visitStatements(node, env) {
		const res = new CompileResult();
		for (const stmt of node.body) {
			res.register(this.visit(stmt, env));
			if (res.error) return res;
		}

		return res.success(null);
	}

	visitNumericLiteral(node, env) {
		const res = new CompileResult();
		if (this.KNOWN_VALUES.includes(node.value))
			this.write(`COMP ${node.value} D`);
		else {
			this.loadImmediate(node.value);
			this.write("COMP A D");
		}

		return res.success([node.value, "int"]);
	}

	visitIdentifier(node, env) {
		const res = new CompileResult();
		const valueAndType = res.register(env.getSymbol(node));
		if (res.error) return res;

		const value = valueAndType[0];
		const type = valueAndType[1];

		if (env.constants.includes(node.symbol)) {
			return res.success([
				this.visitNumericLiteral(
					new NumericLiteral(
						null,
						null,
						new Token(null, null, TT.NUM, value)
					),
					env
				).value[0],
				type,
			]);
		}

		// 'value' is now an address.
		this.loadImmediate(value);
		this.comment(node.symbol);
		this.write("COMP M D");
		return res.success([
			env.constants.includes(node.symbol) ? value : null,
			type,
		]);
	}

	visitCastExpression(node, env) {
		const res = new CompileResult();
		const value = res.register(this.visit(node.value, env));
		if (res.error) return res;

		if (value[1] === node.dataType) return res.success(value);

		if (value[1] === "bool") {
			if (node.dataType === "int")
				return res.success([value[0], node.dataType]);
		} else if (value[1] === "int") {
			if (Number.isInteger(value[0])) {
				return res.success([
					this.visitNumericLiteral(
						new NumericLiteral(
							null,
							null,
							new Token(null, null, TT.NUM, value[0] > 0 ? -1 : 0)
						),
						env
					).value[0],
					"bool",
				]);
			} else {
				const boolJmp = this.makeNewJump();
				this.loadImmediate(`.true${boolJmp}`);
				this.write(`COMP D JGT`);
				this.loadImmediate(`.end${boolJmp}`);
				this.write("COMP 0 D JMP");
				this.write(`.true${boolJmp}`);
				this.write("COMP -1 D");
				this.write(`.end${boolJmp}`);
				return res.success([null, "bool"]);
			}
		}

		return res.fail(
			new CodingError(
				node.value.startPos,
				node.value.endPos,
				`Cannot cast [${value[1]}] to [${node.dataType}].`
			)
		);
	}

	visitBinaryOperation(node, env) {
		const res = new CompileResult();

		const operationMap = new Map([
			[TT.ADD, "+"],
			[TT.SUB, "-"],
			[TT.AND, "&"],
			[TT.OR, "|"],
			[TT.XOR, "^"],
			[TT.MUL, "*"],
			[TT.RSHIFT, ">>"],
			[TT.LSHIFT, "<<"],
			[TT.ASSIGN, "="],
			[TT.ADDTO, "+="],
			[TT.SUBBY, "-="],
			[TT.LT, "<"],
			[TT.LE, "<="],
			[TT.GT, ">"],
			[TT.GE, ">="],
			[TT.EQ, "=="],
			[TT.NE, "!="],
		]);
		const operation = operationMap.get(node.opToken.tokenType);

		if (operation === undefined) {
			return res.fail(
				new CodingError(
					node.opToken.startPos,
					node.opToken.endPos,
					`Undefined operation: ${node.opToken}`
				)
			);
		}

		res.register(this.validateVariable(node.right, env));
		if (res.error) return res;

		if (["=", "+=", "-="].includes(operation)) {
			const varData = res.register(env.getSymbol(node.left));
			if (res.error) return res;
			if (env.constants.includes(node.left.symbol)) {
				return res.fail(
					new SymbolError(
						node.left.startPos,
						node.left.endPos,
						`Cannot modify the value of constant '${node.left.symbol}'.`
					)
				);
			}

			const valuePos = this.instructions.length;
			const value = res.register(this.visit(node.right, env));
			if (res.error) return res;

			if (varData[1] != value[1]) {
				return res.fail(
					new CustomTypeError(
						node.opToken.startPos,
						node.opToken.endPos,
						`Incompatible operation ('${operation}') between [${varData[1]}] and [${value[1]}].`
					)
				);
			}

			switch (operation) {
				case "=":
					if (this.KNOWN_VALUES.includes(value[0]))
						this.instructions = this.instructions.slice(
							0,
							valuePos
						);
					this.loadImmediate(varData[0]);
					this.comment(node.left.symbol);
					if (!env.definedVars.includes(node.left.symbol))
						env.definedVars.push(node.left.symbol);
					this.write(
						this.KNOWN_VALUES.includes(value[0])
							? `COMP ${value[0]} M`
							: "COMP D M"
					);
					break;
				case "+=":
					if ([0, 1, -1].includes(value[0]))
						this.instructions = this.instructions.slice(
							0,
							valuePos
						);
					this.loadImmediate(varData[0]);
					this.comment(node.left.symbol);
					res.register(this.validateVariable(node.left, env));
					if (res.error) return res;
					this.write(
						value[0] == 0
							? ""
							: value[0] == 1
							? "COMP M++ M"
							: value[0] == -1
							? "COMP M-- M"
							: "COMP D+M DM"
					);
					break;
				case "-=":
					if ([0, 1, -1].includes(value[0]))
						this.instructions = this.instructions.slice(
							0,
							valuePos
						);
					this.loadImmediate(varData[0]);
					this.comment(node.left.symbol);
					res.register(this.validateVariable(node.left, env));
					if (res.error) return res;
					this.write(
						value[0] == 0
							? "COMP M M"
							: value[0] == 1
							? "COMP M-- M"
							: value[0] == -1
							? "COMP M++ M"
							: "COMP M-D DM"
					);
			}

			return res.success([value[0], varData[1]]);
		}

		res.register(this.validateVariable(node.left, env));
		if (res.error) return res;

		const leftPos = this.instructions.length;
		const left = res.register(this.visit(node.left, env));
		if (res.error) return res;
		const leftRegister = res.register(this.allocate(node.left));
		if (res.error) return res;

		this.loadImmediate(leftRegister);
		this.write("COMP D M");

		const rightPos = this.instructions.length;
		const right = res.register(this.visit(node.right, env));
		if (res.error) return res;
		const rightRegister = res.register(this.allocate(node.right));
		if (res.error) return res;

		// Type safety
		if (left[1] !== right[1]) {
			return res.fail(
				new CustomTypeError(
					node.opToken.startPos,
					node.opToken.endPos,
					`Incompatible operation ('${operation}') between [${left[1]}] and [${right[1]}].`
				)
			);
		}

		if (left[1] === "int") {
			if (Number.isInteger(left[0]) && Number.isInteger(right[0])) {
				this.instructions = this.instructions.slice(0, leftPos);
				let value = eval(`${left[0]} ${operation} ${right[0]}`);
				let isBoolean = false;

				if (typeof value === "boolean") {
					value = value ? -1 : 0;
					isBoolean = true;
				}

				const valueLiteral = this.visitNumericLiteral(
					new NumericLiteral(
						null,
						null,
						new Token(null, null, TT.NUM, value)
					),
					env
				).value;

				return res.success([
					valueLiteral[0],
					isBoolean ? "bool" : "int",
				]);
			}
		} else if (left[1] === "bool") {
			if ("&|^".indexOf(operation) == -1) {
				return res.fail(
					new CustomTypeError(
						node.opToken.startPos,
						node.opToken.endPos,
						`Incompatible operation ('${operation}') between [${left[1]}] and [${right[1]}].`
					)
				);
			}

			if (Number.isInteger(left[0]) && Number.isInteger(right[0])) {
				this.instructions = this.instructions.slice(0, leftPos);
				let value = eval(`${left[0]} ${operation} ${right[0]}`);

				if (typeof value === "boolean") {
					value = value ? -1 : 0;
				}

				return this.visitNumericLiteral(
					new NumericLiteral(
						null,
						null,
						new Token(null, null, TT.NUM, value)
					),
					env
				);
			}
		}

		if (operation === "*") {
			this.loadImmediate(rightRegister);
			this.write("COMP D M");
			const productRegister = res.register(this.allocate(node));
			if (res.error) return res;

			this.loadImmediate(productRegister);
			this.write("COMP 0 M");

			this.visitIdentifier(
				new Identifier(
					null,
					null,
					new Token(null, null, TT.IDENTIFIER, "N_BITS")
				),
				env
			);
			const bits = res.register(this.allocate(node));
			if (res.error) return res;
			this.loadImmediate(bits);
			this.write("COMP D M");

			const mulJmp = this.makeNewJump();
			this.write(`.mulLoop${mulJmp}`);
			this.tabs++;
			this.write("COMP 1 D");
			this.loadImmediate(rightRegister);
			this.comment("Get LSB");
			this.write("COMP D&M D");
			this.loadImmediate(`.mulShift${mulJmp}`);
			this.write("COMP D JEQ");

			// Add multiplier to product
			this.loadImmediate(leftRegister);
			this.write("COMP M D");
			this.loadImmediate(productRegister);
			this.write("COMP D+M M");

			this.write(`.mulShift${mulJmp}`);
			this.loadImmediate(rightRegister);
			this.write("COMP >>M M");
			this.loadImmediate(leftRegister);
			this.write("COMP M D");
			this.write("COMP D+M M");

			this.loadImmediate(bits);
			this.write("COMP M-- DM");
			this.loadImmediate(`.mulLoop${mulJmp}`);
			this.write("COMP D JGE");

			this.tabs--;
			this.loadImmediate(productRegister);
			this.write("COMP M D");
			this.free(productRegister);
			this.free(bits);
		} else if (operation === ">>") {
			if ([0, 1].includes(right[0])) {
				this.instructions = this.instructions.slice(0, rightPos - 2);
				if (right[0] == 1) this.write("COMP >>D D");
			} else {
				this.loadImmediate(rightRegister);
				this.write("COMP D M");

				const rshiftJmp = this.makeNewJump();
				this.write(`.rshiftLoop${rshiftJmp}`);
				this.tabs++;
				this.loadImmediate(rightRegister);
				this.write("COMP M-- DM");
				this.loadImmediate(`.rshiftEnd${rshiftJmp}`);
				this.write("COMP D JLT");

				this.loadImmediate(leftRegister);
				this.write("COMP >>M M");
				this.loadImmediate(`.rshiftLoop${rshiftJmp}`);
				this.write("COMP 0 JMP");

				this.tabs--;
				this.write(`.rshiftEnd${rshiftJmp}`);
				this.loadImmediate(leftRegister);
				this.write("COMP M D");
			}
		} else if (operation === "<<") {
			if ([0, 1].includes(right[0])) {
				this.instructions = this.instructions.slice(0, rightPos - 2);
				if (right[0] == 1) this.write("COMP D+M D");
			} else {
				this.loadImmediate(rightRegister);
				this.write("COMP D M");

				const lshiftJmp = this.makeNewJump();
				this.write(`.lshiftLoop${lshiftJmp}`);
				this.tabs++;
				this.loadImmediate(rightRegister);
				this.write("COMP M-- DM");
				this.loadImmediate(`.lshiftEnd${lshiftJmp}`);
				this.write("COMP D JLT");

				this.loadImmediate(leftRegister);
				this.write("COMP M D");
				this.write("COMP D+M M");
				this.loadImmediate(`.lshiftLoop${lshiftJmp}`);
				this.write("COMP 0 JMP");

				this.tabs--;
				this.write(`.lshiftEnd${lshiftJmp}`);
				this.loadImmediate(leftRegister);
				this.write("COMP M D");
			}
		} else if (["<", "<=", ">", ">=", "==", "!="].includes(operation)) {
			const compResult = this.handleComparison(
				`${node.opToken}`,
				leftRegister
			);
			this.free(leftRegister);
			this.free(rightRegister);
			return compResult;
		} else {
			this.loadImmediate(leftRegister);
			this.write(
				`COMP ${operation == "-" ? "M-D" : "D" + operation + "M"} D`
			);
		}

		this.free(leftRegister);
		this.free(rightRegister);
		return res.success([null, left[1]]);
	}

	visitUnaryOperation(node, env) {
		const res = new CompileResult();

		const operationMap = new Map([
			[TT.ADD, "+"],
			[TT.SUB, "-"],
			[TT.NOT, "~"],
			[TT.INC, "++"],
			[TT.DEC, "--"],
			[TT.ABS, "#"],
			[TT.SIGN, "$"],
		]);
		const operation = operationMap.get(node.opToken.tokenType);

		if (node.opToken.tokenType === TT.AT) {
			const address = res.register(env.getSymbol(node.value))[0];
			if (res.error) return res;

			if (env.constants.includes(node.value.symbol)) {
				return res.fail(
					new CustomTypeError(
						node.value.startPos,
						node.value.endPos,
						"Cannot retrieve the address of a constant."
					)
				);
			}
			return this.visitNumericLiteral(
				new NumericLiteral(
					null,
					null,
					new Token(null, null, TT.NUM, address)
				),
				env
			);
		}

		res.register(this.validateVariable(node.value, env));
		if (res.error) return res;

		const valuePos = this.instructions.length;
		const value = res.register(this.visit(node.value, env));
		if (res.error) return res;

		if (value[1] !== "int" && operation != "~") {
			return res.fail(
				new CustomTypeError(
					node.value.startPos,
					node.value.endPos,
					`Incompatible operation ('${operation}') for [${value[1]}].`
				)
			);
		}

		if (Number.isInteger(value[0])) {
			this.instructions = this.instructions.slice(0, valuePos);
			let foldedValue;

			switch (node.opToken.tokenType) {
				case TT.ADD:
					foldedValue = value[0];
					break;
				case TT.SUB:
					foldedValue = -value[0];
					break;
				case TT.NOT:
					foldedValue = ~value[0];
					break;
				case TT.INC:
					foldedValue = value[0] + 1;
					break;
				case TT.DEC:
					foldedValue = value[0] - 1;
					break;
				case TT.ABS:
					foldedValue = Math.abs(value[0]);
					break;
				case TT.SIGN:
					foldedValue = value[0] < 0 ? -1 : value[0] == 0 ? 0 : 1;
					break;
			}

			const foldedNum = res.register(
				this.visitNumericLiteral(
					new NumericLiteral(
						null,
						null,
						new Token(null, null, TT.NUM, foldedValue)
					),
					env
				)
			)[0];
			return res.success([foldedNum, value[1]]);
		} else {
			switch (node.opToken.tokenType) {
				case TT.SUB:
					this.write("COMP -D D");
					break;
				case TT.NOT:
					this.write("COMP !D D");
					break;
				case TT.INC:
					this.write("COMP D++ D");
					break;
				case TT.DEC:
					this.write("COMP D-- D");
					break;
				case TT.ABS:
					const absJmp = this.makeNewJump();
					this.loadImmediate(`.abs${absJmp}`);
					this.write("COMP D JGE");
					this.write("COMP -D D");
					self.write(`.abs${absJmp}`);
					break;
				case TT.SIGN:
					const signJmp = this.makeNewJump();
					this.loadImmediate(`.neg${signJmp}`);
					this.write("COMP D JLT");
					this.loadImmediate(`.pos${signJmp}`);
					this.write("COMP D JGT");
					this.loadImmediate(`.end${signJmp}`);
					this.write("COMP D JMP");

					this.write(`.neg${signJmp}`);
					this.write("COMP -1 D");
					this.loadImmediate(`.end${signJmp}`);
					this.write("COMP D JMP");
					this.write(`.pos${signJmp}`);
					this.write("COMP 1 D");
					this.write(`.end${signJmp}`);
					break;
			}
			return res.success([null, value[1]]);
		}
	}

	visitConstDefinition(node, env) {
		const res = new CompileResult();
		const startPos = this.instructions.length;

		const value = res.register(this.visit(node.value, env));
		if (res.error) return res;

		if (value[0] === null) {
			return res.fail(
				new CodingError(
					node.value.startPos,
					node.value.endPos,
					"Cannot use a variable in const definition."
				)
			);
		}

		this.instructions = this.instructions.slice(0, startPos);
		return env.defineSymbol(node.symbol, "const", value[1], value[0]);
	}

	visitVarDeclaration(node, env) {
		const res = new CompileResult();
		const startPos = this.instructions.length;
		let address = env.assignAddress;

		const value = node.value
			? res.register(this.visit(node.value, env))
			: [null, node.dataType];
		if (res.error) return res;

		if (value[1] != node.dataType) {
			return res.fail(
				new CustomTypeError(
					node.startPos,
					node.endPos,
					`Cannot assign [${value[1]}] to [${node.dataType}].`
				)
			);
		}

		res.register(env.defineSymbol(node.symbol, "var", node.dataType));
		if (res.error) return res;

		if (node.value != null) {
			env.definedVars.push(node.symbol.symbol);
			if (this.KNOWN_VALUES.includes(value[0]))
				this.instructions = this.instructions.slice(0, startPos);
			this.loadImmediate(address);
			this.comment(node.symbol.symbol);
			this.write(
				this.KNOWN_VALUES.includes(value[0])
					? `COMP ${value[0]} M`
					: "COMP D M"
			);
		}

		return res.success([null, node.dataType]);
	}

	visitForLoop(node, env) {
		const res = new CompileResult();
		const operationMap = new Map([
			[TT.ADD, "+"],
			[TT.SUB, "-"],
			[TT.AND, "&"],
			[TT.OR, "|"],
			[TT.XOR, "^"],
			[TT.MUL, "*"],
			[TT.RSHIFT, ">>"],
			[TT.LSHIFT, "<<"],
			[TT.ASSIGN, "="],
			[TT.ADDTO, "+="],
			[TT.SUBBY, "-="],
			[TT.LT, "<"],
			[TT.LE, "<="],
			[TT.GT, ">"],
			[TT.GE, ">="],
			[TT.EQ, "=="],
			[TT.NE, "!="],
		]);
		const endOperation = operationMap.get(node.endToken.tokenType);

		const startValue = res.register(
			this.visit(
				new BinaryOperation(
					node.identifier.startPos,
					node.startValue.endPos,
					node.identifier,
					new Token(null, null, TT.ASSIGN),
					node.startValue
				),
				env
			)
		);
		if (res.error) return res;

		const forJmp = this.makeNewJump();
		this.write(`.forLoop${forJmp}`);
		this.tabs++;

		res.register(this.visit(node.body, env));
		if (res.error) return res;

		res.register(
			this.visit(
				new BinaryOperation(
					node.stepValue.startPos,
					node.stepValue.endPos,
					node.identifier,
					node.stepToken,
					node.stepValue
				),
				env
			)
		);
		if (res.error) return res;

		const endValue = res.register(this.visit(node.endValue, env));
		if (res.error) return res;

		if (endValue[1] !== startValue[1]) {
			return res.fail(
				new CustomTypeError(
					node.opToken.startPos,
					node.opToken.endPos,
					`Incompatible operation ('${endOperation}') between [${startValue[1]}] and [${endValue[1]}].`
				)
			);
		}

		const symbol = res.register(env.getSymbol(node.identifier));
		if (res.error) return res;
		this.loadImmediate(symbol[0]);
		this.comment(node.identifier.symbol);
		this.write("COMP D-M D");

		this.loadImmediate(`.forLoop${forJmp}`);
		switch (endOperation) {
			case "<":
				this.write("COMP D JGT");
				break;
			case "<=":
				this.write("COMP D JGE");
				break;
			case ">":
				this.write("COMP D JLT");
				break;
			case ">=":
				this.write("COMP D JLE");
				break;
		}

		this.tabs--;
		return res.success([null, null]);
	}

	visitWhileLoop(node, env) {
		const res = new CompileResult();
		const whileJmp = this.makeNewJump();

		this.write(`.whileLoop${whileJmp}`);
		this.tabs += 1;

		const condition = res.register(this.visit(node.condition, env));
		if (res.error) return res;

		if (condition[1] !== "bool") {
			return res.fail(
				new CustomTypeError(
					node.condition.startPos,
					node.condition.endPos,
					"Condition must be a boolean."
				)
			);
		}
		this.loadImmediate(`.endWhile${whileJmp}`);
		this.write("COMP D JEQ");

		res.register(this.visit(node.body, env));
		if (res.error) return res;

		this.loadImmediate(`.whileLoop${whileJmp}`);
		this.write("COMP 0 JMP");
		this.tabs--;
		this.write(`.endWhile${whileJmp}`);

		return res.success([null, null]);
	}

	visitIfStatement(node, env) {
		const res = new CompileResult();
		const endIfJmp = this.makeNewJump();
		let ifBlockJmp, condition, startPos;

		for (const ifCase of node.cases) {
			// ifCase is an array, where [0] = condition and [1] = body
			ifBlockJmp = this.makeNewJump();
			startPos = this.instructions.length;
			condition = res.register(this.visit(ifCase[0], env));
			if (res.error) return res;

			if (condition[1] !== "bool") {
				return res.fail(
					new CustomTypeError(
						ifCase[0].startPos,
						ifCase[0].endPos,
						"Condition must be a boolean."
					)
				);
			}

			if (condition[0] === -1) {
				this.instructions = this.instructions.slice(0, startPos);
				res.register(this.visit(ifCase[1], env));
				if (res.error) return res;
				if (ifBlockJmp - endIfJmp > 1) this.write(`.endIf${endIfJmp}`);
				this.jumps--;
				return res.success([null, null]);
			}
			if (condition[0] === 0) {
				this.instructions = this.instructions.slice(0, startPos);
				this.jumps--;
				continue;
			}

			this.loadImmediate(`.endIfBlock${ifBlockJmp}`);
			this.write("COMP D JEQ");

			res.register(this.visit(ifCase[1], env));
			if (res.error) return res;

			this.loadImmediate(`.endIf${endIfJmp}`);
			this.write("COMP 0 JMP");
			this.write(`.endIfBlock${ifBlockJmp}`);
		}

		if (node.elseCase) {
			res.register(this.visit(node.elseCase, env));
			if (res.error) return res;
		}

		if (ifBlockJmp - endIfJmp > 1) this.write(`.endIf${endIfJmp}`);
		return res.success([null, null]);
	}
}

const run = (fn, ftxt) => {
	const lexer = new Lexer(fn, ftxt);
	const lexResult = lexer.lex();
	if (lexResult.error) return lexResult;

	const parser = new Parser(lexResult.value);
	const ast = parser.parse();
	if (ast.error) return ast;

	console.log(ast.value);

	const compiler = new Compiler();
	const result = compiler.compile(ast.value);
	return result;
};

document.addEventListener("keydown", (e) => {
	if (e.key === "Tab" && e.target.id === "codeEdit") {
		e.preventDefault(); // Prevent default tab behavior (losing focus)

		const textarea = e.target;
		const start = textarea.selectionStart;
		const end = textarea.selectionEnd;

		textarea.value =
			textarea.value.substring(0, start) +
			"\t" +
			textarea.value.substring(end);
		textarea.selectionStart = textarea.selectionEnd = start + 1;
	}
	if (e.key === "Enter" && e.target.id === "codeEdit") {
		setTimeout(() => {
			const textarea = e.target;
			const start = textarea.selectionStart;
			const value = textarea.value;

			const prevLineStart = value.lastIndexOf("\n", start - 2) + 1;
			const prevLineEnd = value.indexOf("\n", prevLineStart);
			const prevLine =
				prevLineEnd === -1
					? value.slice(prevLineStart)
					: value.slice(prevLineStart, prevLineEnd);

			const indentMatch = prevLine.match(/^[\t ]*/);
			const indent = indentMatch ? indentMatch[0] : "";

			const newPos = textarea.selectionStart + indent.length;
			textarea.value =
				value.substring(0, textarea.selectionStart) +
				indent +
				value.substring(textarea.selectionStart);
			textarea.selectionStart = textarea.selectionEnd = newPos;
		}, 0);
	}
});

document.getElementById("runButton").addEventListener("click", (event) => {
	const codeEdit = document.getElementById("codeEdit");
	const output = document.getElementById("output");
	const ftxt = codeEdit.value;

	let strResult;
	if (ftxt) {
		const result = run("code", ftxt);
		if (result.error) {
			output.style.color = "red";
			strResult = `${result.error}`;
		} else {
			output.style.color = "white";
			strResult = `${result.value}`;
		}
	} else {
		strResult = "";
	}

	output.value = strResult;
});
