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
	EOF: 34,

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
		}, column ${this.startPos.col + 1}:\n\n${this.errorName}: ${
			this.details
		}`;
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

class ProcessResult {
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
		const res = new ProcessResult();

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
		const res = new ProcessResult();

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
				} else {
					tokens.push(new Token(startPos, this.pos.copy(), TT.ADD));
				}
			} else if (this.currentChar == "-") {
				this.advance();
				if (this.currentChar == "-") {
					this.advance();
					tokens.push(new Token(startPos, this.pos.copy(), TT.DEC));
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
		const res = new ProcessResult();
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
		const res = new ProcessResult();

		const ast = res.register(this.statements());
		return res.success(ast);
	}

	statements(end = [TT.EOF]) {
		const res = new ProcessResult();
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
			}
		}
		return this.expr();
	}

	varDeclaration() {
		const res = new ProcessResult();
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
		const res = new ProcessResult();
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

	expr() {
		return this.assignment();
	}

	assignment() {
		const res = new ProcessResult();
		const left = res.register(this.comparison());
		if (res.error) return res;

		if (this.currentTok.tokenType == TT.ASSIGN) {
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
		return this.bitwise();
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
		const res = new ProcessResult();
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
		const res = new ProcessResult();
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
				const expression = res.register(this.expr());
				if (res.error) return res;

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
			default:
				return res.fail(
					new InvalidSyntax(
						tok.startPos,
						tok.endPos,
						"Expected a number or an identifier."
					)
				);
		}
	}
}

class Environment {
	constructor() {
		this.symbols = new Map([
			["true", [-1, "bool"]],
			["false", [0, "bool"]],
			["N_BITS", [16, "int"]],
		]);
		this.constants = ["true", "false", "N_BITS"];
		this.assignAddress = 16;
	}

	defineSymbol(symbol, varType, dataType, value = null) {
		const res = new ProcessResult();

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
		const res = new ProcessResult();
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
		const res = new ProcessResult();
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

		return res.success(`.r${freeRegister}`);
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
		const res = new ProcessResult();
		const env = new Environment();

		this.jumps = 0;
		this.tabs = 0;

		if (!ast.body) return res.success("HALT");

		res.register(this.visit(ast, env));
		if (res.error) return res;
		this.write("HALT");

		return res.success(this.instructions.join("\n"));
	}

	noVisitMethod(node, env) {
		return new ProcessResult().fail(
			new CodingError(
				node.startPos,
				node.endPos,
				`No visit${node.constructor.name} method defined.`
			)
		);
	}

	visit(node, env) {
		const visitMethodName = `visit${node.constructor.name}`;
		const visitMethod = this.methodNames.includes(visitMethodName)
			? this[visitMethodName]
			: this.noVisitMethod;

		return visitMethod.call(this, node, env);
	}

	visitStatements(node, env) {
		const res = new ProcessResult();
		for (const stmt of node.body) {
			res.register(this.visit(stmt, env));
			if (res.error) return res;
		}

		return res.success(null);
	}

	visitNumericLiteral(node, env) {
		const res = new ProcessResult();
		if (this.KNOWN_VALUES.includes(node.value))
			this.write(`COMP ${node.value} D`);
		else {
			this.loadImmediate(node.value);
			this.write("COMP A D");
		}

		return res.success([node.value, "int"]);
	}

	visitIdentifier(node, env) {
		const res = new ProcessResult();
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

	visitBinaryOperation(node, env) {
		const res = new ProcessResult();

		const operationMap = new Map([
			[TT.ADD, "+"],
			[TT.SUB, "-"],
			[TT.AND, "&"],
			[TT.OR, "|"],
			[TT.XOR, "^"],
		]);
		const operation = operationMap.get(node.opToken.tokenType);

		if (operation === undefined) {
			return res.fail(
				new CodingError(
					node.opToken.startPos,
					node.opToken.endPos,
					"Undefined operation."
				)
			);
		}

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

		this.loadImmediate(leftRegister);
		this.write(
			`COMP ${operation == "-" ? "M-D" : "D" + operation + "M"} D`
		);

		return res.success([null, left[1]]);
	}

	visitUnaryOperation(node, env) {
		const res = new ProcessResult();

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
				default:
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
		const res = new ProcessResult();
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
		const res = new ProcessResult();
		const startPos = this.instructions.length;
		let address = env.assignAddress;

		const value = node.value
			? res.register(this.visit(node.value, env))
			: null;
		if (res.error) return res;

		if (value[1] != node.dataType) {
			return res.fail(
				new CustomTypeError(
					node.startPos,
					node.endPos,
					`Cannot assign [${value[1]}] to [${node.dataType.value}].`
				)
			);
		}

		res.register(env.defineSymbol(node.symbol, "var", node.dataType));

		if (value) {
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
}

const run = (fn, ftxt) => {
	const lexer = new Lexer(fn, ftxt);
	const lexResult = lexer.lex();
	if (lexResult.error) return lexResult;

	const parser = new Parser(lexResult.value);
	const ast = parser.parse();
	if (ast.error) return ast;

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
