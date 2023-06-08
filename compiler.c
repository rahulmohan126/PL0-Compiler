/**
 * Authors: Rahul Mohan
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    GENERAL_ERROR,
    LEX_IDENTIFIER_TOO_LONG,
    LEX_NUMBER_TOO_LONG,
    LEX_COMMENT_NEVER_ENDED,
    LEX_IDENTIFIER_INVALID_START,
    LEX_INVALID_SYMBOL,
    PCG_PERIOD_EXPECTED,
    PCG_IDENTIFIER_EXPECTED_DECLARATION,
    PCG_IDENTIFIER_EXPECTED,
    PCG_SYMBOL_IN_USE,
    PCG_EQUAL_EXPECTED,
    PCG_EQUAL_EXPECTED_NOT_BECOME,
    PCG_VALUE_EXPECTED,
    PCG_SEMICOLON_COMMA_EXPECTED,
    PCG_SEMICOLON_EXPECTED,
    PCG_VAR_EXPECTED,
    PCG_BECOME_EXPECTED,
    PCG_RIGHT_PARENTHESIS_EXPECTED,
    PCG_PROCEDURE_EXPECTED,
    PCG_STATEMENT_EXPECTED,
    PCG_END_SEMICOLON_EXPECTED,
    PCG_MAX_LEVEL,
    PCG_THEN_EXPECTED,
    PCG_DO_EXPECTED,
    PCG_UNDECLARED_SYMBOL,
    PCG_COMPARISON_OPERATOR_EXPECTED,
    PCG_INVALID_EXPRESSION,
    PCG_PROCEDURE_IN_EXPRESSION,
    PCG_END_OF_PROGRAM_REMAINING,
} error_code;

void raise_error(int error_code);

// #region LEXER

// Constants

#define MAX_TEXT_SIZE 10000
#define MAX_TOKEN_SIZE 10000

// Structs and Enums

typedef struct {
    int type;
    char value[12];
} token;

typedef enum {
    skipsym = 1,
    identsym = 2,
    numbersym = 3,
    plussym = 4,
    minussym = 5,
    multsym = 6,
    slashsym = 7,
    oddsym = 8,
    eqlsym = 9,
    neqsym = 10,
    lessym = 11,
    leqsym = 12,
    gtrsym = 13,
    geqsym = 14,
    lparentsym = 15,
    rparentsym = 16,
    commasym = 17,
    semicolonsym = 18,
    periodsym = 19,
    becomessym = 20,
    beginsym = 21,
    endsym = 22,
    ifsym = 23,
    thensym = 24,
    whilesym = 25,
    dosym = 26,
    callsym = 27,
    constsym = 28,
    varsym = 29,
    procsym = 30,
    writesym = 31,
    readsym = 32,
    elsesym = 33
} token_type;

// Global variables

char text[MAX_TEXT_SIZE];
token lexeme[MAX_TOKEN_SIZE];
int text_size = 0;
int text_index = 0;
int token_count = 0;

void load_program(char* filename);
void tokenize(int n, char* value);
void substr(char* src, char* dest, int s, int e);
int is_relation(int t);
int is_alpha(char c);
int is_digit(char c);
int is_special_symbol(char c);
int is_reserved_word(char c);
int is_ident(char c);
int is_num(char c);
int is_comment(char c);
int is_whitespace(char c);
void generate_tokens();

void load_program(char* filename) {
    // Open file
    FILE* fp = fopen(filename, "r");

    // Input
    char c;
    while ((c = fgetc(fp)) != EOF) {
        text[text_index] = c;
        text_index++;
    }

    fclose(fp);
}

// Tokenizes into lexeme array using type and value if given
// If the token is a number or identifier, it is assumed that the value
// has already been copied into the appropriate memory slot
void tokenize(int n, char* value) {
    // Copy type
    lexeme[token_count].type = n;

    // Copy value (if no value given, get lexeme from lookup table)
    // if (value == NULL) strcpy(lexeme[token_count].value, TOKEN_LOOKUP_TABLE[n
    // - 4]); else strcpy(lexeme[token_count].value, value);
    if (value != NULL) strcpy(lexeme[token_count].value, value);

    // Increase number of tokens
    token_count++;
}

// Places substring from S to E into dest string
void substr(char* src, char* dest, int s, int e) {
    int i = 0;
    for (; s < e; s++) {
        dest[i] = src[s];
        i++;
    }

    dest[i] = '\0';
}

int is_relation(int t) { return 9 <= t && t <= 14; }

int is_alpha(char c) { return (65 <= c && c <= 90) || (97 <= c && c <= 122); }

int is_digit(char c) { return 48 <= c && c <= 57; }

// Checks if the current character is special symbol, returning 0 if not
// and returning the type if it is
int is_special_symbol(char c) {
    switch (c) {
        case '+':
            return plussym;
        case '-':
            return minussym;
        case '*':
            return multsym;
        case '/':
            // If "/" is the start of a comment block, ignore
            if (text_index + 1 < text_size && text[text_index + 1] == '*') {
                return 0;
            }
            return slashsym;
        case '=':
            return eqlsym;
        case '!':
            if (text_index + 1 < text_size && text[text_index + 1] == '=') {
                text_index++;
                return neqsym;
            }
            break;
        case '<':
            if (text_index + 1 < text_size && text[text_index + 1] == '=') {
                text_index++;
                return leqsym;
            } else {
                return lessym;
            }
        case '>':
            if (text_index + 1 < text_size && text[text_index + 1] == '=') {
                text_index++;
                return geqsym;
            } else {
                return gtrsym;
            }
        case '(':
            return lparentsym;
        case ')':
            return rparentsym;
        case ',':
            return commasym;
        case ';':
            return semicolonsym;
        case '.':
            return periodsym;
        case ':':
            if (text_index + 1 < text_size && text[text_index + 1] == '=') {
                text_index++;
                return becomessym;
            }
            break;
    }

    return 0;
}

// Checks if the current character is the start of a reserved word,
// returning 0 if not and returning the type if it is
int is_reserved_word(char c) {
    // Checks if the character is alphabetic
    if (!is_alpha(c)) {
        return 0;
    }

    // Keeps going until end of program or next character is no longer
    // alphabetic
    int end = text_index + 1;
    while (end < text_size && is_alpha(text[end])) {
        end++;
    }

    // Attempting to name identifier with a number subsequent, so ignore.
    // For example "begin4" is a valid identifier but not a reserved word
    if (end < text_size && is_digit(text[end])) {
        return 0;
    }

    // Creates a substring from the potential reserved word
    char potential_word[end - text_index + 1];
    substr(text, potential_word, text_index, end);

    // Update program_index in case to end of the reserved word
    int temp = text_index;
    text_index = end - 1;

    if (strcmp(potential_word, "odd") == 0) {
        return oddsym;
    } else if (strcmp(potential_word, "begin") == 0) {
        return beginsym;
    } else if (strcmp(potential_word, "end") == 0) {
        return endsym;
    } else if (strcmp(potential_word, "if") == 0) {
        return ifsym;
    } else if (strcmp(potential_word, "then") == 0) {
        return thensym;
    } else if (strcmp(potential_word, "while") == 0) {
        return whilesym;
    } else if (strcmp(potential_word, "do") == 0) {
        return dosym;
    } else if (strcmp(potential_word, "call") == 0) {
        return callsym;
    } else if (strcmp(potential_word, "const") == 0) {
        return constsym;
    } else if (strcmp(potential_word, "var") == 0) {
        return varsym;
    } else if (strcmp(potential_word, "procedure") == 0) {
        return procsym;
    } else if (strcmp(potential_word, "write") == 0) {
        return writesym;
    } else if (strcmp(potential_word, "read") == 0) {
        return readsym;
    } else if (strcmp(potential_word, "else") == 0) {
        return elsesym;
    } else {
        // If the string is not reserved word, reset program index to original
        // character
        text_index = temp;
        return 0;
    }
}

// Checks if the current character is the start of an identifier, returning 0 if
// not the ending index of the identifier if it is and -1 if it is too long
int is_ident(char c) {
    // If the first character is not alphabetic, cannot be identifier
    if (!is_alpha(c)) {
        return 0;
    }

    // Keeps going until a non-alphanumeric character is reached
    int end = text_index + 1;
    while (end < text_size && (is_alpha(text[end]) || is_digit(text[end]))) {
        end++;
    }

    // Identifier too long
    if (end - text_index > 11) {
        text_index = end - 1;
        return -1;
    }

    return end;
}

// Checks if the current character is the start of a number, returning 0 if not
// the ending index of the identifier if it is and -1 if it is too long and -2
// if there are letters subsequent to the digits, implying an attempt to name
// an identifier starting with a number.
int is_num(char c) {
    // If the first character is not a number, cannot be identifier
    if (!is_digit(c)) {
        return 0;
    }

    // Keeps going until a non-numeric character is reached
    int end = text_index + 1;
    while (end < text_size && is_digit(text[end])) {
        end++;
    }

    // A sequence of numbers immediately followed by a letter implies an
    // identifier starting with a number, which is invalid, example: "45x" or
    // "2test"
    if (end < text_size && is_alpha(text[end])) {
        text_index = end - 1;
        return -2;
    }

    // Number too long
    if (end - text_index > 5) {
        text_index = end - 1;
        return -1;
    }

    return end;
}

// Returns if the current character is the start of a comment block
int is_comment(char c) {
    return c == '/' && (text_index + 1 < text_size) &&
           text[text_index + 1] == '*';
}

// Returns if the current character is whitespace
int is_whitespace(char c) { return c == ' ' || c == '\n' || c == '\t'; }

// Exeecutes lexical analyzer
void generate_tokens() {
    // Set total program size and reset program counter
    text_size = text_index;
    text_index = 0;

    while (text_index < text_size) {
        int res;
        // Check if the current character is a special symbol (or start of
        // special symbol)
        if ((res = is_special_symbol(text[text_index]))) {
            tokenize(res, NULL);
        }
        // Check if the current character is the start of a reserved word
        else if ((res = is_reserved_word(text[text_index]))) {
            tokenize(res, NULL);
        }
        // Check if the current character is the start of a identifier
        else if ((res = is_ident(text[text_index]))) {
            // Error for too long identifier
            if (res == -1) {
                raise_error(LEX_IDENTIFIER_TOO_LONG);
            } else {
                // Copy identifier name in token struct
                char temp[12];
                substr(text, temp, text_index, res);

                tokenize(identsym, temp);
                text_index = res - 1;
            }
        }
        // Check if the current character is the start of a number
        // (this is also where identifiers that start with a number are caught
        // and the appropriate error is given)
        else if ((res = is_num(text[text_index]))) {
            // Error for too long number
            if (res == -1) {
                raise_error(LEX_NUMBER_TOO_LONG);
            }
            // Error for identifier that starts with number
            else if (res == -2) {
                raise_error(LEX_IDENTIFIER_INVALID_START);
            } else {
                // Copy number literal in token struct
                char temp[6];
                substr(text, temp, text_index, res);

                tokenize(numbersym, temp);
                text_index = res - 1;
            }
        }
        // Check if the current character is the start of a comment block
        else if ((res = is_comment(text[text_index]))) {
            text_index += 2;
            while (text_index + 1 < text_size &&
                   !(text[text_index] == '*' && text[text_index + 1] == '/')) {
                text_index++;
            }

            // Comment not ended
            if (text_index + 1 >= text_size) {
                raise_error(LEX_COMMENT_NEVER_ENDED);
            }
            // Skips past the last "/" at the end of the comment block.
            else {
                text_index++;
            }
        }
        // Ignore
        else if ((res = is_whitespace(text[text_index]))) {
        }
        // If the character passes all of the previous check it is an invalid
        // character
        else {
            raise_error(LEX_INVALID_SYMBOL);
        }

        text_index++;
    }
}

// #endregion

// #region PARSER + CODEGEN

// Constants

#define MAX_CODE_SIZE 10000
#define MAX_SYMBOL_TABLE_SIZE 1000
#define MAX_LEVEL 10

// Structs and Enums

typedef struct {
    int kind;                        // const = 1, var = 2, proc = 3
    char name[12];  // name up to 11 chars
    int val;                         // number
    int level;                       // L level
    int addr;                        // M address
    int mark;
} symbol;

typedef struct {
    int op;
    int L;
    int M;
} instruction;

typedef enum {
    IT_LIT = 1,
    IT_OPR = 2,
    IT_LOD = 3,
    IT_STO = 4,
    IT_CAL = 5,
    IT_INC = 6,
    IT_JMP = 7,
    IT_JPC = 8,
    IT_SYS = 9
} instruction_type;

typedef enum {
    OP_RTN = 0,
    OP_ADD = 1,
    OP_SUB = 2,
    OP_MULT = 3,
    OP_DIV = 4,
    OP_EQL = 5,
    OP_NEQ = 6,
    OP_LSS = 7,
    OP_LEQ = 8,
    OP_GTR = 9,
    OP_GEQ = 10,
    OP_ODD = 11,
    OP_NEG = 12
} operation_type;

// Global variables

token current;
int token_index = 0;
int current_level = -1;

symbol symbol_table[MAX_SYMBOL_TABLE_SIZE];
int symbol_count = 0;

instruction code[MAX_CODE_SIZE];
int code_count = 0;

// Helper functions
void next_token();
void insert_symbol(int kind, char* name, int val, int level, int addr);
int get_symbol(char* symbol_name);
void mark_symbols();
void emit(int op, int L, int M);

// Recursive descent functions
void program();
void block();
void const_declaration();
void var_declaration(int* data_address);
void proc_declaration();
void statement();
void condition();
void expression();
void term();
void factor();

// Prints error message and exits immediately
void raise_error(int error_code) {
    printf("%d\n", token_index);
    printf("%s\n\n", text);

    printf("Error number %d, ", error_code);
    switch (error_code) {
        case LEX_IDENTIFIER_TOO_LONG:
            printf("Identifiers can have maximum 11 characters");
            break;
        case LEX_NUMBER_TOO_LONG:
            printf("Numbers can have maximum 5 digits.");
            break;
        case LEX_COMMENT_NEVER_ENDED:
            printf("Comment block never ended.");
            break;
        case LEX_IDENTIFIER_INVALID_START:
            printf("Identifier cannot start with a number.");
            break;
        case LEX_INVALID_SYMBOL:
            printf("Invalid symbol.");
            break;
        case PCG_PERIOD_EXPECTED:
            printf("'.' missing at the end of program.");
            break;
        case PCG_IDENTIFIER_EXPECTED_DECLARATION:
            printf(
                "'const', 'var', or 'procedure' must be followed by "
                "identifier.");
            break;
        case PCG_IDENTIFIER_EXPECTED:
            printf("Expected an identifier.");
            break;
        case PCG_SYMBOL_IN_USE:
            printf("Symbol name has already been declared.");
            break;
        case PCG_EQUAL_EXPECTED:
            printf("Constant identifier must be followed by '='.");
            break;
        case PCG_EQUAL_EXPECTED_NOT_BECOME:
            printf("Use '=' instead of ':='.");
            break;
        case PCG_VALUE_EXPECTED:
            printf("'=' must be followed by a number.");
            break;
        case PCG_SEMICOLON_COMMA_EXPECTED:
            printf("Expected ',' or ';'.");
            break;
        case PCG_SEMICOLON_EXPECTED:
            printf("Expected ';'.");
            break;
        case PCG_VAR_EXPECTED:
            printf("Expected an identifier of type var.");
            break;
        case PCG_BECOME_EXPECTED:
            printf("Var identifiers must be followed by ':='.");
            break;
        case PCG_RIGHT_PARENTHESIS_EXPECTED:
            printf("Right parenthesis missing.");
            break;
        case PCG_PROCEDURE_EXPECTED:
            printf("Only procedure identifiers can be called.");
            break;
        case PCG_STATEMENT_EXPECTED:
            printf("Statement expected.");
            break;
        case PCG_END_SEMICOLON_EXPECTED:
            printf("'end' or ';' expected.");
            break;
        case PCG_MAX_LEVEL:
            printf("Max nested blocks exceeded.");
            break;
        case PCG_THEN_EXPECTED:
            printf("'then' expected.");
            break;
        case PCG_DO_EXPECTED:
            printf("'do' expected.");
            break;
        case PCG_UNDECLARED_SYMBOL:
            printf("Identifier never declared.");
            break;
        case PCG_COMPARISON_OPERATOR_EXPECTED:
            printf("Relational operator expected.");
            break;
        case PCG_INVALID_EXPRESSION:
            printf(
                "In valid expression. Ensure expression contain only operands, "
                "parentheses, numbers, or symbols and no double operands or symbols.");
            break;
        case PCG_PROCEDURE_IN_EXPRESSION:
            printf("Expression must not contain procedure identifier.");
            break;
        case PCG_END_OF_PROGRAM_REMAINING:
            printf(
                "Program block ended with period but there were still "
                "remaining tokens");
            break;
        default:
            printf("Unspecified Error.");
            break;
    }
    printf("\n");
    exit(0);
}

// Gets the next token in lexeme list
void next_token() {
    if (token_index < token_count) {
        current = lexeme[token_index++];
    }
    // Since a token of type -1 doesn't exist, it will trigger the closest
    // error exception and terminate the program gracefully
    else {
        token temp;
        temp.type = -1;
        current = temp;
    }
}

// Inserts a new symbol
void insert_symbol(int kind, char* name, int val, int level, int addr) {
    symbol_table[symbol_count].kind = kind;
    strcpy(symbol_table[symbol_count].name, name);
    symbol_table[symbol_count].val = val;
    symbol_table[symbol_count].level = level;
    symbol_table[symbol_count].addr = addr;
    symbol_table[symbol_count].mark = 0;
    symbol_count++;
}

// Get symbol if exists in table and within the current scope
// Backwards so most recent vars are given first
int get_symbol(char* symbol_name) {
    for (int i = symbol_count - 1; i >= 0; i--) {
        // Checks if matching symbol name, valid level, and unmarked
        if (strcmp(symbol_table[i].name, symbol_name) == 0 &&
            current_level >= symbol_table[i].level && !symbol_table[i].mark) {
            return i;
        }
    }
    return -1;
}

// Checks if a symbol name can be declared
int declared_symbol(char* symbol_name) {
    int idx = get_symbol(symbol_name);

    // If the symbol name doesn't exist than it is permitted
    // Or if the same symbol exists in a broader scope than it is still
    // permissible
    return !(idx == -1 || symbol_table[idx].level < current_level);
}

// Marks all symbols in the current level
void mark_symbols() {
    for (int i = 0; i < symbol_count; i++) {
        if (current_level <= symbol_table[i].level) {
            symbol_table[i].mark = 1;
        }
    }
}

// Emits the assembly instruction to the instructions array
void emit(int op, int L, int M) {
    code[code_count].op = op;
    code[code_count].L = L;
    code[code_count].M = M;

    code_count++;
}

// program ::= block "."
void program() {
    next_token();
    block();

    // Checks if ending with period
    if (current.type != periodsym) {
        return raise_error(PCG_PERIOD_EXPECTED);
    }
    // Checks if there are additional tokens after the final period
    else if (token_index < token_count) {
        return raise_error(PCG_END_OF_PROGRAM_REMAINING);
    }

    // Emit exit code
    emit(IT_SYS, 0, 3);
}

// block ::= const-declaration var-declaration proc-declaration statement
void block() {
    current_level++;

    if (current_level > MAX_LEVEL) {
        raise_error(PCG_MAX_LEVEL);
    }

    int data_address = 3;
    int tx0 = symbol_count - 1;

    // Placeholder jump instruction
    emit(IT_JMP, 0, 0);

    const_declaration();
    var_declaration(&data_address);
    proc_declaration();

    // The placeholder jump address is fixed up
    code[symbol_table[tx0].addr].M = code_count;
    // The space for address for the above jmp is now occupied by the new cx
    symbol_table[tx0].addr = code_count;

    // Jump for number of variables
    emit(IT_INC, 0, data_address);

    statement();

    // If not top level, return
    if (current_level > 0) {
        emit(IT_OPR, 0, OP_RTN);
    }

    // At the end of block, make all symbols declared in scope so they are
    // ignored
    mark_symbols();

    current_level--;
}

// const-declaration ::= ["const" ident "=" number {"," ident "=" number} ";"]
void const_declaration() {
    if (current.type == constsym) {
        do {
            // Check for identifier
            next_token();
            if (current.type != identsym) {
                return raise_error(PCG_IDENTIFIER_EXPECTED_DECLARATION);
            }

            // Check if identifier is already in use
            if (declared_symbol(current.value)) {
                return raise_error(PCG_SYMBOL_IN_USE);
            }

            char const_name[11];
            strcpy(const_name, current.value);

            // Check for equal symbol
            next_token();
            if (current.type == becomessym) {
                return raise_error(PCG_EQUAL_EXPECTED_NOT_BECOME);
            } else if (current.type != eqlsym) {
                return raise_error(PCG_EQUAL_EXPECTED);
            }

            // Check for literal number
            next_token();
            if (current.type != numbersym) {
                return raise_error(PCG_VALUE_EXPECTED);
            }

            // Add const to symbol table
            insert_symbol(1, const_name, atoi(current.value), current_level, 0);

            next_token();
        } while (current.type == commasym);

        // Check if ends with semicolon or is missing a comma
        if (current.type != semicolonsym) {
            return raise_error(PCG_SEMICOLON_COMMA_EXPECTED);
        }
        next_token();
    }
}

// var-declaration ::= [ "var" ident {"," ident} ";"]
void var_declaration(int* data_address) {
    // Checks if starts with var keyword
    if (current.type == varsym) {
        do {
            next_token();

            // Checks for identifier
            if (current.type != identsym) {
                return raise_error(PCG_IDENTIFIER_EXPECTED_DECLARATION);
            }

            // Checks if identifier isn't already being used
            if (declared_symbol(current.value)) {
                return raise_error(PCG_SYMBOL_IN_USE);
            }

            insert_symbol(2, current.value, 0, current_level, *data_address);

            (*data_address)++;

            next_token();
        } while (current.type == commasym);

        // Check if ends with semicolon or is missing a comma
        if (current.type != semicolonsym) {
            return raise_error(PCG_SEMICOLON_COMMA_EXPECTED);
        }

        next_token();
    }
}

// procedure-declaration ::= { "procedure" ident ";" block ";" }
void proc_declaration() {
    while (current.type == procsym) {
        // Checks for identifier
        next_token();
        if (current.type != identsym) {
            return raise_error(PCG_IDENTIFIER_EXPECTED_DECLARATION);
        }

        if (declared_symbol(current.value)) {
            return raise_error(PCG_SYMBOL_IN_USE);
        }

        // TODO double check this
        insert_symbol(3, current.value, 0, current_level, code_count);

        // Checks for semicolon
        next_token();
        if (current.type != semicolonsym) {
            return raise_error(PCG_SEMICOLON_EXPECTED);
        }

        next_token();
        block();

        // Checks for semicolon
        if (current.type != semicolonsym) {
            return raise_error(PCG_SEMICOLON_EXPECTED);
        }

        next_token();
    }
}

/**
 * statement   ::= [ ident ":=" expression
 *                  | "begin" statement { ";" statement } "end"
 *                  | "if" condition "then" statement
 *                  | "while" condition "do" statement
 *                  | "read" ident
 *                  | "write" expression
 *                  | empty
 *                 ]
 */
void statement() {
    // ident ":=" expression
    if (current.type == identsym) {
        int symIdx = get_symbol(current.value);

        // Checks if identifier was declared
        if (symIdx == -1) {
            return raise_error(PCG_UNDECLARED_SYMBOL);
        }

        // Checks if the identifier is a var
        if (symbol_table[symIdx].kind != 2) {
            return raise_error(PCG_VAR_EXPECTED);
        }

        // Checks if the identifier is followed by a become symbol
        next_token();
        if (current.type != becomessym) {
            return raise_error(PCG_BECOME_EXPECTED);
        }

        next_token();
        expression();

        // Emit store, assigns new value of var
        emit(IT_STO, current_level - symbol_table[symIdx].level,
             symbol_table[symIdx].addr);
    }
    // "call" ident
    else if (current.type == callsym) {
        // Checks if "call" is followed by an identifier
        next_token();
        if (current.type != identsym) {
            return raise_error(-1);
        }

        int symIdx = get_symbol(current.value);

        // Checks if identifier was declared
        if (symIdx == -1) {
            return raise_error(PCG_UNDECLARED_SYMBOL);
        }

        // Checks if the identifier is a procedure
        if (symbol_table[symIdx].kind != 3) {
            return raise_error(PCG_PROCEDURE_EXPECTED);
        }

        // Emits call to procedure
        // Double check level
        emit(IT_CAL, current_level - symbol_table[symIdx].level,
             symbol_table[symIdx].addr);

        next_token();
    }
    // "begin" statement { ";" statement } "end"
    else if (current.type == beginsym) {
        // Processes 1 or more statements separated by semicolon
        do {
            next_token();
            statement();
        } while (current.type == semicolonsym);

        // Checks if begin has corresponding end
        if (current.type != endsym) {
            return raise_error(PCG_END_SEMICOLON_EXPECTED);
        }
        next_token();
    }
    // "if" condition "then" statement
    else if (current.type == ifsym) {
        // Process condition
        next_token();
        condition();

        // Emit conditional jump
        emit(IT_JPC, 0, 0);
        // Save index of jpc instruction
        // (so we can change the jpc destination afterwards)
        int jpcIdx = code_count - 1;

        // Checks if "if" has corresponding "then"
        if (current.type != thensym) {
            return raise_error(PCG_THEN_EXPECTED);
        }

        // Process body of the conditional branch
        next_token();
        statement();

        // Change JPC instruction's jump position to be after if-then
        code[jpcIdx].M = code_count;
    }
    // "while" condition "do" statement
    else if (current.type == whilesym) {
        int loop_beginning = code_count;

        // Loop condition
        next_token();
        condition();

        // Checks if do symbol is after condition
        if (current.type != dosym) {
            return raise_error(PCG_DO_EXPECTED);
        }

        // Emit conditional jump
        emit(IT_JPC, current_level, 0);
        // Save index of jpc instruction
        // (so we can change the jpc destination afterwards)
        int jpcIdx = code_count - 1;

        // Process body of loop
        next_token();
        statement();

        // Put JMP for loop to restart at the beginning
        emit(IT_JMP, current_level, loop_beginning);

        // Change JPC instruction's jump position to be at the end of the loop
        code[jpcIdx].M = code_count;
    }
    // "read" ident
    else if (current.type == readsym) {
        // Checks if identifier
        next_token();
        if (current.type != identsym) {
            return raise_error(PCG_IDENTIFIER_EXPECTED);
        }

        // Checks if symbol exists
        int symIdx = get_symbol(current.value);
        if (symIdx == -1) {
            return raise_error(PCG_UNDECLARED_SYMBOL);
        }

        // Checks if symbol is var
        if (symbol_table[symIdx].kind != 2) {
            return raise_error(PCG_VAR_EXPECTED);
        }

        // Emit read input
        emit(IT_SYS, 0, 2);

        // Emit store (assigns the inputted value to a var)
        emit(IT_STO, current_level - symbol_table[symIdx].level,
             symbol_table[symIdx].addr);

        next_token();
    }
    // "write" expression
    else if (current.type == writesym) {
        next_token();
        expression();

        // Emit output
        emit(IT_SYS, 0, 1);
    // Only possible symbol after statement is "end", ";", or "." So if none of
    // those are next, that means something is wrong.
    } else if (!(current.type == endsym || current.type == semicolonsym ||
                 current.type == periodsym)) {
        raise_error(PCG_STATEMENT_EXPECTED);
    }
}

// condition ::= ("odd" expression) | (expression rel-op expression)
void condition() {
    // "odd" expression
    if (current.type == oddsym) {
        next_token();
        expression();
        emit(IT_OPR, 0, OP_ODD);
    }
    // expression rel-op expression
    else {
        expression();
        if (!is_relation(current.type)) {
            return raise_error(PCG_COMPARISON_OPERATOR_EXPECTED);
        }
        int relation_type = current.type - 4;
        next_token();
        expression();
        emit(IT_OPR, 0, relation_type);
    }
}

// expression ::= ["+"|"-"] term { ("+"|"-") term }
void expression() {
    // ["+"|"-"]
    int negate = 0;
    if (current.type == plussym || current.type == minussym) {
        if (current.type == minussym) {
            negate = 1;
        }
        next_token();
    }

    // term
    term();

    // TODO Is negate operation valid?
    if (negate) {
        emit(IT_OPR, 0, OP_NEG);
    }

    // { ("+"|"-") term }
    while (current.type == plussym || current.type == minussym) {
        int current_opr = current.type == plussym ? OP_ADD : OP_SUB;
        next_token();
        term();
        emit(IT_OPR, 0, current_opr);
    }
}

// term ::= factor { ("*"|"/") factor }
void term() {
    // factor
    factor();

    // { ("*"|"/") factor }
    while (current.type == multsym || current.type == slashsym) {
        int current_opr = current.type == multsym ? OP_MULT : OP_DIV;
        next_token();
        factor();
        emit(IT_OPR, 0, current_opr);
    }
}

// factor ::= ident | number | ( "(" expression ")" )
void factor() {
    // ident
    if (current.type == identsym) {
        int symIdx = get_symbol(current.value);
        if (symIdx == -1) {
            return raise_error(PCG_UNDECLARED_SYMBOL);
        }
        if (symbol_table[symIdx].kind == 1) {
            emit(IT_LIT, 0, symbol_table[symIdx].val);
        } else if (symbol_table[symIdx].kind == 2) {
            emit(IT_LOD, current_level - symbol_table[symIdx].level,
                 symbol_table[symIdx].addr);
        } else {
            raise_error(PCG_PROCEDURE_IN_EXPRESSION);
        }
        next_token();
    }
    // number
    else if (current.type == numbersym) {
        emit(IT_LIT, 0, atoi(current.value));
        next_token();
    }
    // "(" expression ")"
    else if (current.type == lparentsym) {
        next_token();
        expression();
        if (current.type != rparentsym)
            return raise_error(PCG_RIGHT_PARENTHESIS_EXPECTED);
        next_token();
    } else {
        return raise_error(PCG_INVALID_EXPRESSION);
    }
}

// #endregion

int main(int argc, char* argv[]) {
    load_program(argv[1]);

    // Generates tokens using lexical analyzer
    generate_tokens();

    // Initial procedure
    insert_symbol(3, "main", 0, 0, code_count);

    program();

    printf("%s\n\nNo errors, program is syntactically correct\n\n", text);

    FILE* out = fopen("elf.txt", "w");

    // Successful Output
    // printf("Assembly Code:\n\n");
    // printf("Line    OP    L    M\n");
    for (int i = 0; i < code_count; i++) {
        // printf("%3d    %d    %d    %d\n", i, code[i].op, code[i].L,
        // code[i].M);

        // Multiply by three since code addresses are stored in an array in vm
        if (code[i].op == IT_JMP || code[i].op == IT_CAL ||
            code[i].op == IT_JPC) {
            code[i].M *= 3;
        }

        printf("%d %d %d\n", code[i].op, code[i].L, code[i].M);
        fprintf(out, "%d %d %d\n", code[i].op, code[i].L, code[i].M);
    }

    fclose(out);

    // printf("\nSymbol Table:\n\n");
    // printf("Kind | Name        | Value | Level | Address | Mark\n");
    // printf("---------------------------------------------------\n");
    // for (int i = 0; i < symbol_count; i++) {
    //     printf("%4d | %11s | %5d | %5d | %7d | %4d\n", symbol_table[i].kind,
    //     symbol_table[i].name, symbol_table[i].val, symbol_table[i].level,
    //     symbol_table[i].addr, symbol_table[i].mark);
    // }

    return 0;
}
