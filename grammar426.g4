grammar grammar426;

program
 : sourceElements? EOF
 ;

sourceElements
 : sourceElement+
 ;

sourceElement
 : variableStatement
 | typeDeclaration
 | functionDeclaration
 ;

statement
 : block
 | variableStatement
 | expressionStatement
 | iterationStatement
 | ifStatement
 | breakStatement
 | continueStatement
 | returnStatement
 | eos
 ;

block
 : '{' statementList? '}'
 ;

/// StatementList :
///     Statement
///     StatementList Statement
statementList
 : statement+
 ;

/// VariableStatement :
///     var VariableDeclarationList ;
variableStatement
 : Type variableDeclarationList eos
 ;

/// VariableDeclarationList :
///     VariableDeclaration
///     VariableDeclarationList , VariableDeclaration
variableDeclarationList
 : variableDeclaration ( ',' variableDeclaration )*
 ;

/// VariableDeclaration :
///     Identifier Initialiser?
variableDeclaration
 : Identifier initialiser?
 ;

/// Initialiser :
///     = AssignmentExpression
initialiser
 : '=' singleExpression
 ;

/// ExpressionStatement :
expressionStatement
 : expressionSequence
 ;

/// IfStatement :
///     if ( Expression ) Statement else Statement
///     if ( Expression ) Statement
ifStatement
 : If '(' expressionSequence ')' statement ( Else statement )?
 ;

iterationStatement
 : While '(' expressionSequence ')' statement
 ;

continueStatement
 : Continue eos
 ;

breakStatement
 : Break eos
 ;

returnStatement
 : Return expressionSequence? eos
 ;

typeDeclaration
 : Struct Identifier '{' ( Type Identifier eos )* '}' eos
 ;

/// FunctionDeclaration
functionDeclaration
 : Type Identifier '(' formalParameterList? ')' block
 ;

/// FormalParameterList :
///     Identifier
///     FormalParameterList , Identifier
formalParameterList
 : Type Identifier ( ',' Type Identifier )*
 ;

arguments
 : '(' argumentList? ')'
 ;

argumentList
 : singleExpression ( ',' singleExpression )*
 ;

expressionSequence
 : singleExpression ( ',' singleExpression )*
 ;


singleExpression
 : functionDeclaration                                                    # FunctionExpression
 | singleExpression '[' expressionSequence ']'                            # MemberIndexExpression
 | singleExpression arguments                                             # ArgumentsExpression
 | '-' singleExpression                                                   # UnaryMinusExpression
 | '~' singleExpression                                                   # BitNotExpression
 | '!' singleExpression                                                   # NotExpression
 | singleExpression ( '*' | '/' | '%' ) singleExpression                  # MultiplicativeExpression
 | singleExpression ( '+' | '-' ) singleExpression                        # AdditiveExpression
 | singleExpression ( '<' | '>' | '<=' | '>=' ) singleExpression          # RelationalExpression
 | singleExpression ( '==' | '!=' ) singleExpression                     # EqualityExpression
 | singleExpression And singleExpression                                 # LogicalAndExpression
 | singleExpression Or singleExpression                                 # LogicalOrExpression
 | singleExpression '=' singleExpression                                # AssignmentExpression
 | singleExpression assignmentOperator singleExpression                 # AssignmentOperatorExpression
 | Identifier                                                             # IdentifierExpression
 | literal                                                                # LiteralExpression
 | '(' expressionSequence ')'                                             # ParenthesizedExpression
 ;

/// AssignmentOperator : one of
///  *=	/=	%=	+=	-=	<<=	>>=	>>>=	&=	^=	|=
assignmentOperator
 : '*='
 | '/='
 | '%='
 | '+='
 | '-='
 ;

literal
 : ( BooleanLiteral
   | StringLiteral
   | CharLiteral
   )
 | numericLiteral
 ;

 numericLiteral
 : DecimalLiteral
 ;

identifierName
 : Identifier
 ;

eos
 : SemiColon
;

eof
 : EOF
 ;


LineTerminator
 : [\r\n\u2028\u2029] -> channel(HIDDEN)
 ;

Comma                      : ',';
OpenBracket                : '[';
CloseBracket               : ']';
OpenParen                  : '(';
CloseParen                 : ')';
OpenBrace                  : '{';
CloseBrace                 : '}';
SemiColon                  : ';';
Assign                     : '=';
QuestionMark               : '?';
Colon                      : ':';
Dot                        : '.';
Plus                       : '+';
Minus                      : '-';
BitNot                     : '~';
Not                        : '!';
Multiply                   : '*';
Divide                     : '/';
Modulus                    : '%';
LessThan                   : '<';
MoreThan                   : '>';
LessThanEquals             : '<=';
GreaterThanEquals          : '>=';
Equals                     : '==';
NotEquals                  : '!=';
And                        : 'and';
Or                         : 'or';
MultiplyAssign             : '*=';
DivideAssign               : '/=';
ModulusAssign              : '%=';
PlusAssign                 : '+=';
MinusAssign                : '-=';

/// 7.8.2 Boolean Literals
BooleanLiteral
 : 'true'
 | 'false'
 ;

/// 7.8.3 Numeric Literals
DecimalLiteral
 : DecimalIntegerLiteral '.' DecimalDigit*
 | '.' DecimalDigit+
 | DecimalIntegerLiteral
 ;

Type
 : Number
 | String
 | Void
 | Char
 | Struct Identifier
 ;

/// 7.6.1.1 Keywords
Break      : 'break';
Return     : 'return';
Continue   : 'continue';
For        : 'for';
Switch     : 'switch';
While      : 'while';
If         : 'if';
Else       : 'else';
Void       : 'void';
Number     : 'number';
String     : 'string';
Char       : 'char';
Struct     : 'struct';

/// 7.6 Identifier Names and Identifiers


WhiteSpaces
 : [\t\u000B\u000C\u0020\u00A0]+ -> channel(HIDDEN)
 ;


/// 7.4 Comments
MultiLineComment
 : '/*' .*? '*/' -> channel(HIDDEN)
 ;

SingleLineComment
 : '//' ~[\r\n\u2028\u2029]* -> channel(HIDDEN)
 ;

/// 7.8.4 String Literals
StringLiteral
 : '"' DoubleStringCharacter* '"'
 ;

/// 7.8.4 String Literals
CharLiteral
 : '\'' SingleStringCharacter '\''
 ;

Identifier
 : IdentifierStart IdentifierPart*
 ;

UnexpectedCharacter
 : .
 ;

fragment DecimalDigit
 : [0-9]
 ;

fragment DecimalIntegerLiteral
 : '0'
 | [1-9] DecimalDigit*
 ;


fragment DoubleStringCharacter
 : ~["\\\r\n]
 | '\\' EscapeSequence
 | LineContinuation
 ;

fragment SingleStringCharacter
 : ~['\\\r\n]
 | '\\' EscapeSequence
 | LineContinuation
 ;

fragment EscapeSequence
 : CharacterEscapeSequence
 | '0' // no digit ahead! TODO
 ;

fragment CharacterEscapeSequence
 : SingleEscapeCharacter
 | NonEscapeCharacter
 ;

fragment SingleEscapeCharacter
 : ['"\\bfnrtv]
 ;

fragment NonEscapeCharacter
 : ~['"\\bfnrtv0-9xu\r\n]
 ;
fragment EscapeCharacter
 : SingleEscapeCharacter
 | DecimalDigit
 | [xu]
 ;

fragment LineContinuation
 : '\\' LineTerminatorSequence
 ;

fragment LineTerminatorSequence
 : '\r\n'
 | '\n'
 | '\r'
 ;

 fragment IdentifierStart
 : [\p{L}]
 | [$_]
 ;

fragment IdentifierPart
 : IdentifierStart
 | [\p{Mn}]
 | [\p{Nd}]
 | [\p{Pc}]
 | ZWNJ
 | ZWJ
 ;
fragment ZWNJ
 : '\u200C'
 ;
fragment ZWJ
 : '\u200D'
 ;