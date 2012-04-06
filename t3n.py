# AUTHOR: triplemstormm
# This code may be freely redistributed, but not sold
#
# Use and modification of this code is allowed, but credit
# must be given to the author

import ply.lex as lex
import ply.yacc as yacc
import sys

loopCount = 0

# List of reserved keywords
reserved = {
	'return' : 'RETURN',
	'break' : 'BREAK',
	'exit' : 'EXIT',
	'if' : 'IF',
	'elif' : 'ELIF',
	'else' : 'ELSE',
	'do' : 'DO',
	'while' : 'WHILE',
	'true' : 'TRUE',
	'false' : 'FALSE',
	'int' : 'T_INT',
	'void' : 'T_VOID'
}

# List of token names
tokens = [
	'ID',
	'INT',
	'STRING',
	'PLUS',
	'MINUS',
	'TIMES',
	'DIVIDE',
	'MOD',
	'LPAREN',
	'RPAREN',
	'LBRACK',
	'RBRACK',
	'LBLOCK',
	'RBLOCK',
	'LT','LEQ','GT','GEQ','EQ','NEQ',
	'ASSIGN',
	'EOS',
	'COMMA'
] + list(reserved.values())

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MOD     = r'%'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_RBRACK  = r'\]'
t_LBRACK  = r'\['
t_LBLOCK  = r'{'
t_RBLOCK  = r'}'
t_LT      = r'<'
t_GT      = r'>'
t_LEQ     = r'<='
t_GEQ     = r'>='
t_NEQ     = r'!='
t_ASSIGN  = r'='
t_EQ	  = r'=='
t_EOS     = r';'
t_COMMA	  = r','

# A regular expression rule with some action code
def t_INT(t):
	r'[0-9]+'
	t.value = int(t.value)    
	return t

def t_ID(t):
	r'[A-Za-z][A-Za-z0-9_]*'
	t.type = reserved.get(t.value, 'ID')
	return t	

def t_STRING(t):
	r'"[^"]*"'
	return t

# Define a rule so we can track line numbers
def t_newline(t):
 	r'\n+'
 	t.lexer.lineno += len(t.value)

def t_comment(t):
	r'\#[^\n]*'
	
# A string containing ignored characters (spaces and tabs)
def t_ws(t):
	r'\s'
	
# Error handling rule
def t_error(t):
	sys.stderr.write("line %d: illegal character (%s)\n" % (t.lexer.lineno,t.value[0]))
	exit(1)

# Build the lexer
lex.lex()

#####################################
#####################################
#####################################

#production rules

def p_start(p):
	'start : head optStms'
	
def p_head(p):
	'''head	: head fxn
			| '''
			
def p_optStms(p):
	'''optStms	: stms
				| '''
				
def p_type(p):
	''' type 	: T_INT 
				| T_VOID'''

def p_lvalue(p):
	'''lvalue	: ID
				| lvalue LBRACK expr RBRACK '''

def p_decl(p):
	'decl : type lvalue optAssign EOS'
	
def p_optAssign(p):
	'''optAssign	: ASSIGN expr
					| '''
					
def p_stms(p):
	'''stms	: stms stm
			| stm'''
			
def p_stm(p):
	'''stm	: decl
			| if
			| do
			| while
			| BREAK EOS
			| EXIT EOS
			| RETURN expr EOS
			| RETURN EOS
			| lvalue ASSIGN expr EOS
			| expr EOS
			| block '''
			
def p_fxn(p):
	'fxn : type ID LPAREN argLst RPAREN block'
	
def p_argLst(p):
	'''argLst 	: type lvalue argLstx
				| '''
def p_argLstx(p):
	'''argLstx	: argLstx COMMA type lvalue
				| '''
				
def p_block(p):
	'block : LBLOCK stms RBLOCK'
	
def p_if(p):
	'if : IF LPAREN expr RPAREN block elTag'
def p_elTag(p):
	'''elTag	: ELIF LPAREN expr RPAREN block elTag
				| ELSE block '''
				
def p_do(p):
	'do : DO block WHILE LPAREN expr RPAREN EOS'
	
def p_while(p):
	'while : WHILE LPAREN expr RPAREN block'
	
def p_expr(p):
	'''expr	: lvalue
			| INT
			| TRUE
			| FALSE
			| STRING
			| callL callR
			| expr LT expr
			| expr GT expr
			| expr LEQ expr
			| expr GEQ expr
			| expr NEQ expr
			| expr EQ expr
			| expr PLUS expr
			| expr MINUS expr
			| expr TIMES expr
			| expr DIVIDE expr
			| expr MOD expr
			| LPAREN expr RPAREN '''

def p_expr_uminus(p):
	'expr : MINUS expr %prec UMINUS'			
	
def p_callL(p):
	'callL : ID LPAREN'
	
def p_callR(p):
	'callR : exprlist RPAREN'
	
def p_exprlist(p):
	'''exprlist	: expr uexprlist
				| '''
				
def p_uexprlist(p):
	'''uexprlist	: uexprlist COMMA expr
					| '''
					
def p_error(p):
	if p == None:
		sys.stderr.write("Unexpected end of file\n")
	else:
		sys.stderr.write("line %r: syntax error near %r\n" % (p.lexer.lineno, p.value))
	exit(2)					

	
precedence = (
	('nonassoc', 'EQ', 'NEQ', 'GT', 'LT', 'GEQ', 'LEQ'),
	('left', 'PLUS', 'MINUS'),
	('left', 'TIMES', 'DIVIDE', 'MOD'),
	('right', 'UMINUS')
)	

yacc.yacc()

#####################################
#####################################
#####################################

# Give the lexer some input
import sys
lexInArr = sys.stdin.readlines()
lexIn = ""
for line in lexInArr:
	lexIn += line

yacc.parse(lexIn)