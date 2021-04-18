
# lex = Tokenizer
# Breaks input text into a collection of specified tokens (using RegEx)

# yacc = Language syntax parser
# Recognize language syntax in the form of a context free grammar

import ply.lex as lex
import ply.yacc as yacc
import sys
import re
import ctypes

from SymbolsTableStructure import SymbolsTableStructure
from tokens import tokens
from reserved_words import reserved_words


symbolsTable = {}
symbolsTableIndex = 0

################################################################
############################ LEXER #############################
################################################################

# Regular expression rules for simple tokens (not part of the reserved_words)

# String of any characters inside '' or ""
t_STRING = r"('[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*')|(\"[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*\")"
t_COMMA = r'\,'
t_OPEN_PARENTHESIS = r'\('
t_CLOSE_PARENTHESIS = r'\)'
t_OPEN_BRACKET = r'\['
t_CLOSE_BRACKET = r'\]'
t_EQUALS = r'\='
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE_FLOATING_POINT = r'\/'
t_POWER_BY = r'\^'
t_EQUAL_TO = r'\=\='
t_DIFFERENT_TO = r'\<\>'
t_LESS_THAN = r'\<'
t_GREATER_THAN = r'\>'
t_LESS_OR_EQUAL_THAN = r'\<\='
t_GREATER_OR_EQUAL_THAN = r'\>\='
t_DOUBLE_POINTS = r'\:'
t_SINGLE_POINT = r'\.'

# A string containing ignored characters (spaces, tabs, and line-breaks)
t_ignore = ' \t\n'
# Comments (capture any 0 or more characters after REM or ' until line-break)
# TODO: change # for REM and ' (r'(REM|').*' does not work)
t_ignore_COMMENT = r'\#.*'


# Regular expression rules for remaining tokens

# It is very important that the FLOAT_VALUE RegEx comes 
# before the WORD_VALUE RegEx
def t_FLOAT_VALUE(t):
  r'\d+\.\d+'
  # Floating point (32 bits)
  t.value = float(t.value)
  return t

def t_WORD_VALUE(t):
	r'\d+'
  # Unsigned integer (16 bits)
	t.value = ctypes.c_uint16(int(t.value))
	return t

# All reserved_words that are not considered an 'ID' are
# tokenized (added to the t object)
def t_ID(t):
    # Any set of alphabet characters (uppercase or lowercase),
    # numbers, or '_'. However, an id can't start with a number.
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # It is only an 'ID' when it is not part of the 
    # existing reserved words.
    if t.value in reserved_words:
        t.type = reserved_words[ t.value ]
    else:  
        t.type = 'ID'
    return t

# Error handling rule
def t_error(t):
  # Show which character was invalid
	print("Invalid characters: '%s'" % t.value[0])
  # Skip token
  # Unrecognized tokens are saved as different characters
	t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()


################################################################
########################### PARSER #############################
################################################################

def p_MATHSCY(p):
	'''
	  MATHSCY  : PROGRAM ID DECLARATIONS PROGRAM_BODY END SINGLE_POINT
	'''
	print("\tCORRECTO")

def p_DECLARATIONS(p):
	'''
	  DECLARATIONS : VARIABLES_DECLARATION SUBPROCEDURES_DECLARATION
	'''
def p_VARIABLES_DECLARATION(p):
	'''
	  VARIABLES_DECLARATION : DIM IDS_SEQUENCE AS VARIABLE_TYPE VARIABLES_DECLARATION
    |
	'''
	if (len(p) > 1):
		for variableName in p[2]:
			addSymbolToTable(variableName, p[4])
def p_IDS_SEQUENCE(p):
	'''
	  IDS_SEQUENCE : ID COMMA IDS_SEQUENCE
    | ID
	'''
	# Only one ID
	if (len(p) == 2):
		p[0] = [p[1]]
	# Multiple ID's separated by commas
	else:
		# Concatenate current ID array with next IDS_SEQUENCE array (recursion)
		p[0] = [p[1]] + p[len(p)-1]
def p_VARIABLE_TYPE(p):
	'''
	  VARIABLE_TYPE : WORD
    | WORD DIMENSIONAL_VAR_DECLARATION
    | FLOAT
    | FLOAT DIMENSIONAL_VAR_DECLARATION
	'''
	# Simple type
	if(len(p) < 3):
		# VARIABLE_TYPE = WORD | FLOAT
		p[0] = p[1]
	# Dimensional type
	else:
		# TODO: define logic for dimensional types    
		print(p[2])
def p_DIMENSIONAL_VAR_DECLARATION(p):
	'''
	  DIMENSIONAL_VAR_DECLARATION : OPEN_BRACKET SIMPLE_VALUE CLOSE_BRACKET
    | OPEN_BRACKET SIMPLE_VALUE COMMA SIMPLE_VALUE CLOSE_BRACKET
    | OPEN_BRACKET SIMPLE_VALUE COMMA SIMPLE_VALUE COMMA SIMPLE_VALUE CLOSE_BRACKET
	'''
def p_SUBPROCEDURES_DECLARATION(p):
	'''
	  SUBPROCEDURES_DECLARATION : SUB PROCEDURE ID STATEMENTS RETURN SUBPROCEDURES_DECLARATION
    |
	'''

def p_PROGRAM_BODY(p):
	'''
	  PROGRAM_BODY : MAIN DOUBLE_POINTS STATEMENTS
	'''

def p_JUMPERS(p):
	'''
	  JUMPERS : GOSUB ID
    | GOTO ID
    | ID DOUBLE_POINTS
	'''

def p_VARIABLE_ASSIGNATION(p):
	'''
	  VARIABLE_ASSIGNATION : ID EQUALS ARITHMETIC_EXPRESSION
    | ID DIMENSIONAL_VAR_INDEX EQUALS ARITHMETIC_EXPRESSION
    | LET ID EQUALS ARITHMETIC_EXPRESSION
    | LET ID DIMENSIONAL_VAR_INDEX EQUALS ARITHMETIC_EXPRESSION
	'''

def p_DIMENSIONAL_VAR_INDEX(p):
	'''
	  DIMENSIONAL_VAR_INDEX : OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
    | OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
    | OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
	'''

def p_USER_INTERACTION(p):
	'''
	  USER_INTERACTION : CLS
    | ID EQUALS INPUT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS
    | LET ID EQUALS INPUT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS
    | PRINT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS
	'''
def p_STRINGS_SEQUENCE(p):
	'''
	  STRINGS_SEQUENCE : STRING COMMA STRINGS_SEQUENCE
    | STRING
    | ID COMMA STRINGS_SEQUENCE
    | ID
	'''

def p_IF_STATEMENT(p):
	'''
	  IF_STATEMENT : IF LOGICAL_EXPRESSION THEN STATEMENTS ELSE_STATEMENT END IF
	'''
def p_ELSE_STATEMENT(p):
	'''
	  ELSE_STATEMENT : ELSE STATEMENTS
    | ELSEIF LOGICAL_EXPRESSION THEN STATEMENTS ELSE_STATEMENT
    |
	'''

def p_WHILE_STATEMENT(p):
	'''
	  WHILE_STATEMENT : WHILE LOGICAL_EXPRESSION STATEMENTS WEND
	'''

def p_DO_STATEMENT(p):
	'''
	  DO_STATEMENT : DO STATEMENTS LOOP UNTIL LOGICAL_EXPRESSION
	'''

def p_FOR_STATEMENT(p):
	'''
	  FOR_STATEMENT : FOR ID EQUALS ARITHMETIC_EXPRESSION TO ARITHMETIC_EXPRESSION SET_FOR_STEPS STATEMENTS NEXT ID
	'''
def p_SET_FOR_STEPS(p):
	'''
	  SET_FOR_STEPS : OPEN_BRACKET SIMPLE_VALUE CLOSE_BRACKET
    |
	'''

def p_STATEMENTS(p):
	'''
	  STATEMENTS : JUMPERS STATEMENTS
    | VARIABLE_ASSIGNATION STATEMENTS
    | USER_INTERACTION STATEMENTS
    | IF_STATEMENT STATEMENTS
    | WHILE_STATEMENT STATEMENTS
    | FOR_STATEMENT STATEMENTS
    | DO_STATEMENT STATEMENTS
    |
	'''

# cte
def p_SIMPLE_VALUE(p):
  '''
    SIMPLE_VALUE : WORD_VALUE
    | FLOAT_VALUE
  '''
# SIMPLE_VALUE, ARITHMETIC_EXPRESSION, ID, arrays, matrices, cubes
def p_ANY_VALUE(p):
  '''
    ANY_VALUE : SIMPLE_VALUE
    | ID
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET
  '''  

# ARITHMETIC_EXPRESSION
def p_ARITHMETIC_EXPRESSION(p):
	'''
	  ARITHMETIC_EXPRESSION : ARITHMETIC_EXPRESSION_P1
    | ARITHMETIC_EXPRESSION PLUS ARITHMETIC_EXPRESSION_P1
    | ARITHMETIC_EXPRESSION MINUS ARITHMETIC_EXPRESSION_P1
	'''   
def p_ARITHMETIC_EXPRESSION_P1(p):
	'''
	  ARITHMETIC_EXPRESSION_P1 : ARITHMETIC_EXPRESSION_P2
    | ARITHMETIC_EXPRESSION_P1 MULTIPLY ARITHMETIC_EXPRESSION_P2
    | ARITHMETIC_EXPRESSION_P1 DIVIDE_FLOATING_POINT ARITHMETIC_EXPRESSION_P2
    | ARITHMETIC_EXPRESSION_P1 MOD ARITHMETIC_EXPRESSION_P2
    | ARITHMETIC_EXPRESSION_P1 DIVIDE_ROUND_DOWN ARITHMETIC_EXPRESSION_P2
	'''  
def p_ARITHMETIC_EXPRESSION_P2(p):
	'''
	  ARITHMETIC_EXPRESSION_P2 : ARITHMETIC_EXPRESSION_P3
    | ARITHMETIC_EXPRESSION_P2 POWER_BY ARITHMETIC_EXPRESSION_P3
	'''  
def p_ARITHMETIC_EXPRESSION_P3(p):
	'''
	  ARITHMETIC_EXPRESSION_P3 : OPEN_PARENTHESIS ARITHMETIC_EXPRESSION CLOSE_PARENTHESIS
    | ANY_VALUE
	'''  

# LOGICAL_EXPRESSION
def p_LOGICAL_EXPRESSION(p):
	'''
	  LOGICAL_EXPRESSION : LOGICAL_EXPRESSION_P1
    | LOGICAL_EXPRESSION OR LOGICAL_EXPRESSION_P1
	'''      
def p_LOGICAL_EXPRESSION_P1(p):
	'''
	  LOGICAL_EXPRESSION_P1 : LOGICAL_EXPRESSION_P2
    | LOGICAL_EXPRESSION_P1 AND LOGICAL_EXPRESSION_P2
	'''  
def p_LOGICAL_EXPRESSION_P2(p):
	'''
	  LOGICAL_EXPRESSION_P2 : LOGICAL_EXPRESSION_P3
    | NOT LOGICAL_EXPRESSION_P3
	'''  
def p_LOGICAL_EXPRESSION_P3(p):
	'''
	  LOGICAL_EXPRESSION_P3 : OPEN_PARENTHESIS LOGICAL_EXPRESSION CLOSE_PARENTHESIS
    | ARITHMETIC_EXPRESSION RELATIONAL_OPERATOR ARITHMETIC_EXPRESSION
    
	'''  
def p_RELATIONAL_OPERATOR(p):
	'''
	  RELATIONAL_OPERATOR : EQUAL_TO
    | DIFFERENT_TO
    | LESS_THAN
    | GREATER_THAN
    | LESS_OR_EQUAL_THAN
    | GREATER_OR_EQUAL_THAN
	'''    

# Error rule for syntax errors  
def p_error(p):
	print("\tSyntax error...", p)

# Build the parser
parser = yacc.yacc()

def addSymbolToTable(variableId, variableType):
  # Specify that variable is global to avoid conflict
  global symbolsTableIndex
  if(variableId in symbolsTable):
    raise Exception('Variable \'' + variableId + '\' already declared...')
  # Add variable_
  else:
    symbolsTable[variableId] = SymbolsTableStructure(variableId, variableType, symbolsTableIndex)
    symbolsTableIndex += 1

def printSymbolsTable():
  print("\nSymbols Table:")
  print("{:<15} {:<10} {:<6}".format('ID','TYPE','ADDRESS'))
  for symbolObject in symbolsTable.values():
    attrs = vars(symbolObject)
    print("{:<15} {:<10} {:<6}".format(attrs['id'], attrs['type'], attrs['address']))  


# Receive file name from parameter when executing program from terminal
#   - Read text from the user until EOFError (line break)
#   - Parse text afterwards
if (len(sys.argv) > 1):
  program_name = sys.argv[1]
  # Make sure file has correct extension (.mcy)
  if(re.search(r'.*\.mcy$', program_name) == None):
    raise Exception('File extension must be \'.mcy\ ...')
  program_file = open("TestingPrograms/" + program_name, "r")
  # Format file to properly read text as string
  program = program_file.read().replace('\\n', '\n')
  parser.parse(program)
  program_file.close()

  printSymbolsTable()
else:
    raise Exception('Please type the name of the test file...')