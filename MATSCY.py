
# lex = Tokenizer
# Breaks input text into a collection of specified tokens (using RegEx)

# yacc = Language syntax parser
# Recognize language syntax in the form of a context free grammar

import ply.lex as lex
import ply.yacc as yacc
import sys
import re
import numpy as np

from SymbolsTableStructure import SymbolsTableStructure
from QuadrupletStructure import QuadrupletStructure
from tokens import tokens
from reserved_words import reserved_words
from operators import relational_operators, dimensionalOperatorStringToFunctional
from program_executor import execute_program, isTypeDimensional
from quadruplets_keys import *

# Symbols table
symbolsTable = {}
symbolsTableIndex = 0
# Quadruplets
quadruplets = []
quadrupletsIndex = 0
operandsStack = []
operandsTypeStack = []
temporalVariablesIndex = 0
jumpsStack = []
ifsStack = []



################################################################
############################ LEXER #############################
################################################################

# Regular expression rules for simple tokens (not part of the reserved_words)

# String of any characters inside '' or ""
t_STRING = r"('[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,\|]*')|(\"[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,\|]*\")"
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
	t.value = int(t.value)
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
	raise Exception("Invalid characters: '%s'" % t.value[0])
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
	  MATHSCY  : PROGRAM ACTION_START_PROGRAM ID DECLARATIONS ACTION_START_MAIN PROGRAM_BODY END SINGLE_POINT ACTION_END_PROGRAM
	'''

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
			if (p[4]['dimensions'] == None):
				addSymbolToTable(variableName, p[4]['type'])
			else:				
				addSymbolToTable(variableName, p[4]['type'], dimensions=p[4]['dimensions'])

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
		p[0] = { 'type': p[1], 'dimensions': None }
	# Dimensional type
	else:
		p[2]['type'] = p[1] + '_' + p[2]['type'] 
		p[0] = p[2]

def p_DIMENSIONAL_VAR_DECLARATION(p):
	'''
	  DIMENSIONAL_VAR_DECLARATION : OPEN_BRACKET SIMPLE_VALUE CLOSE_BRACKET
    | OPEN_BRACKET SIMPLE_VALUE COMMA SIMPLE_VALUE CLOSE_BRACKET
    | OPEN_BRACKET SIMPLE_VALUE COMMA SIMPLE_VALUE COMMA SIMPLE_VALUE CLOSE_BRACKET
	'''
	if(len(p) == 4):
		p[0] = {
      'type': 'ARRAY',
		  'dimensions': [p[2]],
    }
	elif(len(p) == 6):
		p[0] = {
      'type': 'MATRIX',
		  'dimensions': [p[2], p[4]],
    }
	elif(len(p) == 8):
		p[0] = {
      'type': 'CUBE',
		  'dimensions': [p[2], p[4], p[6]],
    }

def p_SUBPROCEDURES_DECLARATION(p):
	'''
	  SUBPROCEDURES_DECLARATION : SUB PROCEDURE ID ACTION_ADD_SUB_PROCEDURE DOUBLE_POINTS STATEMENTS RETURN ACTION_END_SUB_PROCEDURE SUBPROCEDURES_DECLARATION
    |
	'''

def p_PROGRAM_BODY(p):
	'''
	  PROGRAM_BODY : MAIN DOUBLE_POINTS STATEMENTS
	'''

def p_JUMPERS(p):
	'''
	  JUMPERS : GOSUB ID ACTION_CALL_SUB_PROCEDURE
	'''

# TODO: dimensional
def p_VARIABLE_ASSIGNATION(p):
	'''
	  VARIABLE_ASSIGNATION : ID EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_ARRAY
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_MATRIX
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_CUBE
    | LET ID EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE
    | LET ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_ARRAY
    | LET ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_MATRIX
    | LET ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE_CUBE
	'''

def p_CONSOLE(p):
	'''
	  CONSOLE : CLS ACTION_CONSOLE_CLEAR
    | ID EQUALS INPUT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS ACTION_CONSOLE_INPUT
    | LET ID EQUALS INPUT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS ACTION_CONSOLE_INPUT
    | PRINT OPEN_PARENTHESIS STRINGS_SEQUENCE CLOSE_PARENTHESIS ACTION_CONSOLE_OUTPUT
	'''

# TODO: logic for ANY_VAUE
def p_STRINGS_SEQUENCE(p):
	'''
	  STRINGS_SEQUENCE : STRING COMMA STRINGS_SEQUENCE
    | STRING
    | ANY_VALUE COMMA STRINGS_SEQUENCE
    | ANY_VALUE
	'''
  # NOTE: STRINGS_SEQUENCE will always be an array of ids and/or strings
	# Only one string/id
	if (len(p) == 2):
		p[0] = [p[1]]
	# Multiple strings/ids separated by commas
	else:
		# Concatenate current ID array with next STRINGS_SEQUENCE array (recursion)
		p[0] = [p[1]] + p[len(p)-1]

def p_IF_STATEMENT(p):
	'''
	  IF_STATEMENT : IF LOGICAL_EXPRESSION THEN ACTION_NEW_IF ACTION_QUADRUPLET_EMPTY_GOTOF STATEMENTS ELSE_STATEMENT END IF ACTION_FILL_GOTO
	'''

def p_ELSE_STATEMENT(p):
	'''
	  ELSE_STATEMENT : ELSE ACTION_QUADRUPLET_EMPTY_GOTO ACTION_FILL_GOTOF STATEMENTS
    | ELSEIF ACTION_QUADRUPLET_EMPTY_GOTO ACTION_FILL_GOTOF LOGICAL_EXPRESSION THEN ACTION_QUADRUPLET_EMPTY_GOTOF STATEMENTS ACTION_QUADRUPLET_EMPTY_GOTO ELSE_STATEMENT
    | ACTION_FILL_GOTOF
	'''

def p_WHILE_STATEMENT(p):
	'''
	  WHILE_STATEMENT : WHILE ACTION_LOOP_START LOGICAL_EXPRESSION ACTION_QUADRUPLET_EMPTY_GOTOF STATEMENTS WEND ACTION_WHILE_GOTO
	'''

def p_DO_STATEMENT(p):
	'''
	  DO_STATEMENT : DO ACTION_LOOP_START STATEMENTS LOOP UNTIL LOGICAL_EXPRESSION ACTION_QUADRUPLET_EMPTY_GOTOF_DO_WHILE
	'''

def p_FOR_STATEMENT(p):
	'''
	  FOR_STATEMENT : FOR ID EQUALS ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_STORE ACTION_FOR_COUNTER_VALUE TO ARITHMETIC_EXPRESSION ACTION_QUADRUPLET_FOR_CONDITION ACTION_QUADRUPLET_EMPTY_GOTOF STATEMENTS NEXT ID ACTION_FOR_INCREMENT ACTION_WHILE_GOTO
	'''

def p_STATEMENTS(p):
	'''
	  STATEMENTS : JUMPERS STATEMENTS
    | VARIABLE_ASSIGNATION STATEMENTS
    | CONSOLE STATEMENTS
    | IF_STATEMENT STATEMENTS
    | WHILE_STATEMENT STATEMENTS
    | FOR_STATEMENT STATEMENTS
    | DO_STATEMENT STATEMENTS
    |
	'''

# cte
def p_SIMPLE_VALUE(p):
  '''
    SIMPLE_VALUE : WORD_VALUE ACTION_WORD_VALUE
    | FLOAT_VALUE ACTION_FLOAT_VALUE
  '''
  p[0] = p[1]
  
def p_ANY_VALUE(p):
  '''
    ANY_VALUE : SIMPLE_VALUE
    | ID ACTION_VARIABLE_VALUE
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET ACTION_VARIABLE_ARRAY_VALUE
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET ACTION_VARIABLE_MATRIX_VALUE
    | ID OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET OPEN_BRACKET ARITHMETIC_EXPRESSION CLOSE_BRACKET ACTION_VARIABLE_CUBE_VALUE
  '''
  if (len(p) <= 3):
    p[0] = p[1]
  elif (len(p) == 6):
    p[0] = {
      'type': 'ARRAY',
  	  'id': p[1],
    }
  elif(len(p) == 9):
    p[0] = {
      'type': 'MATRIX',
  	  'id': p[1],
    }
  elif(len(p) == 11):
    p[0] = {
      'type': 'CUBE',
  	  'id': p[1],
    }


# ARITHMETIC_EXPRESSION
def p_ARITHMETIC_EXPRESSION(p):
	'''
	  ARITHMETIC_EXPRESSION : ARITHMETIC_EXPRESSION_P1
    | ARITHMETIC_EXPRESSION PLUS ARITHMETIC_EXPRESSION_P1 ACTION_GENERATE_QUADRUPLET
    | ARITHMETIC_EXPRESSION MINUS ARITHMETIC_EXPRESSION_P1 ACTION_GENERATE_QUADRUPLET
	'''   
def p_ARITHMETIC_EXPRESSION_P1(p):
	'''
	  ARITHMETIC_EXPRESSION_P1 : ARITHMETIC_EXPRESSION_P2
    | ARITHMETIC_EXPRESSION_P1 MULTIPLY ARITHMETIC_EXPRESSION_P2 ACTION_GENERATE_QUADRUPLET
    | ARITHMETIC_EXPRESSION_P1 DIVIDE_FLOATING_POINT ARITHMETIC_EXPRESSION_P2 ACTION_GENERATE_QUADRUPLET
    | ARITHMETIC_EXPRESSION_P1 MOD ARITHMETIC_EXPRESSION_P2 ACTION_GENERATE_QUADRUPLET
    | ARITHMETIC_EXPRESSION_P1 DIV ARITHMETIC_EXPRESSION_P2 ACTION_GENERATE_QUADRUPLET
	'''  
def p_ARITHMETIC_EXPRESSION_P2(p):
	'''
	  ARITHMETIC_EXPRESSION_P2 : ARITHMETIC_EXPRESSION_P3
    | ARITHMETIC_EXPRESSION_P2 POWER_BY ARITHMETIC_EXPRESSION_P3 ACTION_GENERATE_QUADRUPLET
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
    | LOGICAL_EXPRESSION OR LOGICAL_EXPRESSION_P1 ACTION_GENERATE_QUADRUPLET
	'''      
def p_LOGICAL_EXPRESSION_P1(p):
	'''
	  LOGICAL_EXPRESSION_P1 : LOGICAL_EXPRESSION_P2
    | LOGICAL_EXPRESSION_P1 AND LOGICAL_EXPRESSION_P2 ACTION_GENERATE_QUADRUPLET
	'''  
def p_LOGICAL_EXPRESSION_P2(p):
	'''
	  LOGICAL_EXPRESSION_P2 : LOGICAL_EXPRESSION_P3
    | NOT LOGICAL_EXPRESSION_P3 ACTION_GENERATE_QUADRUPLET
	'''  
def p_LOGICAL_EXPRESSION_P3(p):
	'''
	  LOGICAL_EXPRESSION_P3 : OPEN_PARENTHESIS LOGICAL_EXPRESSION CLOSE_PARENTHESIS
    | ARITHMETIC_EXPRESSION RELATIONAL_OPERATOR ARITHMETIC_EXPRESSION ACTION_GENERATE_QUADRUPLET_LOGICAL
    
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
	p[0] = p[1]

# Error rule for syntax errors  
def p_error(p):
	raise Exception("\tSyntax error...", p)

########################################
######### ACTIONS EXPRESSIONS ##########
########################################
# NOTE: P array indexes are negative because action is placed at the end of production

def p_ACTION_WORD_VALUE(p):
  '''
    ACTION_WORD_VALUE :
  '''   
  operandsStack.append(p[-1])
  operandsTypeStack.append('WORD')

def p_ACTION_FLOAT_VALUE(p):
  '''
    ACTION_FLOAT_VALUE :
  '''   
  operandsStack.append(p[-1])
  operandsTypeStack.append('FLOAT')

def p_ACTION_VARIABLE_VALUE(p):
  '''
    ACTION_VARIABLE_VALUE :
  '''   
  operandsStack.append(symbolsTable[p[-1]].id)
  operandsTypeStack.append(symbolsTable[p[-1]].type)

def p_ACTION_VARIABLE_ARRAY_VALUE(p):
  '''
    ACTION_VARIABLE_ARRAY_VALUE :
  '''   
  arrayId = p[-4]
  arrayHash = buildDimensionalHashByType('ARRAY', arrayId)
  operandsStack.append(arrayHash)
  arrayType = 'WORD' if ('WORD' in symbolsTable[arrayId].type) else 'FLOAT'
  operandsTypeStack.append(arrayType)

def p_ACTION_VARIABLE_MATRIX_VALUE(p):
  '''
    ACTION_VARIABLE_MATRIX_VALUE :
  '''   
  matrixId = p[-7]
  matrixHash = buildDimensionalHashByType('MATRIX', matrixId)
  operandsStack.append(matrixHash)
  matrixType = 'WORD' if ('WORD' in symbolsTable[matrixId].type) else 'FLOAT'
  operandsTypeStack.append(matrixType)

def p_ACTION_VARIABLE_CUBE_VALUE(p):
  '''
    ACTION_VARIABLE_CUBE_VALUE :
  '''   
  cubeId = p[-10]
  cubeHash = buildDimensionalHashByType('CUBE', cubeId)
  operandsStack.append(cubeHash)
  cubeType = 'WORD' if ('WORD' in symbolsTable[cubeId].type) else 'FLOAT'
  operandsTypeStack.append(cubeType)


def p_ACTION_GENERATE_QUADRUPLET_STORE(p):
  '''
    ACTION_GENERATE_QUADRUPLET_STORE :
  '''
  operandsStack.append(symbolsTable[p[-3]].id)
  operandsTypeStack.append(symbolsTable[p[-3]].type)
  generateQuadruplet("=")

def p_ACTION_GENERATE_QUADRUPLET_STORE_ARRAY(p):
  '''
    ACTION_GENERATE_QUADRUPLET_STORE_ARRAY :
  '''
  global quadrupletsIndex  
  # Get (operandLeft)
  operandLeft = operandsStack.pop()
  operandTypeLeft = operandsTypeStack.pop()
  # Get (operandRight)
  arrayId = p[-6]
  arrayElementHash = buildDimensionalHashByType('ARRAY', arrayId)
  operandRight = arrayElementHash
  operandTypeRight = symbolsTable[arrayId].type
  # Create quadruplet
  if not(operandTypeLeft in operandTypeRight):
    raise Exception('Attempted to make an operation with variables of different type...')
  quadruplets.append(QuadrupletStructure(EQUAL_QUAD, operandLeft, operandRight, None))
  quadrupletsIndex += 1

def p_ACTION_GENERATE_QUADRUPLET_STORE_MATRIX(p):
  '''
    ACTION_GENERATE_QUADRUPLET_STORE_MATRIX :
  '''
  global quadrupletsIndex  
  # Get (operandLeft)
  operandLeft = operandsStack.pop()
  operandTypeLeft = operandsTypeStack.pop()
  # Get (operandRight)  
  matrixId = p[-9]
  matrixElementHash = buildDimensionalHashByType('MATRIX', matrixId)
  operandRight = matrixElementHash
  operandTypeRight = symbolsTable[matrixId].type
  # Create quadruplet
  if not(operandTypeLeft in operandTypeRight):
    raise Exception('Attempted to make an operation with variables of different type...')
  quadruplets.append(QuadrupletStructure(EQUAL_QUAD, operandLeft, operandRight, None))
  quadrupletsIndex += 1


def p_ACTION_GENERATE_QUADRUPLET_STORE_CUBE(p):
  '''
    ACTION_GENERATE_QUADRUPLET_STORE_CUBE :
  '''
  global quadrupletsIndex  
  # Get (operandLeft)
  operandLeft = operandsStack.pop()
  operandTypeLeft = operandsTypeStack.pop()
  # Get (operandRight)  
  cubeId = p[-12]
  cubeElementHash = buildDimensionalHashByType('CUBE', cubeId)
  operandRight = cubeElementHash
  operandTypeRight = symbolsTable[cubeId].type
  # Create quadruplet
  if not(operandTypeLeft in operandTypeRight):
    raise Exception('Attempted to make an operation with variables of different type...')
  quadruplets.append(QuadrupletStructure(EQUAL_QUAD, operandLeft, operandRight, None))
  quadrupletsIndex += 1  

def p_ACTION_GENERATE_QUADRUPLET(p):
  '''
    ACTION_GENERATE_QUADRUPLET :
  '''
  operator = p[-2]
  if (operator == 'NOT'):
    generateQuadruplet__Not()
  else:
    generateQuadruplet(operator)

def p_ACTION_GENERATE_QUADRUPLET_LOGICAL(p):
  '''
    ACTION_GENERATE_QUADRUPLET_LOGICAL :
  '''
  operator = p[-2]
  generateQuadruplet(operator)


def generateQuadruplet(operator):
  global temporalVariablesIndex
  global quadrupletsIndex  
  # Get operands data
  operandRight = operandsStack.pop()
  operandLeft = operandsStack.pop()
  operandTypeRight = operandsTypeStack.pop()
  operandTypeLeft = operandsTypeStack.pop()
  if (operandTypeRight != operandTypeLeft):
    raise Exception('Attempted to make an operation with variables of different type...')
  if (operator == EQUAL_QUAD):
    # Generate STORE quadruplet
    quadruplets.append(QuadrupletStructure(EQUAL_QUAD, operandLeft, operandRight, None))
    quadrupletsIndex += 1
  else:
    # If the operator is a relational-operator, then assign a 'BOOLEAN'
    # type to the result.
    resultType = 'BOOLEAN' if operator in relational_operators else operandTypeLeft
    isDimensional = isTypeDimensional(operandTypeLeft)
    # Save space for temporal variable in symbols table
    # NOTE: The character '#' at the beginning ensures that no
    # variableId will be named equal to this one.
    # NOTE: the resulting temporal variable has the same type as the operators.
    tempName = '#temp_' + str(temporalVariablesIndex)
    if (isDimensional):
      dimensionalResult = dimensionalOperatorStringToFunctional[operator](symbolsTable[operandLeft].value, symbolsTable[operandRight].value)
      dimensionsList = list(dimensionalResult.shape)
      addSymbolToTable(tempName, resultType, dimensions=dimensionsList)
    else:
      addSymbolToTable(tempName, resultType)    
    temporalVariablesIndex += 1
    # Add temp data to stacks
    operandsStack.append(tempName)
    operandsTypeStack.append(resultType)
    # Generate quadruplet
    quadruplets.append(QuadrupletStructure(operator, operandLeft, operandRight, tempName))
    quadrupletsIndex += 1

# A separate function for the NOT operator is needed because 
# it is the only expression that handles one operand only.
def generateQuadruplet__Not():
  global temporalVariablesIndex
  global quadrupletsIndex
  operand = operandsStack.pop()
  operandType = operandsTypeStack.pop()
  tempName = '#temp_' + str(temporalVariablesIndex)
  addSymbolToTable(tempName, operandType)
  temporalVariablesIndex += 1
  # add temp data to stacks
  operandsStack.append(tempName)
  operandsTypeStack.append(operandType)
  quadruplets.append(QuadrupletStructure('NOT', operand, None, tempName))
  quadrupletsIndex += 1


########################################
######## ACTIONS IF STATEMENTS #########
########################################
def p_ACTION_NEW_IF(p):
  '''
    ACTION_NEW_IF :
  '''
  # Append an empty list, which will be used to keep
  # track of nested IFs within evaluated IF-STATEMENT.
  ifsStack.append([])

def p_ACTION_QUADRUPLET_EMPTY_GOTOF(p):
  '''
    ACTION_QUADRUPLET_EMPTY_GOTOF :
  '''
  global quadrupletsIndex
  # Logical expression quadruplet was the last quadruplet before GOTOF quadruplet
  logicalExpressionResultId = quadruplets[-1].result
  # Verify that the type of the last quadruplet result is indeed a BOOLEAN
  # NOTE: (result) corresponds to the ID of the temporal variable
  #  where the result of the expression was stored.
  if (symbolsTable[logicalExpressionResultId].type != 'BOOLEAN'):
    raise Exception('Expression result of an IF-STATEMENT was not a BOOLEAN type...')
  # GOTOF quadruplet is created without specifying jumping address (operandRight)
  quadruplets.append(QuadrupletStructure(GOTOF_QUAD, logicalExpressionResultId, None, None))
  # Add current quadruplet-index to (jumpsStack) to later fill the missing jumping-address
  jumpsStack.append(quadrupletsIndex)
  quadrupletsIndex += 1

def p_ACTION_FILL_GOTOF(p):
  '''
    ACTION_FILL_GOTOF :
  '''  
  fillJump(jumpsStack.pop(), quadrupletsIndex)    

def p_ACTION_QUADRUPLET_EMPTY_GOTO(p):
  '''
    ACTION_QUADRUPLET_EMPTY_GOTO :
  '''
  global quadrupletsIndex
  # Append quadruplet-index to remember to fill the following 
  # GOTO jumping address after the IF-STATEMENT finishes.
  ifsStack[-1].append(quadrupletsIndex)
  quadruplets.append(QuadrupletStructure(GOTO_QUAD, None, None, None))
  quadrupletsIndex += 1

def p_ACTION_FILL_GOTO(p):
  '''
    ACTION_FILL_GOTO :
  '''
  # NOTE: If there was no ELSE_STATEMENT, then there will be 
  # no need of filling any GOTO (ifsStack[-1] == [])
  for gotofQuadrupletIndex in ifsStack[-1]:
      fillJump(gotofQuadrupletIndex, quadrupletsIndex)
  ifsStack.pop()

def fillJump(pendingGotoQuadrupletIndex, jumpAddress):
  # Get instance of quadruplet by list index
  pendingQuadruplet = quadruplets[pendingGotoQuadrupletIndex]
  # (jumpAddress) of both (GOTOF) and (GOTO) operators is
  # stored in the (operandRight) field.
  pendingQuadruplet.operandRight = jumpAddress


########################################
####### ACTIONS LOOP STATEMENTS ########
########################################  
def p_ACTION_LOOP_START(p):
  '''
    ACTION_LOOP_START :
  '''  
  global quadrupletsIndex
  jumpsStack.append(quadrupletsIndex)

def p_ACTION_WHILE_GOTO(p):
  '''
    ACTION_WHILE_GOTO :
  '''  
  global quadrupletsIndex
  pendingGotoQuadrupletIndex = jumpsStack.pop()
  loopStartQuadrupletIndex = jumpsStack.pop()
  quadruplets.append(QuadrupletStructure(GOTO_QUAD, None, loopStartQuadrupletIndex, None))
  quadrupletsIndex += 1
  fillJump(pendingGotoQuadrupletIndex, quadrupletsIndex) 

def p_ACTION_QUADRUPLET_EMPTY_GOTOF_DO_WHILE(p):
  '''
    ACTION_QUADRUPLET_EMPTY_GOTOF_DO_WHILE :
  '''  
  global quadrupletsIndex
  # Logical expression quadruplet was the last quadruplet before GOTOF quadruplet
  logicalExpressionResultId = quadruplets[-1].result
  # Verify that the type of the last quadruplet result is indeed a BOOLEAN
  # NOTE: (result) corresponds to the ID of the temporal variable
  #  where the result of the expression was stored.
  if (symbolsTable[logicalExpressionResultId].type != 'BOOLEAN'):
    raise Exception('Expression result of an DO-ULTIL-STATEMENT was not a BOOLEAN type...')
  # GOTOF quadruplet is created without specifying jumping address (operandRight)
  loopStartQuadrupletIndex = jumpsStack.pop()
  quadruplets.append(QuadrupletStructure(GOTOF_QUAD, logicalExpressionResultId, loopStartQuadrupletIndex, None))
  # Add current quadruplet-index to (jumpsStack) to later fill the missing jumping-address
  jumpsStack.append(quadrupletsIndex)
  quadrupletsIndex += 1  

def p_ACTION_FOR_COUNTER_VALUE(p):
  '''
    ACTION_FOR_COUNTER_VALUE :
  '''   
  # Add counter-variable id and type to corresponding stacks
  # NOTE: it is important to place this action at this position (before the
  # arithmetic-expression) to conserve the order of the for '<=' condition.
  operandsStack.append(symbolsTable[p[-4]].id)
  operandsTypeStack.append(symbolsTable[p[-4]].type)

def p_ACTION_QUADRUPLET_FOR_CONDITION(p):
  '''
    ACTION_QUADRUPLET_FOR_CONDITION :
  '''
  global quadrupletsIndex
  # Make sure to append the index of the '<=' quadruplet so that
  # the loop jumps back at this point to verify when to stop looping.
  # NOTE: the current (quadrupletsIndex) corresponds to the '<=' quadruplet
  jumpsStack.append(quadrupletsIndex)  
  # Generate '<=' quadruplet
  generateQuadruplet("<=")

def p_ACTION_FOR_INCREMENT(p):
  '''
    ACTION_FOR_INCREMENT :
  '''
  counterId = p[-1]
  # Generate quadruplet to increment counter-variable by 1
  # NOTE: the value 1 must have the type of the counter-variable
  operandsStack.append(symbolsTable[counterId].id)
  operandsTypeStack.append(symbolsTable[counterId].type)
  if(symbolsTable[counterId].type == 'WORD'):
    operandsStack.append(int(1))
    operandsTypeStack.append('WORD')
  else:
    operandsStack.append(float(1))
    operandsTypeStack.append('FLOAT')    
  generateQuadruplet("+")
  # Generate quadruplet to store incremented counter to itself
  operandsStack.append(symbolsTable[counterId].id)
  operandsTypeStack.append(symbolsTable[counterId].type)  
  generateQuadruplet("=")


########################################
####### ACTIONS SUB_PROCEDURES #########
########################################  
def p_ACTION_ADD_SUB_PROCEDURE(p):
  '''
    ACTION_ADD_SUB_PROCEDURE :
  '''  
  global quadrupletsIndex
  subProcedureId = p[-1]
  # Add subProcedureId to symbols table
  addSymbolToTable(subProcedureId, 'SUB_PROCEDURE', functionIndex=quadrupletsIndex)

def p_ACTION_END_SUB_PROCEDURE(p):
  '''
    ACTION_END_SUB_PROCEDURE :
  '''  
  global quadrupletsIndex
  # Create RETURN quadruplet
  quadruplets.append(QuadrupletStructure(END_SUB_PROCEDURE_QUAD, None, None, None))
  quadrupletsIndex += 1

def p_ACTION_CALL_SUB_PROCEDURE(p):
  '''
    ACTION_CALL_SUB_PROCEDURE :
  '''  
  global quadrupletsIndex
  subProcedureId = p[-1]
  quadruplets.append(QuadrupletStructure(CALL_SUB_PROCEDURE_QUAD, symbolsTable[subProcedureId].functionIndex, None, None))
  quadrupletsIndex += 1


########################################
######## ACTIONS MAIN_PROGRAM ##########
########################################  
def p_ACTION_START_PROGRAM(p):
  '''
    ACTION_START_PROGRAM :
  '''  
  global quadrupletsIndex
  # Create GOTO quadruplet, which will point to start of MAIN program
  quadruplets.append(QuadrupletStructure(GOTO_QUAD, None, None, None))
  quadrupletsIndex += 1

def p_ACTION_START_MAIN(p):
  '''
    ACTION_START_MAIN :
  '''  
  # Fill GOTO that points to start of MAIN program
  fillJump(0, quadrupletsIndex)

def p_ACTION_END_PROGRAM(p):
  '''
    ACTION_END_PROGRAM :
  '''  
  global quadrupletsIndex
  quadruplets.append(QuadrupletStructure(END_PROGRAM_QUAD, None, None, None))
  quadrupletsIndex += 1


########################################
########### ACTIONS CONSOLE ############
########################################  
def p_ACTION_CONSOLE_CLEAR(p):
  '''
    ACTION_CONSOLE_CLEAR :
  '''  
  global quadrupletsIndex
  quadruplets.append(QuadrupletStructure(CLS_QUAD, None, None, None))
  quadrupletsIndex += 1  

def p_ACTION_CONSOLE_INPUT(p):
  '''
    ACTION_CONSOLE_INPUT :
  '''  
  global quadrupletsIndex
  inputId = p[-6]
  inputStrings = p[-2]
  # Hash dimensional variables
  operandsStackVisitedElements = 1
  for i in range (len(inputStrings)):
    if isinstance(inputStrings[i], dict):
      inputStrings[i] = operandsStack[-operandsStackVisitedElements]
      operandsStackVisitedElements = operandsStackVisitedElements + 1
  # Build Quadruplet  
  quadruplets.append(QuadrupletStructure(INPUT_QUAD, inputStrings, inputId, None))
  quadrupletsIndex += 1  

def p_ACTION_CONSOLE_OUTPUT(p):
  '''
    ACTION_CONSOLE_OUTPUT :
  '''  
  global quadrupletsIndex
  outputStrings = p[-2]
  # Hash dimensional variables
  operandsStackVisitedElements = 1
  for i in range (len(outputStrings)):
    if isinstance(outputStrings[i], dict):
      outputStrings[i] = operandsStack[-operandsStackVisitedElements]
      operandsStackVisitedElements = operandsStackVisitedElements + 1
  # Build Quadruplet
  quadruplets.append(QuadrupletStructure(OUTPUT_QUAD, outputStrings, None, None))
  quadrupletsIndex += 1  

def buildDimensionalHashByType(type, id):
  if (type == 'ARRAY'):
    # Get indexes
    index_1 = operandsStack.pop()
    index_1_Type = operandsTypeStack.pop()
    if (index_1_Type!='WORD'):
      raise Exception('Index of dimensional variable must be type WORD...')  
    # Build arrayHash
    return '@'+id+'@'+str(index_1)    
  elif (type == 'MATRIX'):
    # Get indexes
    index_2 = operandsStack.pop()
    index_2_Type = operandsTypeStack.pop()
    index_1 = operandsStack.pop()
    index_1_Type = operandsTypeStack.pop()
    if (index_2_Type!='WORD' or index_1_Type!='WORD'):
      raise Exception('Index of dimensional variable must be type WORD...')
    # Build matrixHash  
    return '@'+id+'@'+str(index_1)+'@'+str(index_2)  
  elif (type == 'CUBE'):
    # Get indexes
    index_3 = operandsStack.pop()
    index_3_Type = operandsTypeStack.pop()
    index_2 = operandsStack.pop()
    index_2_Type = operandsTypeStack.pop()
    index_1 = operandsStack.pop()
    index_1_Type = operandsTypeStack.pop()
    if (index_3_Type!='WORD' or index_2_Type!='WORD' or index_1_Type!='WORD'):
      raise Exception('Index of dimensional variable must be type WORD...')
    # Build cubeHash 
    return '@'+id+'@'+str(index_1)+'@'+str(index_2)+'@'+str(index_3)  

########################################
########################################
########################################  

# Build the parser
parser = yacc.yacc()

def addSymbolToTable(variableId, variableType, functionIndex=None, dimensions=None):
  # Specify that variable is global to avoid conflict
  global symbolsTableIndex
  if(variableId in symbolsTable):
    raise Exception('Variable \'' + variableId + '\' already declared...')
  # Get dimensions (if exists)
  if(isinstance(dimensions, list)):
    dimension1 = None if len(dimensions) < 1 else dimensions[0]
    dimension2 = None if len(dimensions) < 2 else dimensions[1]
    dimension3 = None if len(dimensions) < 3 else dimensions[2]
    symbolsTable[variableId] = SymbolsTableStructure(variableId, variableType, symbolsTableIndex, functionIndex, dimension1, dimension2, dimension3)
  # Fill table with new variable information
  else:
    symbolsTable[variableId] = SymbolsTableStructure(variableId, variableType, symbolsTableIndex, functionIndex, None, None, None)
    symbolsTableIndex += 1

def printSymbolsTable():
  print("\nSymbols Table:")
  print("{:<20} {:<15} {:<10} {:<15} {:<10}".format('ID','TYPE','ADDRESS','FUNCTION_INDEX', 'VALUE'))
  for symbolObject in symbolsTable.values():
    attrs = vars(symbolObject)
    isDimensional = isTypeDimensional(attrs['type'])
    value = str(attrs['value'].tolist()) if (isDimensional) else (attrs['value'])
    print("{:<20} {:<15} {:<10} {:<15} {:<10}".format(str(attrs['id']), str(attrs['type']), str(attrs['address']), str(attrs['functionIndex']), str(value)))
  print("\n")

def printQuadruplets():
  print("\nQuadruplets:")
  print("{:<7} {:<20} {:<15} {:<15} {:<5}".format('INDEX','KEY','OPERAND_LEFT','OPERAND_RIGHT','RESULT'))  
  for (index, quadObject) in enumerate(quadruplets):
    attrs = vars(quadObject)
    operandLeft = 'Strings Array' if attrs['key']=='OUTPUT' or attrs['key']=='INPUT' else str(attrs['operandLeft'])
    print("{:<7} {:<20} {:<15} {:<15} {:<5}".format(str(index), str(attrs['key']), operandLeft, str(attrs['operandRight']), str(attrs['result'])))  
  print("\n")    


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

  execute_program(quadruplets, symbolsTable)
  printSymbolsTable()
  printQuadruplets()
else:
    raise Exception('Please type the name of the test file...')