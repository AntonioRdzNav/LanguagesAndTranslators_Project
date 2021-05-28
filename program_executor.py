

# ast.literal_eval is used to efectively parse a string to a float or integer
# NOTE: this is only safe because we are doing a regex check during the parsing
# steps to verify that the values are only integers and floats.
import ast
from os import system, name
import re
import math

from operators import relational_operators, arithmetic_operators, logical_operators
from quadruplets_keys import *


regexString = r"('[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,\|]*')|(\"[a-zA-Z0-9 \\\.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,\|]*\")"

functionsStack = []

endOfProgramPC = None

def isTypeDimensional(type):
  return ("ARRAY" in type) or ("MATRIX" in type) or ("CUBE" in type)  

def execute_program(quadruplets, symbolsTable):
  global endOfProgramPC
  program_counter = 0
  endOfProgramPC = len(quadruplets)
  print("||||||||||||||||||||||||| STARTING PROGRAM |||||||||||||||||||||||||", end="\n\n")
  while program_counter < endOfProgramPC:
    program_counter = executeQuadruplet(quadruplets[program_counter], program_counter, symbolsTable)
  print("\n||||||||||||||||||||||||||| ENDED PROGRAM ||||||||||||||||||||||||||", end="\n\n")


def printConcatenationToConsole(concatenation, symbolsTable):
  # Get array of strings and variables to print.
  for data in concatenation:
    # Print string
    if re.match(regexString, data):
      stringText = re.sub("\"|\'", "", data)
      print(stringText, end='')
    # Print an element of a dimensional variable
    elif ('@' in data):      
      decodedDimensionalElem = accessDimensionalElement(data, symbolsTable)
      print(decodedDimensionalElem, end='')
    # Print a dimensional variable
    elif (data in symbolsTable) and isTypeDimensional(symbolsTable[data].type):
      print(symbolsTable[data].value.tolist(), end='')
    # Print a variable or numeric constant
    else:
      print(getOperandValue(data, symbolsTable), end='')
  # End of line
  print()

# Get value of (operand) from symbolsTable if it's a variable, or return it if it's numeric
def getOperandValue(operand, symbolsTable):
  if operand in symbolsTable:
    return symbolsTable[operand].value
  else:
    return operand  

# Get an specific dimensional element from a raw dimensional hash
def accessDimensionalElement(rawDimensionalHash, symbolsTable, assignValue=None):
  # Get hash with id and indexes (as an array)
  dimensionalHash = rawDimensionalHash.split('@')[1:]
  # Handle array asignation
  if (len(dimensionalHash) == 2):
    index = getOperandValue(dimensionalHash[1], symbolsTable)
    if (assignValue != None):
      symbolsTable[dimensionalHash[0]].value[index] = assignValue
    return symbolsTable[dimensionalHash[0]].value[index]
  # Handle matrix asignation
  elif (len(dimensionalHash) == 3):
    index1 = getOperandValue(dimensionalHash[1], symbolsTable)
    index2 = getOperandValue(dimensionalHash[2], symbolsTable)
    if (assignValue != None):
      symbolsTable[dimensionalHash[0]].value[index1, index2] = assignValue
    return symbolsTable[dimensionalHash[0]].value[index1, index2]
  # Handle array asignation
  elif (len(dimensionalHash) == 4):
    index1 = getOperandValue(dimensionalHash[1], symbolsTable)
    index2 = getOperandValue(dimensionalHash[2], symbolsTable)
    index3 = getOperandValue(dimensionalHash[3], symbolsTable)
    if (assignValue != None):
      symbolsTable[dimensionalHash[0]].value[index1, index2, index3] = assignValue
    return symbolsTable[dimensionalHash[0]].value[index1, index2, index3]  
  else:
    return None

def executeQuadruplet(quadruplet, program_counter, symbolsTable):

  if quadruplet.key == CLS_QUAD:
    # For windows
    system('cls')
    # For mac and linux(here, os.name is 'posix')
    system('clear')

  if quadruplet.key == OUTPUT_QUAD:
    printConcatenationToConsole(quadruplet.operandLeft, symbolsTable)

  if quadruplet.key == INPUT_QUAD:
    # Print INPUT message/instructions to console.
    printConcatenationToConsole(quadruplet.operandLeft, symbolsTable)
    # Assign users given input to specified variable,
    # but first make sure such variable was declared.
    variableId = quadruplet.operandRight
    if variableId in symbolsTable:
      # Parse string to a number (either integer or float)
      inputValue = ast.literal_eval(input())
      # Verify input is a numeric value
      if (isinstance(inputValue, float) or isinstance(inputValue, int)):
        if symbolsTable[variableId].type=='FLOAT':
          # Input matches a FLOAT type and variable its a FLOAT type
          symbolsTable[variableId].value = float(inputValue)
        elif symbolsTable[variableId].type=='WORD':
          # Input matches a WORD type and variable its a WORD type
          symbolsTable[variableId].value =  int(inputValue)
      else:
        raise Exception (f'Console Input Error: attempted to assign a non-numeric value to {variableId}')       
    else:
      raise Exception (f'Console Input Error: variable {variableId} is not defined')    


  elif quadruplet.key in (relational_operators + arithmetic_operators + logical_operators):
    symbolsTable[quadruplet.result].value = solveOperation(quadruplet.key, quadruplet.operandLeft, quadruplet.operandRight, symbolsTable)

  elif quadruplet.key == EQUAL_QUAD:
    storeVarId = quadruplet.operandRight
    valueToStore = quadruplet.operandLeft
    # Will store in a dimensional variable (dimensional hashes include '@')
    if '@' in storeVarId:
      # (valueToStore) is a dimensional element (given as a hash)
      if (isinstance(valueToStore, str)) and ('@' in valueToStore):
        cleanValueToStore = accessDimensionalElement(valueToStore, symbolsTable)
      else:
        cleanValueToStore = getOperandValue(valueToStore, symbolsTable)      
      accessDimensionalElement(storeVarId, symbolsTable, assignValue=cleanValueToStore)
    # Will store in a 'simple' variable
    else:
      symbolsTable[storeVarId].value = getOperandValue(valueToStore, symbolsTable)

  elif quadruplet.key == GOTOF_QUAD:
    conditionResult = symbolsTable[quadruplet.operandLeft].value
    # Jump to specified (program_counter) when condition is falsy
    if not conditionResult:
      return quadruplet.operandRight

  elif quadruplet.key == GOTO_QUAD:
    # Jump to specified (program_counter)
    return quadruplet.operandRight

  elif quadruplet.key == CALL_SUB_PROCEDURE_QUAD:
    # Save next (program_counter) value to return after sub_procedure is completed
    functionsStack.append(program_counter + 1)
    # Jump to specified (program_counter)
    return quadruplet.operandLeft

  elif quadruplet.key == END_SUB_PROCEDURE_QUAD:
    # Return to saved (program_counter) value to continue operation after sub_procedure
    return functionsStack.pop() 

  elif quadruplet.key == END_PROGRAM_QUAD:
    # Jump to end of program
    return endOfProgramPC

  return program_counter + 1


def solveOperation(operator, operandLeft, operandRight, symbolsTable):
  # Operand is a dimensional element (given as a hash)
  if (isinstance(operandLeft, str)) and ('@' in operandLeft):
    valOperandLeft = accessDimensionalElement(operandLeft, symbolsTable)
  else:
    valOperandLeft = getOperandValue(operandLeft, symbolsTable)
  # Operand is a dimensional element (given as a hash)
  if (isinstance(operandRight, str)) and ('@' in operandRight):
    valOperandRight = accessDimensionalElement(operandRight, symbolsTable)
  else:
    valOperandRight = getOperandValue(operandRight, symbolsTable)

  # Arithmetic operations
  if operator == '+':
    return valOperandLeft + valOperandRight
  elif operator == '-':
    return valOperandLeft - valOperandRight
  elif operator == '*':
    return valOperandLeft * valOperandRight
  elif operator == '/':
    return valOperandLeft / valOperandRight
  elif operator == 'MOD':
    return valOperandLeft % valOperandRight
  elif operator == 'DIV':
    return math.floor(valOperandLeft / valOperandRight)
  elif operator == '^':
    return valOperandLeft ** valOperandRight
  # Relational operations
  elif operator == '==':
    return valOperandLeft == valOperandRight
  elif operator == '<>':
    return valOperandLeft != valOperandRight
  elif operator == '>':
    return valOperandLeft > valOperandRight
  elif operator == '<':
    return valOperandLeft < valOperandRight
  elif operator == '>=':
    return valOperandLeft >= valOperandRight
  elif operator == '<=':
    return valOperandLeft <= valOperandRight
  # Logical operations
  elif operator == 'AND':
    return valOperandLeft and valOperandRight
  elif operator == 'OR':
    return valOperandLeft or valOperandRight  
  elif operator == 'NOT':
    return not valOperandLeft  