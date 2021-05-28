import numpy as np

class SymbolsTableStructure:
  def __init__(self, variableId, variableType, variableAddress, functionIndex, dimension1, dimension2, dimension3):
    self.id = variableId
    self.type = variableType
    self.address = variableAddress
    self.functionIndex = functionIndex
    if variableType == 'WORD':
        self.value = int(0)
    elif variableType == 'FLOAT':
        self.value = float(0.0)
    elif variableType == 'BOOLEAN':
        self.value = False
    elif variableType == 'SUB_PROCEDURE':
        self.value = None  
    elif variableType == 'WORD_ARRAY':
        self.value = np.zeros((dimension1), dtype=int)
    elif variableType == 'WORD_MATRIX':
        self.value = np.zeros((dimension1, dimension2), dtype=int)
    elif variableType == 'WORD_CUBE':
        self.value = np.zeros((dimension1, dimension2, dimension3), dtype=int)  
    elif variableType == 'FLOAT_ARRAY':
        self.value = np.zeros((dimension1), dtype=float)
    elif variableType == 'FLOAT_MATRIX':
        self.value = np.zeros((dimension1, dimension2), dtype=float)
    elif variableType == 'FLOAT_CUBE':
        self.value = np.zeros((dimension1, dimension2, dimension3), dtype=float)  