
initialValueByType = {
  'WORD': int(0),
  'FLOAT': float(0.0),
  'BOOLEAN': False,
  'SUB_PROCEDURE': None,
}

class SymbolsTableStructure:
  def __init__(self, variableId, variableType, variableAddress, functionIndex):
    self.id = variableId
    self.type = variableType
    self.address = variableAddress
    self.functionIndex = functionIndex
    self.value = initialValueByType[variableType]