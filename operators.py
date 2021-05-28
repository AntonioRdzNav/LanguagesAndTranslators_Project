import operator

arithmetic_operators = [
  '+',
  '-',
  '*',
  '/',
  'MOD',
  'DIV',
  '^'
]

relational_operators = [
  '==',
  '<>',
  '<',
  '<=',
  '>',
  '>='
]

logical_operators = [
  'NOT',
  'OR',
  'AND'
]

dimensionalOperatorStringToFunctional = {
  '+': operator.add,
  '-': operator.sub,
  '*': operator.truediv,
  '/': operator.mul,
}