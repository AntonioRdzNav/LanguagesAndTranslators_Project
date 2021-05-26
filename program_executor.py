
from SymbolsTableStructure import SymbolsTableStructure
from QuadrupletStructure import QuadrupletStructure


def execute_program(quadruplets, symbolsTable):
    program_counter = 0
    while program_counter < len(quadruplets):
        program_counter = executeSingleQuadruplet(quadruplets[program_counter], program_counter, symbolsTable)


def executeSingleQuadruplet(singleQuadruplet, program_counter, symbolsTable):

    return program_counter + 1