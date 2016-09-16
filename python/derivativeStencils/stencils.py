import stencilMaker as sm

import os
clear = lambda: os.system('cls')
clear()

nLeft = -1             # Number of points to the LEFT of central location (may be 0)
nRight = 1             # Number of points to the RIGHT of central location (may be 0)

nonUniformGrid = False # Use non-uniform grid spacing
substituteDH = True   # Substitute alpha_{i} for (h_{i+k} - h_{i}), takes longer
multiLine = False       # multiline output


# ******************************
# To get staggered derivatives,
# take the average between the
# result at i and i+1. The 
# stencil will be larger, but 
# the order of accuracy should 
# not change.
# ******************************

SM = sm.stencilMaker(nLeft,nRight,nonUniformGrid,substituteDH,multiLine)
SM.prepVariables()
SM.solveSystem()
SM.prepSolutionLatex()
SM.exportLatexSetup()
SM.exportAllTruncations()
SM.exportAllLatexSolutions()
#SM.exportLatexSolution_old()
#SM.test()


# ******************************
# Once this file has been run, 
# build the latex file in order 
# to view the results
# ******************************
