import stencilMaker as sm

import os
clear = lambda: os.system('cls')
clear()

nLeft = -1             # Number of points to the LEFT of central location (may be 0)
nRight = 1             # Number of points to the RIGHT of central location (may be 0)
nonUniformGrid = True  # Use non-uniform grid spacing
substituteDH = True    # Substitute alpha_{i+k} for (h_{i+k} - h_{i})
expandSolution = False # Attempt to expand solution

sm.stencilMaker(nLeft,nRight,nonUniformGrid,substituteDH,expandSolution)

# ******************************
# Once this file has been run, 
# build the latex file in order 
# to view the results
# ******************************
