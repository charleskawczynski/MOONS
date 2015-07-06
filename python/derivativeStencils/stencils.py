import stencilMaker as sm

import os
clear = lambda: os.system('cls')
clear()

nLeft = -2             # Number of points to the LEFT of central location (may be 0)
nRight = 2             # Number of points to the RIGHT of central location (may be 0)
nonUniformGrid = False # Use non-uniform grid spacing
substituteDH = False   # Substitute alpha_{i+k} for (h_{i+k} - h_{i})
expandSolution = False # Attempt to expand solution
staggered = False      # Make result on dual grid (true) DOES NOT WORK YET...

sm.stencilMaker(nLeft,nRight,nonUniformGrid,substituteDH,expandSolution,staggered)

# ******************************
# Once this file has been run, 
# build the latex file in order 
# to view the results
# ******************************
