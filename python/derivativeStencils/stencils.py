
import os
clear = lambda: os.system('cls')
clear()

from sympy import *
import prettyPy as pp

nLeft = -2
nRjght = 2
RHS = [];LHS = [];EQ = [];Sak = [];Sfk = [];Sfp = []; temp = [];
err = 0

if nLeft>0:
    print 'Error: nLeft must be < 0'
    err = 1
if nRight<0:
    print 'Error: nRight must be > 0'
    err = 1

if err==0:
    n = nRight - nLeft
    nMiddle = -2
    j = 0
    for k in range(nLeft,nRight+1):
        if k>0:
            ak = 'alpha_{i+'+str(k)+'}'
            fk = 'f_{i+'+str(k)+'}'
        elif k<0:
            ak = 'alpha_{i'+str(k)+'}'
            fk = 'f_{i'+str(k)+'}'
        elif k==0:
            ak = 'alpha_{i}'
            # fk = 'f_{i}' # Use this when done
            fk = 'f_{i+0}'
        if (j==0):
            fp = '{f_i}'
        else:
            fp = '{f_i}^{('+str(j)+')}'
    
        Sak.append(Symbol(ak))
        Sfk.append(Symbol(fk))
        Sfp.append(Symbol(fp))
        j = j+1
        
    print ' ------------------- CONSTRUCTED VARIABLES ------------------- '
    print Sak
    print Sfk
    print Sfp
    j = 0
    for k in range(nLeft,nRight+1):
        LHS.append(Sfk[j])
        j = j+1
    j = 0
    for k in range(nLeft,nRight+1):
        if (j==-nLeft):
            temp.append(Sfp[j]/factorial(j))
        else:
            temp.append(Sak[j]*Sfp[j]/factorial(j))
        j = j+1
    for k in range(nLeft,nRight+1):
        RHS.append(sum(temp))
        j = j+1
    j = 0
    # RHS.append(sum(temp))
    for k in range(nLeft,nRight+1):
        EQ.append(Eq(LHS[j],RHS[j]))
        j = j+1
    
    print ' ------------------- CONSTRUCTED EQUATIONS ------------------- '
#    for k in range(0,n+1):
#        print EQ[k]
    for k in range(0,len(EQ)):
        print EQ[k]
    
    print ' ------------------------------------------------------------- '
    
    print ' --------------------- TRUNCATION ERROR ---------------------- '
    
    print ' --------------------- ORDER OF ACCURACY --------------------- '
    
    print ' ---------------------- LATEX OUTPUT ------------------------- '

    
    print type(EQ[0])
    s = EQ[0]
    s = 'f_{i-2} == alpha_{i+1}*{f_i}^{(3)}/6 + alpha_{i+2}*{f_i}^{(4)}/24 + alpha_{i-1}*{f_i}^{(1)} + alpha_{i-2}*{f_i}^{(0)} + alpha_{i}*{f_i}^{(2)}/2'
    print pp.pretty(s)





























