from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial


def stencilMaker(nLeft,nRight,nonUniformGrid,substituteDH,expandSolution,staggered):
    
    RHS = [];LHS = [];EQ = [];Sak = [];
    Sdhk = [];Sfk = [];Sfp = []; temp = [];
    err = 0
    if nLeft>0:
        print 'Error: nLeft must be < 0'
        err = 1
    if nRight<0:
        print 'Error: nRight must be > 0'
        err = 1
    if nRight==0 and nLeft == 0:
        print 'Error: nRight AND nLeft cannot = 0'
        err = 1
        
    if err==0:
        # *************************************************************************
        # ********************* CONSTRUCT SYMBOLIC VARIABLES **********************
        # *************************************************************************
        j = 0
        n = nRight-nLeft+1
        for k in range(nLeft,nRight+1):
            if k>0:
                ak = '\\alpha_{i+'+str(k)+'}'
                fk = 'f_{i+'+str(k)+'}'
                Sak.append(Symbol(ak))
                Sfk.append(Symbol(fk))
            elif k<0:
                ak = '\\alpha_{i'+str(k)+'}'
                fk = 'f_{i'+str(k)+'}'
                Sak.append(Symbol(ak))
                Sfk.append(Symbol(fk))
        for k in range(nLeft,nRight):
            if k>0:
                dhk = '\\Delta h_{i+'+str(k)+'}'
            elif k<0:
                dhk = '\\Delta h_{i'+str(k)+'}'
            elif k==0:
                dhk = '\\Delta h_{i}'
            Sdhk.append(Symbol(dhk))
            
            dhUniform = Symbol('{\\Delta h}')
        
        nUnknowns = n+2 # To include truncation error
        for k in range(0,nUnknowns):
            if (k==0):
                fp = 'f_i'
            elif k>=n:
                fp = 'f_{\\xi}^{('+str(k)+')}'
            else:
                fp = 'f_i^{('+str(k)+')}'
            Sfp.append(Symbol(fp))
            
        print ' --------------------- USER INPUT ---------------------- '
        print 'nLeft = '+str(nLeft)
        print 'nRight = '+str(nRight)
        print 'n = '+str(n)
        print ' ------------------------------------------------------- '
    
        # *************************************************************************
        # ************************* CONVERT ALPHA TO DH ***************************
        # *************************************************************************
    
        if substituteDH and not staggered:
            for j in range(0,len(Sak)+nLeft): # Search Right
                temp = []
                i = j - nLeft
                for k in range(0,len(Sdhk)+nLeft):
                    l = k - nLeft
                    if l<=i:
                        if nonUniformGrid:
                            temp.append(Sdhk[l])
                        else:
                            temp.append(dhUniform)
                Sak[i] = Sak[i].replace(Sak[i],sum(temp))
    
            for j in range(nLeft,0): # Search Left
                temp = []
                i = j - nLeft
                for k in range(nLeft,0):
                    l = k - nLeft
                    if l>=i:
                        if nonUniformGrid:
                            temp.append(-Sdhk[l])
                        else:
                            temp.append(-dhUniform)
                Sak[i] = Sak[i].replace(Sak[i],sum(temp))

        if substituteDH and staggered:
            print '--------- before'
            print Sak
            print '--------- between'
            for j in range(0,len(Sak)+nLeft): # Search Right
                temp = []
                i = j - nLeft
                for k in range(0,len(Sdhk)+nLeft):
                    l = k - nLeft
                    if l<=i:
                        if nonUniformGrid:
                            temp.append(Sdhk[l])
                            if (k==0):
                                temp.append(Sdhk[l]/2)
                            else:
                                temp.append(Sdhk[l])
                        else:
                            if (k==0):
                                temp.append(dhUniform/2)
                            else:
                                temp.append(dhUniform)
                Sak[i] = Sak[i].replace(Sak[i],sum(temp))
            print Sak
            print '--------- after'

            for j in range(nLeft,0): # Search Left
                temp = []
                i = j - nLeft
                for k in range(nLeft,0):
                    l = k - nLeft
                    if l>=i:
                        if nonUniformGrid:
                            if (k==-1):
                                temp.append(-Sdhk[l]/2)
                            else:
                                temp.append(-Sdhk[l])
                        else:
                            if (k==-1):
                                temp.append(-dhUniform/2)
                            else:
                                temp.append(-dhUniform)
                Sak[i] = Sak[i].replace(Sak[i],sum(temp))
            print Sak
    
    
        # *************************************************************************
        # **************************** CONSTRUCT LHS ******************************
        # *************************************************************************
        
        for k in range(0,len(Sfk)):
            LHS.append(Sfk[k])
    
        # *************************************************************************
        # **************************** CONSTRUCT RHS ******************************
        # *************************************************************************
        for k in range(0,len(Sak)):
            temp = [];
            for j in range(0,len(Sfp)):
                temp.append(Sak[k]**j*Sfp[j]/factorial(j))
            RHS.append(sum(temp))
            
        # *************************************************************************
        # **************************** CONSTRUCT EQS ******************************
        # *************************************************************************
        for j in range(0,len(RHS)):
            EQ.append(Eq(LHS[j],RHS[j]))
    
        # *************************************************************************
        # **************************** SOLVE EQUATIONS ****************************
        # *************************************************************************
        print ' ------------------ SOLVING SYSTEM... ------------------'
    
        S = solve(EQ,Sfp[1:],dict=True)
        print ' -------------- FINISHED SOLVING SYSTEM ----------------'
    
        # *************************************************************************
        # ************************ CONVERT SOLUTION TO LIST ***********************
        # *************************************************************************
        Sd = S[0]
        SL = list()
        for key, value in Sd.iteritems():
            if expandSolution:
                temp = [expand(key),expand(value)]
            else:
                temp = [key,value]
            SL.append(temp)
        
        # *************************************************************************
        # *********************** CONVERT SOLUTION TO STRING **********************
        # *************************************************************************

        SS = [];
        for k in range(0,len(SL)):
            SS.append(str(SL[k][0]) + ' = ' + str(SL[k][1]))
        
        print ' ------------------- EXPORTING LATEX ------------------- '

        # *************************** VARIABLES ***************************
        f = open('latex/knowns.tex','w')

        f.write('\\begin{equation} \n')
        for k in range(0,len(Sak)):
            f.write(latex(Sak[k]))
            if k<len(Sak)-1:
                f.write(' , ')
        f.write('\n \\end{equation} \n')
        
        f.write('\\begin{equation} \n')
        f.write(latex(Sfp[0]))
        f.write(' , ')
        for k in range(0,len(Sfk)):
            f.write(latex(Sfk[k]))
            if k<len(Sfk)-1:
                f.write(' , ')
        f.write('\n \\end{equation}')
        f.close()

        f = open('latex/unknowns.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(1,len(Sfp)):
            f.write(latex(Sfp[k]))
            if k<len(Sfp)-1:
                f.write(' , ')
        f.write('\n \\end{equation}')

        f.close()

        # *********** EQUATIONS SOLVED WITHOUT TRUNCATION *****************
        f = open('latex/eqsNoTrunc.tex','w')
        for k in range(0,len(EQ)):
            temp = EQ[k].subs(Sfp[nUnknowns-1],0) # Remove truncation error
            temp = temp.subs(Sfp[nUnknowns-2],0)  # Remove truncation error
            f.write('\\begin{equation} \n')
            f.write(latex(expand(temp)))
            f.write('\n \\end{equation} \n')
        f.close()

        # ************* EQUATIONS SOLVED WITH TRUNCATION ******************
        f = open('latex/eqs.tex','w')
        for k in range(0,len(EQ)):
            f.write('\\begin{equation} \n')
            f.write(latex(expand(EQ[k])))
            f.write('\n \\end{equation} \n')
        f.close()

        # ************************ 1ST DERIVATIVE EQUATION FORM *********** 
        fort = open('latex/dfdxFort.txt','w')
        # ************************ 1ST DERIVATIVE ************************* 
        f = open('latex/dfdx.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(0,len(SL)):
            if str(SL[k][0])=='f_i^{(1)}':
                temp = SL[k][1].subs(Sfp[nUnknowns-1],0) # Remove truncation error
                temp = temp.subs(Sfp[nUnknowns-2],0)     # Remove truncation error
                L = latex(SL[k][0])
                R = latex(simplify(temp))
                Le = SL[k][0]
                Re = simplify(temp)
        p = L+' = '+R
        pe = str(Le) +' = '+str(Re)
        f.write(p)
        fort.write(pe)
        f.write('\n \\end{equation} \n')
        # **************** 1ST DERIVATIVE TRUNCATION **********************
        f.write('\\begin{equation} \n')
        f.write('\\text{Truncation} = ')
        for k in range(0,len(SL)):
            if str(SL[k][0])=='f_i^{(1)}':
                temp = SL[k][1]
                for t in range(0,len(Sfk)):
                    temp = temp.subs(Sfk[t],0) # Remove all BUT truncation error
                    temp = temp.subs(Sfp[0],0) # Remove all BUT truncation error
                p = latex(simplify(temp))
        f.write(p)
        f.write('\n \\end{equation}')

        f.close()
        fort.close()
         # *********************** 2ND DERIVATIVE ************************* 
        f = open('latex/d2fdx2.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(0,len(SL)):
            if str(SL[k][0])=='f_i^{(2)}':
                temp = SL[k][1].subs(Sfp[nUnknowns-1],0) # Remove truncation error
                temp = temp.subs(Sfp[nUnknowns-2],0)     # Remove truncation error
                L = latex(SL[k][0])
                R = latex(simplify(temp))
        p = L+' = '+R
        f.write(p)
        f.write('\n \\end{equation} \n')
        # **************** 2ND DERIVATIVE TRUNCATION **********************
        f.write('\\begin{equation} \n')
        f.write('\\text{Truncation} = ')
        for k in range(0,len(SL)):
            if str(SL[k][0])=='f_i^{(1)}':
                temp = SL[k][1]
                for t in range(0,len(Sfk)):
                    temp = temp.subs(Sfk[t],0) # Remove all BUT truncation error
                    temp = temp.subs(Sfp[0],0) # Remove all BUT truncation error
                p = latex(simplify(temp))
        f.write(p)
        f.write('\n \\end{equation}')

        f.close()

        print ' ---------------------- FINISHED ----------------------- '





