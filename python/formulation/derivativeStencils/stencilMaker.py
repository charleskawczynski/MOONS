from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial

class stencilMaker:

    def __init__(self,nLeft,nRight,nonUniformGrid,substituteDH,multiLine):
        self.nLeft = nLeft
        self.nRight = nRight
        self.nonUniformGrid = nonUniformGrid
        self.substituteDH = substituteDH
        self.multiLine = multiLine

    def prepVariables(self):
        self.Sak = []; self.Sdhk = [];self.Sfk = [];self.Sfp = []; temp = [];
        self.Sfk_all = []
        err = 0
        if self.nLeft>0:
            print 'Error: nLeft must be < 0'
            err = 1
        if self.nRight<0:
            print 'Error: nRight must be > 0'
            err = 1
        if self.nRight==0 and self.nLeft == 0:
            print 'Error: nRight AND nLeft cannot = 0'
            err = 1
        if err==0:
            # ********************* CONSTRUCT SYMBOLIC VARIABLES **********************
            j = 0
            self.n = self.nRight-self.nLeft+1
            for k in range(self.nLeft,self.nRight+1):
                if k>0:
                    ak = '\\alpha_{i+'+str(k)+'}'
                    fk = 'f_{i+'+str(k)+'}'
                    self.Sak.append(Symbol(ak))
                    self.Sfk.append(Symbol(fk))
                    self.Sfk_all.append(Symbol(fk))
                elif k<0:
                    ak = '\\alpha_{i'+str(k)+'}'
                    fk = 'f_{i'+str(k)+'}'
                    self.Sak.append(Symbol(ak))
                    self.Sfk.append(Symbol(fk))
                    self.Sfk_all.append(Symbol(fk))
                elif k==0:
                    fk = 'f_{i}'
                    self.Sfk_all.append(Symbol(fk))
            for k in range(self.nLeft,self.nRight):
                if k>0:
                    dhk = '\\Delta h_{i+'+str(k)+'}'
                elif k<0:
                    dhk = '\\Delta h_{i'+str(k)+'}'
                elif k==0:
                    dhk = '\\Delta h_{i}'
                self.Sdhk.append(Symbol(dhk))

                self.dhUniform = Symbol('{\\Delta h}')

            self.nUnknowns = self.n+2 # To include truncation error
            for k in range(0,self.nUnknowns):
                if (k==0):
                    fp = 'f_i'
                elif k>=self.n:
                    fp = 'f_{\\xi}^{('+str(k)+')}'
                else:
                    fp = 'f_i^{('+str(k)+')}'
                self.Sfp.append(Symbol(fp))
            print ' --------------------- USER INPUT ---------------------- '
            print 'nLeft = '+str(self.nLeft)
            print 'nRight = '+str(self.nRight)
            print 'n = '+str(self.n)
            print ' --------------------- VARIABLES ---------------------- '
            print 'Sak = '+str(self.Sak)
            print 'Sdhk = '+str(self.Sdhk)
            print 'Sfk = '+str(self.Sfk)
            print 'Sfp = '+str(self.Sfp)
            print ' ------------------------------------------------------- '
            # ************************* CONVERT ALPHA TO DH ***************************
            if self.substituteDH:
                for j in range(0,len(self.Sak)+self.nLeft): # Search Right
                    temp = []
                    i = j - self.nLeft
                    for k in range(0,len(self.Sdhk)+self.nLeft):
                        l = k - self.nLeft
                        if l<=i:
                            if self.nonUniformGrid:
                                temp.append(self.Sdhk[l])
                            else:
                                temp.append(self.dhUniform)
                    self.Sak[i] = self.Sak[i].replace(self.Sak[i],sum(temp))

                for j in range(self.nLeft,0): # Search Left
                    temp = []
                    i = j - self.nLeft
                    for k in range(self.nLeft,0):
                        l = k - self.nLeft
                        if l>=i:
                            if self.nonUniformGrid:
                                temp.append(-self.Sdhk[l])
                            else:
                                temp.append(-self.dhUniform)
                    self.Sak[i] = self.Sak[i].replace(self.Sak[i],sum(temp))

    def solveSystem(self):
        self.RHS = [];self.LHS = [];self.EQ = [];
        # **************************** CONSTRUCT LHS ******************************
        for k in range(0,len(self.Sfk)):
            self.LHS.append(self.Sfk[k])
        # **************************** CONSTRUCT RHS ******************************
        for k in range(0,len(self.Sak)):
            temp = [];
            for j in range(0,len(self.Sfp)):
                temp.append(self.Sak[k]**j*self.Sfp[j]/factorial(j))
            self.RHS.append(sum(temp))
        # **************************** CONSTRUCT EQS ******************************
        for j in range(0,len(self.RHS)):
            self.EQ.append(Eq(self.LHS[j],self.RHS[j]))
        # **************************** SOLVE EQUATIONS ****************************
        print ' ------------------ SOLVING SYSTEM... ------------------'
        self.S = solve(self.EQ,self.Sfp[1:],dict=True)
        print ' -------------- FINISHED SOLVING SYSTEM ----------------'

    def prepSolutionLatex(self):
        # ************************ CONVERT SOLUTION TO LIST ***********************
        Sd = self.S[0]
        self.SL = list()
        for key, value in Sd.iteritems():
            temp = [expand(key),expand(value)]
            self.SL.append(temp)
        # *********************** CONVERT SOLUTION TO STRING **********************
        SS = [];
        for k in range(0,len(self.SL)):
            SS.append(str(self.SL[k][0]) + ' = ' + str(self.SL[k][1]))

    def exportLatexSetup(self):
        print ' ------------ EXPORTING LATEX SETUP'
        # ***************** EQS SOLVED WITHOUT TRUNCATION ******************
        f = open('latex/eqsNoTrunc.tex','w')
        for k in range(0,len(self.EQ)):
            temp = self.EQ[k].subs(self.Sfp[self.nUnknowns-1],0) # Remove truncation error
            temp = temp.subs(self.Sfp[self.nUnknowns-2],0)  # Remove truncation error
            f.write('\\begin{equation} \n')
            f.write(latex(expand(temp)))
            f.write('\n \\end{equation} \n')
        f.close()
        # ***************** EQS SOLVED WITH TRUNCATION ********************
        f = open('latex/eqs.tex','w')
        for k in range(0,len(self.EQ)):
            f.write('\\begin{equation} \n')
            f.write(latex(expand(self.EQ[k])))
            f.write('\n \\end{equation} \n')
        f.close()
        # **************************** KNOWNS *****************************
        f = open('latex/knowns.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(0,len(self.Sak)):
            f.write(latex(self.Sak[k]))
            if k<len(self.Sak)-1:
                f.write(' , ')
        f.write('\n \\end{equation} \n')
        f.write('\\begin{equation} \n')
        f.write(latex(self.Sfp[0]))
        f.write(' , ')
        for k in range(0,len(self.Sfk)):
            f.write(latex(self.Sfk[k]))
            if k<len(self.Sfk)-1:
                f.write(' , ')
        f.write('\n \\end{equation}')
        f.close()
        # **************************** UNKNOWNS ****************************
        f = open('latex/unknowns.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(1,len(self.Sfp)):
            f.write(latex(self.Sfp[k]))
            if k<len(self.Sfp)-1:
                f.write(' , ')
        f.write('\n \\end{equation}')
        f.close()

    def exportAllTruncations(self):
        self.exportTruncation('dfdx_Truncation',1)
        self.exportTruncation('d2fdx2_Truncation',2)
        print ' ------------ FINISHED'

    def exportTruncation(self,filename,nthDerivative):
        print ' ------------ EXPORTING ' + filename
        # **************** 1ST DERIVATIVE TRUNCATION **********************
        f = open('latex/'+filename+'.tex','w')
        f.write('\\begin{equation} \n')
        f.write('\\text{Truncation} = ')
        for k in range(0,len(self.SL)):
            if str(self.SL[k][0])=='f_i^{('+str(nthDerivative)+')}':
                temp = self.SL[k][1]
                for t in range(0,len(self.Sfk)):
                    temp = temp.subs(self.Sfk[t],0) # Remove all BUT truncation error
                    temp = temp.subs(self.Sfp[0],0) # Remove all BUT truncation error
                p = latex(simplify(temp))
        f.write(p)
        f.write('\n \\end{equation}')
        f.close()

    def exportAllLatexSolutions(self):
        self.exportLatexSolution('dfdx',1)
        self.exportLatexSolution('d2fdx2',2)
        self.exportLatexSolution_safe('dfdx_safe',1)
        self.exportLatexSolution_safe('d2fdx2_safe',2)
        print ' ------------ FINISHED'

    def exportLatexSolution(self,filename,nthDerivative):
        # This routine exports the solution with factoring
        print ' ------------ EXPORTING ' + filename
        # ************************ 1ST DERIVATIVE *************************
        f = open('latex/'+filename+'.tex','w')
        if self.multiLine:
            f.write('\\begin{multline} \n'); f.write('\\begin{aligned} \n')
        else:
            f.write('\\begin{equation} \n')
        coeffs = []

        for t in range(0,len(self.SL)):
            if str(self.SL[t][0])=='f_i^{('+str(nthDerivative)+')}':
                temp = self.SL[t][1].subs(self.Sfp[self.nUnknowns-1],0) # Remove truncation error
                temp = temp.subs(self.Sfp[self.nUnknowns-2],0)     # Remove truncation error
                L = latex(self.SL[t][0])
                m = 0
                for k in range(self.nLeft,self.nRight+1):
                    if k==0:
                        coeffs.append(temp.coeff(self.Sfp[0]))
                    else:
                        coeffs.append(temp.coeff(self.Sfk_all[m]))
                    m = m+1
        R = []
        tail = '' if not self.multiLine else ' \\\\'
        for k in range(0,len(coeffs)):
            R.append('\left('+latex(simplify(coeffs[k]))+'\\right)'+latex(self.Sfk_all[k]) + '+'+tail)
        R = ' '.join(R)

        num = 4 if self.multiLine else 1
        p = L + ' = ' + R[:-num] # last plus sign, space and \\
        f.write(p)
        if self.multiLine:
            f.write('\n \end{aligned}'); f.write('\n \\end{multline} \n')
        else:
            f.write('\n \\end{equation} \n')
        f.close()

    def exportLatexSolution_safe(self,filename,nthDerivative):
        # This routine exports the solution without factoring, it may
        # be useful for debugging
        print ' ------------ EXPORTING ' + filename
        # ************************ 1ST DERIVATIVE *************************
        f = open('latex/'+filename+'.tex','w')
        f.write('\\begin{equation} \n')
        for k in range(0,len(self.SL)):
            if str(self.SL[k][0])=='f_i^{('+str(nthDerivative)+')}':
                temp = self.SL[k][1].subs(self.Sfp[self.nUnknowns-1],0) # Remove truncation error
                temp = temp.subs(self.Sfp[self.nUnknowns-2],0)     # Remove truncation error
                L = latex(self.SL[k][0])
                R = latex(self.SL[k][1])
        p = L + ' = ' + R
        f.write(p)
        f.write('\n \\end{equation} \n')
        f.close()

