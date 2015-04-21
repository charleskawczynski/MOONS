# This program calculates the coefficients of the 1st and 2nd
# derivative stencils made from the file "Non-uniform grid 
# stencil" in the documentation folder.

from __future__ import division
import sympy as sp
import math as myMath

def stencilMaker(alpha,beta,i,j,k,dx,s,checkForErrors):
    
    f_i = sp.symbols('f_{'+str(i).replace(' ','')+'}')
    f_k = sp.symbols('f_{'+str(k).replace(' ','')+'}')
    f_j = sp.symbols('f_{'+str(j).replace(' ','')+'}')
    
    itemp = i
    jtemp = j
    ktemp = k
    
    g_0 = sp.symbols('g_{0}')
    
    #f_i = symbols('f_{i}')
    #f_k = symbols('f_{k}')
    #f_j = symbols('f_{j}')
    
    # Check for input errors
    err = 0
    if checkForErrors:
        if alpha == beta:
            err = 1
            print 'ERROR: alpha_k cannot equal beta_j'
        if i == j:
            err = 1
            print 'ERROR: i cannot equal j'
        if j == k:
            err = 1
            print 'ERROR: j cannot equal k'
        if i == k:
            err = 1
            print 'ERROR: i cannot equal k'
        
        try: 
            if not myMath.copysign(1,(k-i).subs(s,1)) == myMath.copysign(1,alpha.subs(dx,1)):
                err = 1
                print 'ERROR: The sign of k and alpha_k must match'
            
            if not myMath.copysign(1,(j-i).subs(s,1)) == myMath.copysign(1,beta.subs(dx,1)):
                err = 1
                print 'ERROR: The sign of j and beta_j must match'
            sgiven = 1
        except:
            if not myMath.copysign(1,(k-i)) == myMath.copysign(1,alpha.subs(dx,1)):
                err = 1
                print 'ERROR: The sign of k and alpha_k must match'
            
            if not myMath.copysign(1,(j-i)) == myMath.copysign(1,beta.subs(dx,1)):
                err = 1
                print 'ERROR: The sign of j and beta_j must match'
            sgiven = 0
    
    
    
        if not myMath.copysign(1,j-k) == myMath.copysign(1,(beta-alpha).subs(dx,1)):
            err = 1
            print 'ERROR: Alpha and beta are not consistent with their indecies'
        
        if max(i,j,k) - min(i,j,k) > 2:
            err = 1
            print 'The index range is too large.'
    
    if err == 0:
        # Initialize arrays
        f = [0, 0, 0]
        df = [0, 0, 0]
        ddf = [0, 0, 0]
        
        print ''
        print '----------------- Input Parameters -------------------'
        print 'i,k,j = ' + str(itemp).replace(' ','') + ', ' + str(ktemp).replace(' ','') + ', ' + str(jtemp).replace(' ','')
        print 'alpha_k = ' + str(alpha)
        print 'beta_j = ' + str(beta)
        
        # Shift indicies for accessing array
        mi = min(i,j,k)
        ma = max(i,j,k)
        i = i-mi
        j = j-mi
        k = k-mi
        mi = min(i,j,k)
        ma = max(i,j,k)
        
        # Print output
        print '------------------- 1st derivative -------------------'
        df[i] = 1/(beta-alpha)*(alpha/beta - beta/alpha)*f_i
        df[k] = 1/(beta-alpha)*(beta/alpha)*f_k
        df[j] = 1/(beta-alpha)*(-alpha/beta)*f_j
        print 'df[' + str(itemp).replace(' ','') + '] = '
#        if sgiven == 1:
#            print df[::-1]
#        else:
#            print df
        print df[::-1]
        print df
        
        print '------------------- 2nd derivative -------------------'
        ddf[i] = 2/(alpha*beta)*f_i
        ddf[k] = 2/(alpha**2-alpha*beta)*f_k
        ddf[j] = 2/(beta**2 - beta*alpha)*f_j
        print 'ddf[' + str(itemp).replace(' ','') + '] = '
#        if sgiven == 1:
#            print ddf[::-1]
#        else:
#            print ddf
        print ddf[::-1]
        print ddf

        if mi == i or ma == i:
            print '------------------- Boundary Values -------------------'
            f[i] = (beta - alpha)/(alpha/beta - beta/alpha)*g_0
            f[k] = -beta/alpha/(alpha/beta - beta/alpha)*f_k
            f[j] = alpha/beta/(alpha/beta - beta/alpha)*f_j
            print 'f_boundary[' + str(itemp).replace(' ','') + '] = '
#            if sgiven == 1:
#                print f[::-1]
#            else:
#                print f
            print f[::-1]
            print f
    
        print ''
    
