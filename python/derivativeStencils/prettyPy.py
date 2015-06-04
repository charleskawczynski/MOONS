from __future__ import division # Turn off integer division
import re
import numpy as np
from itertools import *
from operator import *
from IPython.display import display, Math, Latex

#-------------------------------------------------
# Program: prettyPy
#     This program receives a string of some 
#     equation and wraps it in LaTeX code, and
#     outputs the result to reduce the verbose
#     implementation of writing LaTeX equations.
# Developer: Charlie Kawczynski
# Email:       charliekawczynski@gmail.com
# Available @: charliekawczynski.com
#-------------------------------------------------
# Example:
# from prettyPy import prettyPy as pp
# a = 'b*cos(theta) + c**(d*sin(gamma))'
# pp.prettyPrint(a)
# print pp.pretty(a) + ' = ' + str(eval(a))

# Notes:
# The only place where "\"'s are required is when
# you want to use a ', because this is a special symbol
# in python..
#-------------------------------------------------
# Improvements to come:

# -Remove replacement of * symbol to allow convolution expressions
# and t^* superscripts..

# - allow for defining variables in a local file for later use
# ex: setVars({
# "v_vec":"{\bf v_vec}",
# "B":"{\bf B}",
# })

# -Reorganize greek letters, symbols, and user defined variables,
# make them into a list and replace by decreasing string size
# Add in parsing strings by seperated equal signs at level zero...
#-------------------------------------------------
# Cases to fix:
# THIS, IS A STRANGE ONE...
# pp('pd_x f(eta) = A^(1/2)/2 y/nu^(1/2) x^(2m-3) (m-1) f\'')
# Requires parenthesis: pp('U_e^10 d/dx delta^2 + delta^2 d/dx U_e^(10) = 12 nu U_e^8')
#-------------------------------------------------
# General things to fix:
# -try moving exponent of sin()**2 to sin**2() and delta(t)^2 to delta^2(t)

def pretty(s):
    return markup(s)

def prettyPrint(s):
    return display(Math(markup(s)))

def defineVars(s):
    # Replace Greek Letters:
    s = replaceGreekLetter(s,'alpha')
    s = replaceGreekLetter(s,'Alpha')
    s = replaceGreekLetter(s,'beta')
    s = replaceGreekLetter(s,'Beta')
    s = replaceGreekLetter(s,'Delta')
    s = replaceGreekLetter(s,'delta')
    s = replaceGreekLetter(s,'eta')
    s = replaceGreekLetter(s,'epsilon')
    s = replaceGreekLetter(s,'gamma')
    s = replaceGreekLetter(s,'Gamma')
    s = replaceGreekLetter(s,'kappa')
    s = replaceGreekLetter(s,'lambda')
    s = replaceGreekLetter(s,'nabla')
    s = replaceGreekLetter(s,'mu')
    s = replaceGreekLetter(s,'nu')
    s = replaceGreekLetter(s,'omega')
    s = replaceGreekLetter(s,'Omega')
    s = replaceGreekLetter(s,'partial')
    s = replaceGreekLetter(s,'pi')
    s = replaceGreekLetter(s,'Pi')
    s = replaceGreekLetter(s,'psi')
    s = replaceGreekLetter(s,'Psi')
    s = replaceGreekLetter(s,'Phi')
    s = replaceGreekLetter(s,'rho')
    s = replaceGreekLetter(s,'sigma')
    s = replaceGreekLetter(s,'Sigma')
    s = replaceGreekLetter(s,'tau')
    s = replaceGreekLetter(s,'Tau')
    s = replaceGreekLetter(s,'theta')
    s = replaceGreekLetter(s,'Theta')
    s = replaceGreekLetter(s,'varphi')
    s = replaceGreekLetter(s,'varepsilon')
    s = replaceGreekLetter(s,'xi')
    s = replaceGreekLetter(s,'Xi')
    s = replaceGreekLetter(s,'zeta')
    # Define Symbols and Characters:
    s = replaceGreekLetter(s,'nless')
    s = replaceGreekLetter(s,'leq')
    s = replaceGreekLetter(s,'ll')
    s = replaceGreekLetter(s,'gg')
    s = replaceGreekLetter(s,'ggg')
    s = replaceGreekLetter(s,'parallel')
    s = replaceGreekLetter(s,'parallel')
    s = replaceGreekLetter(s,'propto')
    s = replaceGreekLetter(s,'max')
    s = replaceGreekLetter(s,'min')
    s = replaceGreekLetter(s,'sum')
    s = replaceGreekLetter(s,'det')
    s = replaceGreekLetter(s,'iiint')
    s = replaceGreekLetter(s,'iint')
    s = replaceGreekLetter(s,'int')
    s = replaceGreekLetter(s,'infty')
    s = replaceGreekLetter(s,'dagger')
    s = replaceGreekLetter(s,'ast')
    s = replaceGreekLetter(s,'div')
    s = replaceGreekLetter(s,'times')
    s = replaceGreekLetter(s,'times')

    s = userDefinedVars(s,'cross','\\times')
    s = userDefinedVars(s,'dot','\\bullet')
    s = userDefinedVars(s,'infinity','\\infty')
#    s = userDefinedVars(s,'order','\\cal O') # This is the script 'order' symbol, and requires a certain plugin...

    # Define Truncated Symbols and Characters (if truncation can be found in original):
    s = userDefinedRecursiveVars(s,'approx','\\approx')
    s = userDefinedRecursiveVars(s,'eps','\\epsilon')
    s = userDefinedRecursiveVars(s,'sig','\\sigma')
    s = userDefinedRecursiveVars(s,'ne','\\ne')
    s = userDefinedRecursiveVars(s,'Sig','\\Sigma')
    # Define Personal Variables:
    s = userDefinedVars(s,'pd','\\partial')
    s = userDefinedVars(s,'star','*')
    s = s.replace('-->','\\rightarrow') # a bit sloppy, but works
    s = s.replace('<--','\\leftarrow') # a bit sloppy, but works
    return s

def functions(s):
    s = g_of_x(s,'sqrt(')
    
    s = g_of_x(s,'sin(')
    s = g_of_x(s,'cos(')
    s = g_of_x(s,'tan(')

    s = g_of_x(s,'sinh(')
    s = g_of_x(s,'cosh(')
    s = g_of_x(s,'tanh(')
    
    s = g_of_x(s,'asin(')
    s = g_of_x(s,'sin^{-1}(')
    s = g_of_x(s,'acos(')
    s = g_of_x(s,'cos^{-1}(')
    s = g_of_x(s,'atan(')
    s = g_of_x(s,'tan^{-1}(')
    
    s = g_of_x(s,'csc(')
    s = g_of_x(s,'sec(')
    s = g_of_x(s,'cot(')
    return s

def userDefinedVars(s,L,Lnew):
    r = 0
    for k in range(0,s.count(L)):
        loc = find_nth(s,L,k+1+r)
        if loc == 0:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)])
        elif loc == len(s) - len(L):
            TF = not re.match(r'[A-Za-z0-9]',s[loc-1])
        else:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)]) and not re.match(r'[A-Za-z0-9]',s[loc-1])
        if TF:
            s_before = s[0:loc]
            s_after = s[loc+len(L):]
            s = s_before + Lnew + s_after
            r = r-1
    return s

def userDefinedRecursiveVars(s,L,Lnew):
    for k in range(0,s.count(L)):
        loc = find_nth(s,L,k+1)
        if loc == 0:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)])
        elif loc == len(s) - len(L):
            TF = not re.match(r'[A-Za-z0-9]',s[loc-1])
        else:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)]) and not re.match(r'[A-Za-z0-9]',s[loc-1])
        if TF:
            s_before = s[0:loc]
            s_after = s[loc+len(L):]
            s = s_before + Lnew + s_after
    return s

def replaceGreekLetter(s,L):
    Lnew = '\\' + L
    for k in range(0,s.count(L)):
        loc = find_nth(s,L,k+1)
        if loc == 0:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)])
        elif loc == len(s) - len(L):
            TF = not re.match(r'[A-Za-z0-9]',s[loc-1])
        else:
            TF = not re.match(r'[A-Za-z0-9]',s[loc+len(L)]) and not re.match(r'[A-Za-z0-9]',s[loc-1])
        if TF:
            s_before = s[0:loc]
            s_after = s[loc+len(L):]
            s = s_before + Lnew + s_after
    return s

def format_res(n,units):
    if abs(n) < 1000 and abs(n) > 1/1000:
        if units.replace(' ','') == '':
            res = str('%.3f' % n)
        else:
            res = str('%.3f' % n) + ' [' + units + ']'
        return res
    else:
        a = '%.3E' % n
        n_formatted = a.split('E')[0].rstrip('0').rstrip('.') + 'E' + a.split('E')[1]
        if units.replace(' ','') == '':
            res = n_formatted
        else:
            res = n_formatted + ' [' + units + ']'
        return res

def parseByEqualSigns(s):
    p_any = step(s,'(',')') + step(s,'[',']') + step(s,'{','}')
    ss = []
    seg = s
    for j in range(0,s.count('=')):
        L = find_nth(seg,'=',j+1)
        if p_any[L] == 0:
            ss.append(seg[0:L])
            ss.append(seg[L])
            seg = seg[L+1:]
    ss.append(seg)
    snew = []
    if ss:
        for k in ss:
            snew.append(markupMain(k))
        s = ''.join(snew)
        return s
    else:
        return markupMain(s)

def parseByAmpersandSigns(s):
    p_any = step(s,'(',')') + step(s,'[',']') + step(s,'{','}')
    ss = []
    seg = s
    for j in range(0,s.count('&')):
        L = find_nth(seg,'&',j+1)
        if p_any[L] == 0:
            ss.append(seg[0:L])
            ss.append(seg[L])
            seg = seg[L+1:]
    ss.append(seg)
    snew = []
    if ss:
        for k in ss:
            snew.append(parseByEqualSigns(k))
        s = ''.join(snew)
        return s
    else:
        return parseByEqualSigns(s)

def postProcess(s):
    if not s.count('{') == s.count('}'):
        print 'UNEQUAL NUMBER OF CURLY BRACES'
    if not s.count('[') == s.count(']'):
        print 'UNEQUAL NUMBER OF BRACKETS'
    if not s.count('(') == s.count(')'):
        print 'UNEQUAL NUMBER OF PARENTHASES'
    s = s.replace('(',' \\left( ')
    s = s.replace(')',' \\right) ')
    s = s.replace('[',' \\left[ ')
    s = s.replace(']',' \\right] ')
    s = s.replace('*',' ')
    s = s.replace('&','&&, \\qquad \\qquad')
    s = '\\begin{align}' + s + '\\end{align}'
    s = '$$' + s + '$$'
    return s

def markup(s):
    s = markupMain(s)
#    s = parseByAmpersandSigns(s)
    s = postProcess(s)
    s = defineVars(s)
    return s

def markupMain(s):
    s = impliedMultiplication(s)
    s = s.replace(' ','')
    s = s.replace("**","^")
    s = superscriptsWithParen(s)
    s = subscripts(s)
    s = superscriptsWithoutParen(s)
    s = fractions(s)
    s = functions(s)
    s = removeExcessParenthesis(s)
    return s


def consecutiveLists(data):
    d = {}; i = 0
    for k, g in groupby(enumerate(data), lambda (i,x):i-x):
        d[i] = map(itemgetter(1), g)
        i = i + 1
    return d

def step(s,openDelim,closeDelim):
    p = np.zeros(len(s))
    for i in range(len(s)-1):
        if s[i] == openDelim:
            p[i:] = p[i:] + 1
        elif s[i] == closeDelim:
            p[i+1:] = p[i+1:] - 1
    return p

def replaceLevelJ(s,p):
    try:
        if p == 0:
            p = step(s,'(',')')
    except:pass
    t = np.where(p == max(p))
    cl = consecutiveLists(t[0])
    s_new = s
    for i in cl:
        s_prefix = s_new[cl[i][0]-1]
        s_before = s_new[0:cl[i][0]]
        s_after = s_new[cl[i][-1]+1:]
        s_cut = s_new[cl[i][0]:cl[i][-1]+1]
        if s_prefix == '^':
            s_mid = '{' + s_cut[1:-1] + '}'
            s_new = s_before + s_mid + s_after
        for j in cl[i]:
            p[j] = p[j] - 1
            
    return (p,s_new)

def superscriptsWithParen(s):
    n = s.count('(') + s.count('_')
    (p,s_new) = replaceLevelJ(s,0)
    for j in range(n-1,0,-1):
        s_temp = s_new
        (p,s_new) = replaceLevelJ(s_temp,p)
        if max(p) == 0:
            break
    return s_new

def subscripts(s):
    r = re.compile(r'_[^\}\(\)\]\=\+\-\^\/\*]*')
    m = list(set(r.findall(s)))
    m.sort(lambda x,y: cmp(len(x), len(y)), reverse=True)
    for j in m:
        s_mid = j[0] + '{' + j[1:] + '}'
        s = s.replace(j,s_mid)
    return s

def superscriptsWithoutParen(s):
    r = re.compile(r'\^[^\{]{1}[^\*\)\+\=\-\/]+')
    la = list(set(r.findall(s)))
    for i in la:
        if i[0:1] == '^{': # .startswith('^{')
            la.remove(i)
    for j in range(0,len(la)):
        if not la[j].count('{') == la[j].count('}'):
            temp_la = la[j][0:-1]
            la.pop(j)
            la.insert(j,temp_la)
    for j in la:
        k = s.index(j)
        s_before = s[0:k+1]
        s_mid = '{' + j[1:] + '}'
        s_after = s[k+len(j):]
        s = s_before + s_mid + s_after
    return s

def findnth(haystack, needle, n):
    parts= haystack.split(needle, n+1)
    if len(parts)<=n+1:
        return -1
    return len(haystack)-len(parts[-1])-len(needle)

def find_nth(haystack, needle, n):
    start = haystack.find(needle)
    while start >= 0 and n > 1:
        start = haystack.find(needle, start+len(needle))
        n -= 1
    return start

def impliedMultiplication(s):
    for k in range(0,s.count(' ')+1):
        f = find_nth(s,' ',k)
        t_left = ('+','/','-','(','[','=','*')
        t_right = ('+','/','-',')',']','=','*')
        TFL = not s[0:f].strip(' ').endswith(t_left)
        TFR = not s[f+1:].strip(' ').startswith(t_right)
        if all([TFL,TFR]):
            s = s[0:f] + '* ' + s[f+1:]
    return s
    
def fractions(s):
    # any case
    t_left = ['+','-','/','*','\n','[','{','(','=']
    t_right = ['+','-','/','*','\n',']','}',')','=']
    r = 0
    for j in range(0,s.count('/')):
        L = find_nth(s,'/',j+1+r)
        p = step(s,'(',')')
        pcb = step(s,'{','}')
        p_0 = p[L]
        pcb_0 = pcb[L]
        # find ending to left
        for k in range(L-1,0,-1):
            if p[k] == p_0 and pcb[k] == pcb_0 and s[k] in t_left:
                l0 = k+1
                break
        try:l0
        except:l0 = 0
        # find ending to right
        for k in range(L+1,len(s)):
            if p[k] == p_0 and pcb[k] == pcb_0 and s[k] in t_right:
                r0 = k
                break
        try:r0
        except:r0 = len(s)
        if r0 == None:
            r0 = len(s)
        s_num = s[l0:L]
        s_denom = s[L+1:r0]
        s_mid = '\\frac{' + s_num + '}{' +  s_denom + '}'
        p_any = step(s,'(',')') + step(s,'[',']') + step(s,'{','}')
        # The exponent in the numerator and denominator
        # will be of the form, e.g, 2/3 when the length
        # of the numerator and denominator are less than n.
        # One exception to this is when the fraction is not
        # contained in parentheses
        n = 1
        if len(s_num) > n or len(s_denom) > n or max(p_any[l0:r0]) == 0:
            if s_denom.startswith('(') and s_denom.endswith(')') and s_num.startswith('(') and s_num.endswith(')'):
                s_numNew = s_num[1:-1]
                s_denomNew = s_denom[1:-1]
                s_mid = '\\frac{' + s_numNew + '}{' +  s_denomNew + '}'
            elif s_denom.startswith('(') and s_denom.endswith(')'):
                s_denomNew = s_denom[1:-1]
                s_mid = '\\frac{' + s_num + '}{' +  s_denomNew + '}'
            elif s_num.startswith('(') and s_num.endswith(')'):
                s_numNew = s_num[1:-1]
                s_mid = '\\frac{' + s_numNew + '}{' +  s_denom + '}'
            s = s[0:l0] + s_mid + s[r0:]
            r = r-1
        r0 = None;l0 = None;
    return s

def isExponent(s,L):
    p_any = step(s,'(',')') + step(s,'[',']') + step(s,'{','}') 
    for k in range(0,max(p_any)):
        L = find_nth(s,'/',k+1)
    return True

    
def g_of_x(s,needle):
    if needle == 'sqrt(':
        removeParen = True
    else:
        removeParen = False
    t_right = ['+','-','/','*','\n','}',')','=']
    r = 0
    for j in range(0,s.count(needle)):
        L = find_nth(s,needle,j+1+r)
        n_len = len(needle)
        p = step(s,'(',')')
        pcb = step(s,'{','}')
        p_0 = p[L]
        pcb_0 = pcb[L]
        # find ending to right
        for k in range(L+n_len,len(s)):
            if p[k] == p_0 and pcb[k] == pcb_0 and s[k] in t_right:
                r0 = k
                break
            try:r0
            except:r0 = len(s)
        if removeParen:
            s_termInside = s[n_len + L:r0-1]
            s_old = needle[0:-1] + '(' + s_termInside + ')'
            r = r - 1
        else:
            s_termInside = s[n_len + L-1:r0]
            s_old = needle[0:-1] + s_termInside
        s_new = '\\' + needle[0:-1] + '{' + s_termInside + '}'
        s = s.replace(s_old,s_new)
        r0 = None
    return s

def removeExcessParenthesis(s):
    return s
def notInsideFrac(s):
    p = step(s,'\\frac{','}')
    TF = max(p) == 0
    return TF
