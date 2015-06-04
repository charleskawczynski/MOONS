# -*- coding: utf-8 -*-
"""
Created on Wed Mar 12 14:55:02 2014

@author: charlie
"""
import prettyPy as pp

s = {}
s[0] = 'f_{i-1} == alpha_{i+1}*{f_i}\'^{2}/2 + alpha_{i+2}*{f_i}\'^{3}/6 + alpha_{i+3}*{f_i}\'^{4}/24 + alpha_{i+4}*{f_i}\'^{5}/120 + alpha_{i-1}*{f_i}\'^{0} + alpha_{i}*{f_i}\'^{1}'

for j in s:
    print '******************'
    print s[j]
    print '------------------'
    print pp.pretty(s[j])
    print '******************'

