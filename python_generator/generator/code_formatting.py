def break_single_line(s_0):
  s = s_0
  n_max_characters = 70
  result = [s]
  if not type(s_0)==str:
    raise NameError('Bad input to break_single_line')
  if (len(s) >= n_max_characters and not ';' in s_0):
    potential_break_locations = [',']
    # potential_break_locations = [')','(',',']
    PBL = potential_break_locations
    n_spaces = len(s) - len(s.lstrip(' '))
    spaces = n_spaces*' '
    s_max = s[0:n_max_characters]
    if any(x in s_max for x in PBL):
      cutoff = [s_max.rfind(x)+1 for x in PBL]
      cutoff = min([x for x in cutoff if not x==0])
      s_cut = s_max[0:cutoff] + '&'
      s_remain = s[cutoff:]
      if (s_remain=='&'):
        s_remain=''
      result = [s_cut]
      if not (len(s_remain.replace(' ','')) <= 0):
          result = result + break_single_line(spaces + s_remain)
  else:
    result = [s]
  return result

def get_n_leading_white_spaces(L):
  temp = [len(x)-len(x.lstrip()) for x in L if (not x=='') and x]
  if temp:
    if (len(temp)==1):
      if temp[0]==0:
        temp = 0
    else:
      temp = min(temp)
  else:
    temp = 0
  return temp

def indent_lines(L):
  n_leading_white_space = get_n_leading_white_spaces(L)
  L = [x.lstrip() for x in L]
  indent = '  '
  T_up = ('if ','type ','subroutine ','function ','do ')
  T_dn = ('endif','end type','end subroutine','end function','enddo','end module')
  T_unindent = ['else','elseif']
  indent_cumulative = ''
  s_indent = len(indent)
  temp = ['' for x in L]
  for i,x in enumerate(L):
    if x.startswith(T_dn):
      indent_cumulative = indent_cumulative[s_indent:]
    temp[i]=indent_cumulative+temp[i]
    if x.startswith(T_up):
      indent_cumulative = indent_cumulative + indent
  L = [s+x for (s,x) in zip(temp,L)]
  L = [break_single_line(x) for x in L]
  L = [item for sublist in L for item in sublist]
  L = [x[s_indent:] if any([y in x for y in T_unindent]) else x for x in L]
  L = [n_leading_white_space*' '+x if not x=='' else x for x in L]
  return L
