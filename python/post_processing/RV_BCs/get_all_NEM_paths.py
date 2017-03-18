def get_all_NEM_paths(PS):
	rel = get_all_NEM_paths_real(PS)
	# rel = get_all_NEM_paths_test(PS)
	return rel

def get_all_NEM_paths_test(PS):
	# rel = get_all_NEM_paths(PS)
	rel = []
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 1000 RV'+PS+'LDC')
	return rel

def get_all_NEM_paths_real(PS):
	rel = []
	# ###################### THICK WALL
	# ###################### High Rem
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 1000 RV'+PS+'LDC')
	# # ###################### Low Rem
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 1000 RV'+PS+'LDC')
	# ###################### THIN WALL
	# ###################### High Rem
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100'+PS+'Re = 1000 RV'+PS+'LDC')
	# ###################### Low Rem
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100'+PS+'Re = 1000 RV'+PS+'LDC')
	return rel

def get_all_Rem_paths(PS):
	rel = []
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 1'  +PS+'LDC')
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 20' +PS+'LDC')
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 40' +PS+'LDC')
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 60' +PS+'LDC')
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 80' +PS+'LDC')
	rel.append('Rem_study'+PS+'PV'+PS+'Rem = 100'+PS+'LDC')

	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 1'  +PS+'LDC')
	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 20' +PS+'LDC')
	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 40' +PS+'LDC')
	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 60' +PS+'LDC')
	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 80' +PS+'LDC')
	rel.append('Rem_study'+PS+'RV'+PS+'Rem = 100'+PS+'LDC')

	return rel
