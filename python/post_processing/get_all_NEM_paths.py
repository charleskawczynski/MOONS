def get_all_NEM_paths(PS):
	rel = []
	# ###################### THICK WALL
	# ###################### High Rem
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20\Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20\Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 20\Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100\Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100\Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 100'+PS+'Ha = 100\Re = 1000 RV'+PS+'LDC')
	# # ###################### Low Rem
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100\Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100\Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.5'+PS+'Rem = 1'+PS+'Ha = 100\Re = 1000 RV'+PS+'LDC')
	# ###################### THIN WALL
	# ###################### High Rem
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20\Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20\Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 20\Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100\Re = 400 RV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100\Re = 1000 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 100'+PS+'Ha = 100\Re = 1000 RV'+PS+'LDC')
	# ###################### Low Rem
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 400 RV'+PS+'LDC')
	# rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 PV'+PS+'LDC')
	# rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 20'+PS+'Re = 1000 RV'+PS+'LDC')

	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100\Re = 400 PV'+PS+'LDC')
	rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100\Re = 400 RV'+PS+'LDC')
	# rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100\Re = 1000 PV'+PS+'LDC')
	# rel.append('tw = 0.05'+PS+'Rem = 1'+PS+'Ha = 100\Re = 1000 RV'+PS+'LDC')
	return rel
