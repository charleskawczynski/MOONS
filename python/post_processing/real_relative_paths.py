def get_all_NEM_paths():
	rel = []
	###################### THICK WALL
	###################### High Rem
	rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 400 PV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 400 RV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 1000 PV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 1000 RV\LDC')

	rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 400 PV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 400 RV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 1000 PV\LDC')
	rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 1000 RV\LDC')
	# ###################### Low Rem
	rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 400 PV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 400 RV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 1000 PV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 1000 RV\LDC')

	rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 400 PV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 400 RV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 1000 PV\LDC')
	rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 1000 RV\LDC')
	###################### THIN WALL
	###################### High Rem
	rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 400 PV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 400 RV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 1000 PV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 1000 RV\LDC')

	rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 400 PV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 400 RV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 1000 PV\LDC')
	rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 1000 RV\LDC')
	###################### Low Rem
	rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 400 PV\LDC') # Does not exist yet
	rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 400 RV\LDC') # Does not exist yet
	rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 1000 PV\LDC') # Does not exist yet
	rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 1000 RV\LDC') # Does not exist yet

	rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 400 PV\LDC')
	rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 400 RV\LDC')
	rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 1000 PV\LDC')
	rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 1000 RV\LDC')
