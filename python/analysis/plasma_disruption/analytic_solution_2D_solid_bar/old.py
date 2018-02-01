# def get_B_from_t(t_all,B_all,t)
# 	n_data_points = 58
# 	i_start = 8
# 	n = n_data_points
# 	B = B_all[0]
# 	for i in range(0,n-1)
# 		if ((t >= t_all[i]) and (t <= t_all[i+1])):
# 			B = interp_simple(B_all[i],t_all[i],B_all[i+1],t_all[i+1],t)
# 	if (t >= t_all[n]) B = B_all[n]
# 	return B
