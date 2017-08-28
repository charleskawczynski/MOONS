import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'unit_conversion'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('days_per_year',real,priv)
	g.module[m_name].add_prop('seconds_per_second',real,priv)
	g.module[m_name].add_prop('seconds_per_minute',real,priv)
	g.module[m_name].add_prop('seconds_per_hour',real,priv)
	g.module[m_name].add_prop('seconds_per_day',real,priv)
	g.module[m_name].add_prop('seconds_per_year',real,priv)
	g.module[m_name].add_prop('minute_per_seconds',real,priv)
	g.module[m_name].add_prop('hour_per_seconds',real,priv)
	g.module[m_name].add_prop('day_per_seconds',real,priv)
	g.module[m_name].add_prop('year_per_seconds',real,priv)

	m_name = 'clock'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('t_elapsed_computational',real,priv)
	g.module[m_name].add_prop('t_start_computational',real,priv)
	g.module[m_name].add_prop('t_stop_computational',real,priv)
	g.module[m_name].add_prop('t_start',real,priv)
	g.module[m_name].add_prop('t_stop',real,priv)
	g.module[m_name].add_prop('i_start','integer(li)',priv)
	g.module[m_name].add_prop('i_stop','integer(li)',priv)
	g.module[m_name].add_prop('count_rate','integer(li)',priv)

	m_name = 'stop_clock'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['dir','name'],'string',priv)
	g.module[m_name].add_prop('c','clock',priv)
	g.module[m_name].add_prop('uc','unit_conversion',priv)
	g.module[m_name].add_prop('seconds_per_step',real,priv)
	g.module[m_name].add_prop('sim_time_per_sec',real,priv)
	g.module[m_name].add_prop('t_passed',real,priv)
	g.module[m_name].add_prop('estimated_total',real,priv)
	g.module[m_name].add_prop('estimated_remaining',real,priv)
	g.module[m_name].add_prop('percentage_complete',real,priv)
	g.module[m_name].add_prop('percentage_complete_wc',real,priv)
	g.module[m_name].add_prop('t_elapsed',real,priv)
	g.module[m_name].add_prop('frozen_elapsed','logical',priv)
	g.module[m_name].add_prop('un_plot','integer',priv)

	return g