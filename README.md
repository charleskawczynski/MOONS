# MOONS

UCLA's Magnetohydrodynamic Object Oriented Solver.
MOONS is a finite difference research code, written in fortran, for studying MHD flows in rectangular coordinate systems.

Here are instructions on how to run a simulation:

1) Run the code generator and makefile generator
	1.1) Navigate to the python_generator/application/ folder and run main.py.
				This will auto-generate a huge portion of the MOONS data structure,
				including constructors, destructors, and print and other routines.
				Running main.py also preps the final makefile. So, main.py must be
				run after adding any new handwritten files to the code (that need compiled).

2) Configure the makefile
	2.1) Navigate to, and open, the makefile in the makefiles/ folder.
	2.1) Configure the path where the simulation will be run, in the variable TARGET_DIR.
	2.1) Configure the release ('DEBUG','OPTIMIZE','PROFILE') and parallelization (all near the top).
	2.1) Choose a cluster (or make your own) with the 'CLUSTER' variable. This variable allows for customized compiler flags depending on the machine being used. The purpose is to allow each user to create customized settings.

3) Configure the TARGET_DIR
	3.1) The TARGET_DIR (set in the makefile) folder must contain 2 files and 1 folder (main.f90, makefile,out/). They can be copied over directly from the out_dir/ folder.

4) Delete the restart file (if it exists, or for a fresh simulation only)
	4.1) If out/LDC does not exist, then skip this step. Navigate to the restart folder (e.g. out/LDC/restart/restart1) & delete the "primitives.dat" file.
				This ensures that the code will not attempt to restart the
				simulation, which is (***AND SHOULD REMAIN***) the default behavior.
				See the below explanation as to why.

5) Configure the simulation parameters
	5.1) Navigate to the code/handwritten/sim_params/sim_params_extend.f90 file and set the desired parameters for the simulation. There should be comments in this file to help assist how the parameters should be set. When a particular case is set, they can be hard-code saved in the saved_input_files/ folder.

6) Build and run MOONS
	6.1) Open a **git shell** window and navigate to the makefiles/ folder in it.
	6.2) Clean the object files and modules with the "make clean" command
	6.3) Build with the "make" command
	6.4) Build and run with the "make run" command

** Windows CMD has, in the past, demonstrated spurrious compilation errors that have not been resolved. The latest conclusion is that, actually, this is a windows command prompt bug, and not related to MOONS nor the makefile. Git Power Shell does not demonstrate this bug with the same exact code and makefile configuration, which supports this hypothesis.

*** The code should always try to restart as the default behavior so that, in the event of a crash, the only required action needed is to double click the MOONS.exe application, which will automatically restart the code. The reason that this should not be changed is that, if the code crashes and restarting is not the default behavior, then a new MOONS.exe must be generated. That means that the .exe must be built by setting up the code for that particular set of simulation parameters, which is a time consuming and tedious task if it has not been looked at for months.
