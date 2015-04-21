[33mcommit 7d9d8bb273385bf14b85c57f4d8d4848decb4fb8[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Apr 2 08:38:45 2015 -0700

    Updated documentation for multigrid.
    
    Finished benchmarking LDC cases.
    
    Working on benchmarking duct flow cases
    	Right now there is an asymmetry introduced from SOR
    	Converting the SOR to Jacobi removes the introduced asymmetry,
    	So it's probably not the BCs.
    	The current version will be compared with the old version
    
    A checkSymmetry routine was added to delOps.f90
    
    Once the purely hydrodynamic flow is working well, run
    	Hunt's Flow
    	Shercliff's Flow

[33mcommit 379ebc865a505b46fb8a7c7007eaa3ead5c93859[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Mon Mar 30 18:54:03 2015 -0700

    Fixed the latest "bug". I didn't realize that the uniform grid case
    that I was trying to run was using a slightly non-uniform grid.
    
    In the future, be careful how the geometry is defined since it is
    in a transition from the old routines.
    
    Some of the MG routines were modified
    	The MG method seems to work okay for node data
    	The restriction operator was written for cc data
    
    A few if statements were added to griddata.f90 to make uniform 3D cavities.

[33mcommit 4862ecf8ebf5d9dd2832fbc4ef42eebca671353b[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Sun Mar 29 16:16:50 2015 -0700

    need to checkout master to compare SOR

[33mcommit a6270df73db82255db30ab6148459baa524cb416[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 27 15:35:22 2015 -0700

    The poisson.f90 file was added to the unit testing folder. It solves the poisson equation
    for the following cases:
    	Dirichlet
    		Node data
    			SOR
    			ADI
    			MG
    		CC data
    			SOR
    			ADI
    	Neumann
    		Node data
    			SOR
    			ADI
    			MG
    		CC data
    			SOR
    			ADI
    
    The only parameters that need adjusting are modelProblem in modelProblem_mod, which controls
    the source term, boundary conditions, the exact solution etc., and s in the main program,
    which controls whether the data is cell corner or cell center. If these two parameters disagree
    an error is recieved.
    
    This is a milestone, since the residuals of all tested problems go to zero within machine accuracy.
    It should be clear from the setup in this file how source terms should be constructed.
    
    There is still a little ambiguity about whether the BCs should be applied before or after taking the
    mean of the source term. Although this may affect convergence rates, residuals were observed to reach
    zero within machine accuracy for more than one setup.

[33mcommit 6c51b31687f4c4a6ec939225d64e1c9a2adb510f[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 27 09:12:35 2015 -0700

    Still developing the poisson testing module to compare
    	Gauss-Seidel / SOR
    	ADI
    	MG
    
    The residuals for the first two have dropped to machine accuracy in the past
    for
    	Dirichlet
    		N data
    		CC data
    	Neumann
    		N data
    		CC data
    
    Once all methods have been tested for all BCs and grids, non-uniform grids
    will be tested, and then non-uniform coefficients.
    
    Once this is finished, the cleaning procedure will be tested with the best
    candidate.

[33mcommit fa17cdc42612598adac083c30e8a7cc869fd074a[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Sun Mar 22 15:10:37 2015 -0700

    Got the duct flow working for purely hydrodynamic flow with a ghost node.
    
    Added some new documentation and deleted some old documentation.
    
    Next:
    	try MHD duct flow (should be fine)
    	get non-uniform grids working
    	test all previous cases with non-uniform grids
    	test 3D neumann ADI for canonical problem

[33mcommit d066d76482ef5aafeb3f70d1fd56e13b136548ae[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Sat Mar 21 14:55:13 2015 -0700

    Just updated SOR documentation and deleted old MS word doc.

[33mcommit 61b307d446f0c56ce1358acf090f8f119f605de2[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 20 16:44:07 2015 -0700

    made some minor tweaks. Now ghostNode runs the same as master for
    uniform grids with Dirichlet BCs.

[33mcommit 74ac2d13659d7341143d2443fc193bed666995cc[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 20 13:19:24 2015 -0700

    fixed momentum counter for solving ppe from > 1 to > 0

[33mcommit 2c0cb035f96d9e11b950971ad5a319c4f22ca858[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 20 13:16:53 2015 -0700

    added some debugging write statements into momentum solver

[33mcommit ea7ab3a556c0645857304f7404301b9f47479ba3[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Mar 20 11:56:39 2015 -0700

    Made a branch with a ghost node, trying to run original master to compare some things.

[33mcommit eed92e019a08e3e7fb91a7ebfa3ab6caef8b45a5[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Mar 19 12:29:29 2015 -0700

    Documentation:
    A lot of new documentation has been added with LaTex, and a lot
    of old documentation has been removed from microsoft word. Much work
    is still needed with this.
    
    Code:
    The myDel.f90 dependency has been removed, and myDel.f90 has been deleted,
    so now del.f90 is used.
    
    gridGenTools.f90 was implemented and their corresponding routines have
    been removed from griddata.f90.]
    
    diffType is now computed internally in del.f90, and no longer exists externally.
    gridType is now computed internally in interp.f90, and no longer exists externally.
    
    The OO matlab scripts have been developed for the 1D relaxation as a proof of concept
    for the 3D ADI.
    
    The 3D ADI is still broken for Neumann BCs.
    
    Next:
    The ghost node is about to be introduced which will require changing.
    	stencils.f90
    	del.f90
    	interp.f90
    	applyBCs.f90
    	coordinates.f90
    Test cases will be run to make sure nothing is broken after and that
    results are consistent with earlier versions. Cases to test include
    	LDC, duct flow (for neumann BCs)
    
    Finally, the MG will be developed for CC data and the projection cleaning
    procedure will be investigated,implemented and tested to ensure this can
    be used alongside the 3D ADI for insulating cases.

[33mcommit 2780f17bdf4534bcbf9c909ef13bb2363102fb8e[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Mar 11 13:52:14 2015 -0700

    Implemented MG for node-based data.
    Replaced the del.f90 with del_eliminated_diffType.f90
    Added stencils.f90
    
    Added some Neumann routines to the ADI, still not finished though,
    It will require modifying applyBCs or some routine to adjust the RHS.
    
    Modified BCs to have a grid module, but I didn't remove the coordinates yet.
    
    Deleted some files (old files from MG)

[33mcommit 7b3d8facddb308e7ad2516a5fed67bc0fedc0192[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Tue Mar 3 15:34:21 2015 -0800

    Deleted some files.

[33mcommit 3f9588b3a12b676094ff1fdf734d0c9d8d99756d[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Tue Mar 3 15:32:01 2015 -0800

    Continued removing dependence on griddata.
    Added some analysis tools in matlab.
    Added some documentation for 3D amplification factor.
    Made a del.f90 for different operation types (+-*/)

[33mcommit faf6d79620030e4e0f37f7246da668783d33ee23[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 25 18:35:39 2015 -0800

    Just finished implementing grid_mod instead of griddata_mod.
    Ran BMC 100,102. Results look very similar to past cases.
    Some differences may be due to including real(,cp) to more
    values than before. Re-named a handful of initialize__ to init__.
    
    Next step is to pull use griddata_mod from as many modules as possible
    and change griddata_mod to gridGen_mod (naming/printing/exporting)

[33mcommit b82d1d7265b8087308e8bb56507dfbfa5618c737[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 25 00:06:41 2015 -0800

    Added the coordinates and grid class with restrict routines for MG.
    3D ADI has been tested for 3D cosine modes.
    About to try to remove griddata from all routines inside MHDSolver.
    Added documentation.

[33mcommit 51ecc04b0d6b9ad9c1ef3371c1fb756c31043667[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Fri Feb 20 14:54:32 2015 -0800

    Updating before I try some new things with the 3D ADI

[33mcommit ce192638de46986a3bc8319240294ca7d8f4515e[m
Merge: d40fc09 e287fc8
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Mon Feb 16 18:05:47 2015 -0800

    Merge branch 'master' of https://github.com/charliekawczynski/MOONS

[33mcommit d40fc09e51e83150385d2955bbafc8b2c18db659[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Mon Feb 16 18:00:14 2015 -0800

    fixed the closeTransientUnits (now they close at the end of the sim).

[33mcommit e287fc8f3db27addb45623b63361b3ddb3eac5bd[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Mon Feb 16 18:00:14 2015 -0800

    fixed the closeTransientUnits (now they close at the end of the sim).

[33mcommit 5510921407183bebe08428c03e04d9489447c30e[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Mon Feb 16 17:23:13 2015 -0800

    Finished implementing, and testing, the transient probe modules.
    These modules output a separate file for coordinates so that they
    are not exported at every step.

[33mcommit 7fcdffdc9587cd31a7666e12eb8422e02f86cf2c[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 21:56:37 2015 -0800

    removed all warnings associated with select case

[33mcommit 2a97bbc554757c7f490e017f61f15063fc5ea5fb[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 21:47:29 2015 -0800

    re-added .gitignore

[33mcommit fded792ebcec1ac29e42122e362b46d22f39df68[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 16:27:07 2015 -0800

    fixed griddata and rundata export

[33mcommit f85f91fcf5adc2fff284e615941e81345521fa5a[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 16:26:04 2015 -0800

    Fixed the griddata and rundata export. Currently running BMC_106

[33mcommit b9d576d8b198291d788684e01a895a30c19fc1e1[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 11:41:55 2015 -0800

    Now exporting the grid and rundata, removed the useOpenMP logical,
    adjusted BMCs for multi-material domain to compare with HIMAG

[33mcommit 931e363a0aaee80f188518715401231b05f46bb1[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Thu Feb 12 08:30:54 2015 -0800

    not sure why this is not allowing me to git add .

[33mcommit 72455217ef47d110eb80bdc3756212b4b5a59cf2[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 15:34:36 2015 -0800

    just added from new branch

[33mcommit 6491b8b40f4abaea504566183ebb05efa96cdcdd[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 15:19:21 2015 -0800

    changed the readme again

[33mcommit 4d5a8379464ad4b4fb804c8eec8e6d0beb334c73[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 15:17:33 2015 -0800

    Just changed the readme (still testing git)

[33mcommit da89087daa33656cdedc4af8c11232c1e1eacbdd[m
Merge: d6b2d23 f81f96c
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:51:46 2015 -0800

    Merge branch 'master' of https://github.com/charliekawczynski/MOONS

[33mcommit d6b2d23399b575b919062ba1448fa58480abeaf2[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:50:57 2015 -0800

    Tryin to commit again, last one didn't seem to work

[33mcommit d6f36762b3dcf8305d7599daca499de4805f3701[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:46:49 2015 -0800

    Just committed some changes, want to see the outcome

[33mcommit f05e31db7af9c7e7cf2cf84529c54c9873bf9aa8[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:33:08 2015 -0800

    just added documentation before commiting

[33mcommit f81f96c2d517c93db97c3ed1a3712c56d2c25e1e[m
Author: Charlie Kawczynski <charliekawczynski@gmail.com>
Date:   Wed Feb 11 14:27:37 2015 -0800

    Create README.md

[33mcommit eeb5c9664921151efc0e092d5a287a55ac92063b[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:25:24 2015 -0800

    Second try

[33mcommit d79fd8c87cc0ee9c406d70f87c248558fb365376[m
Author: Charlie Kawczynski <CharlieKawczynski@gmail.com>
Date:   Wed Feb 11 14:05:36 2015 -0800

    First time commit
