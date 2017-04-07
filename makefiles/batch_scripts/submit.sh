#### sumit.sh START #####
#!/bin/bash
#$ -cwd
# error = Merged with joblog
#$ -o MOONS.exe.joblog.$JOB_ID
#$ -j y
#$ -l h_data=1G,h_rt=8:00:00
#$ -N MOONS
# Email address to notify
#$ -M $USER@mail
# Notify at beginning and end of job
# #$ -m n # for never
#$ -m bea

echo ""
echo "MOONS.exe started on: "` hostname -s `
echo "MOONS.exe started at: "` date `
echo ""

#
# Run the user program
#

. /u/local/Modules/default/init/modules.sh
module load intel/13.cs > /dev/null 2>&1
module load gcc.4.9.3 > /dev/null 2>&1
#
echo ""
/usr/bin/time ./MOONS.exe > ./MOONS.exe.output.$JOB_ID 2>&1
#
echo ""
echo "MOONS.exe finished at: "` date `

#### sumit.sh END ##### Then (just for the first time you created the file) issue: