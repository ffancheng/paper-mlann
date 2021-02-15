#!/bin/env bash
#SBATCH --job-name=installR
#SBATCH --time=00:10:00
#SBATCH --mem=1000
#SBATCH --ntasks=1
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=fan.cheng@monash.edu
#SBATCH --output=install_output.txt
# export R_LIBS=~/R/libs
# module load R/3.6.2-mkl
module load gnu8
module load R
R --vanilla < install_packages.R
## or R --vanilla < install_packages.R > install_output.txt
# export R_LIBS=~/R/libs
