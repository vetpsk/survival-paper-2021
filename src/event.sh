#!/usr/bin/bash
#SBATCH --time=480
#SBATCH --mem=240G
#SBATCH --cpus-per-task=4
#SBATCH --mail-user=p.s.kulkarni@uu.nl
#SBATCH --mail-typ=ALL
source $HOME/.guix-profile/etc/profile
R --vanilla
Rscript get_deviance)model10.R
