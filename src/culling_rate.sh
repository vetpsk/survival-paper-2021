#!/usr/bin/bash
#SBATCH --time=240
#SBATCH --mem=120G
#SBATCH --mail-user=p.s.kulkarni@uu.nl
#SBATCH --mail-typ=ALL
source $HOME/.guix-profile/etc/profile
R --vanilla
Rscript cullingrate.R
