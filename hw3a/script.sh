#!/bin/bash
#
#SBATCH --mail-user=wthomas37@uchicago.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/home/wthomas37/slurm_hw3a/slurm_out/%j.%N.stdout
#SBATCH --error=/home/wthomas37/slurm_hw3a/slurm_out/%j.%N.stderr
#SBATCH --chdir=/home/wthomas37/slurm_hw3a
#SBATCH --job-name=wthomas37-hw-3a
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=80:00
#SBATCH --partition=general
#SBATCH --exclusive

python3.8 experiments.py