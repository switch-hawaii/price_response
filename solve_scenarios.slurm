#!/bin/sh

## reporting
#SBATCH --error=%A.err
#SBATCH --output=%A.out
# change the following lines to have one pound sign instead of two
# if you want to receive notices about jobs
##SBATCH --mail-type=BEGIN,END,FAIL,REQUEUE,TIME_LIMIT_80
##SBATCH --mail-user=8083489586@vtext.com

#SBATCH --mem-per-cpu=6400

#SBATCH --job-name=demand_scenarios
#SBATCH --partition=community.q
#SBATCH --time=2-00:00:00
#SBATCH --nodes=1
# tasks-per-node can be up to 20
#SBATCH --tasks-per-node=5

srun --unbuffered solve_scenarios.sh
