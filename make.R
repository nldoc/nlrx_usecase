# R Script to run all use cases

## source globals
source("1_Helper/globals.R")

## run sobol use case
source("2_UseCases/1_Ants_sobolt.R")

## run GenAlg use case
source("2_UseCases/2_Ants_genAlg.R")

## run point pattern analysis use case
source("2_UseCases/3_Ants_spatstat.R")

## run screenshot use case
source("2_UseCases/4_Ants_screenshot.R")

## run benchmark
#source("2_UseCases/5_1_run_benchmarks_unix.R")
#source("2_UseCases/5_1_run_benchmarks_win.R")

## run benchmark plot script
source("2_UseCases/5_2_Plot_Benchmarks.R")

## run nldoc use case
source("2_UseCases/6_nldoc.R")

