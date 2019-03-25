
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Benchmark to test RAM demand for nlrx and RNetLogo:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prerequirements, both approaches:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# TODO: Define sysname
set.seed(56474)
sysname <- "desktop_jan"

# Define number of seeds, runs and runtime:
nseeds <- 8

nlpath <- "C:/Program Files/NetLogo 6.0.4"
modelpath <- "D:/owncloud/Rdev/nlrx_manuscript/nlrx_usecase/nlrx_usecase/1_Helper/Ants_nlrx.nlogo"
javapath <- "C:/Program Files/Java/jdk1.8.0_171"
jvmmem <- 4  ##gb

## NetLogo headless.bat/sh for BehaviorSpace benchmark:
nlbat <- "D:/owncloud/Rdev/nlrx_manuscript/nlrx_usecase/nlrx_usecase/1_Helper/NetLogo 6.0.4/netlogo-headless.bat"
bsout <- "D:/owncloud/Rdev/nlrx_manuscript/nlrx_usecase/nlrx_usecase/3_Results/benchmark/temp_bs_ants.csv"
# Initialize tibble to collect RAM demand data:
mem <- tibble::tibble()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Store system Information:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(devtools)
library(benchmarkme)
sink(paste0("3_Results/benchmark/nlrx_benchmark_ants_", sysname, ".txt"))
devtools::session_info()
cat("\n")
cat("- CPU -----------------------------------------------------------------------------\n")
cat(paste0(names(benchmarkme::get_cpu()[1]), ": ", benchmarkme::get_cpu()[1]), "\n")
cat(paste0(names(benchmarkme::get_cpu()[2]), ": ", benchmarkme::get_cpu()[2]), "\n")
cat(paste0(names(benchmarkme::get_cpu()[3]), ": ", benchmarkme::get_cpu()[3]), "\n")
cat("\n")
cat("- RAM -----------------------------------------------------------------------------\n")
cat(paste0("System RAM: ", utils:::format.object_size(benchmarkme::get_ram(), units="GB"), "\n"))
sink()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run nlrx benchmark:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(future)
library(nlrx)

nl <- nl(nlversion = "6.0.4",
         nlpath = nlpath,
         modelpath = modelpath,
         jvmmem = (jvmmem * 1024))


nl@experiment <- experiment(expname = "ants_benchmark",
                            outpath = "3_Results/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "false",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 0,
                            stopcond = "not any? patches with [food > 0]",
                            evalticks = NA_integer_,
                            metrics = c("ticks"),
                            variables = list("population" = list(values=c(50, 100, 150, 200)),
                                             "diffusion-rate" = list(values=c(10, 50, 90)),
                                             "evaporation-rate" = list(values=c(10, 50, 90))
                            ))


nl@simdesign <- simdesign_ff(nl=nl,
                              nseeds=nseeds)

## Run parallel
plan(sequential)
plan(multiprocess)
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=1, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
results <- nlrx::run_nl_all(nl = nl,
                            split = 1)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=2, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=3, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run RNetLogo benchmark:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sys.setenv(JAVA_HOME=javapath) 
options(java.parameters = paste0("-Xmx", jvmmem, "g"))

## PARALLELE PROCESSING NETLOGO INITIALIZATION FUNCTION:
prepro <- function(gui, nl.path, model.path) {
  NLStart(nl.path=nl.path, gui=gui, nl.jarname = "netlogo-6.0.4.jar")
  NLLoadModel(model.path=model.path)
}
postpro <- function() {
  NLQuit()
}
simfun <- function(nl, nlpath, modelpath, seed, siminputrow) {
  
  prepro(gui=FALSE, nl.path=file.path(nl@nlpath, "app"), model.path=nl@modelpath)
  model.seed <- seed
  parameter.names <- names(nl@simdesign@siminput)
  param.set <- nl@simdesign@siminput[siminputrow,]
  
  
  eval.values <- NULL
  NLCommand(paste0("random-seed ", model.seed))
  
  ### SET ALL PARAMETERS:
  lapply(seq(1:length(parameter.names)), function(x) {NLCommand(paste0("set ", parameter.names[x], " ", param.set[x]))})
  
  NLCommand(nl@experiment@idsetup)
  
  var.names <- c("behaviorspace-run-number", names(nl@experiment@variables), "ticks", nl@experiment@metrics)
  
  NLDoCommandWhile("any? patches with [food > 0]", nl@experiment@idgo)
  
  cal.crit <- as.data.frame(NLReport(var.names))
  colnames(cal.crit) <- var.names
  
  ## add seed and siminputrow:
  cal.crit$rndseed <- model.seed
  cal.crit$siminputrow <- siminputrow
  
  eval.values <- rbind(eval.values, cal.crit)
  
  postpro()
  
  return(eval.values)
}
run_nl_all_rnetlogo <- function(nl, split = 1)
{
  
  siminput_nrow <- nrow(getsim(nl, "siminput"))
  ## Check if split parameter is valid:
  if (siminput_nrow %% split != 0) {
    stop(
      "Modulo of split parameter and number of rows of the siminput matrix is
      not 0. Please adjust split parameter to a valid value!",
      call. = FALSE
    )
  }
  
  ## Calculate size of one part:
  n_per_part <- siminput_nrow / split
  ## Generate job ids from seeds and parts:
  jobs <- as.list(expand.grid(getsim(nl, "simseeds"), seq(1:split)))
  
  
  ## Execute on remote location
  nl_results <- furrr::future_map_dfr(
    seq_along(jobs[[1]]),
    function(job) {
      ## Extract current seed and part from job id:
      job_seed <- jobs[[1]][[job]]
      job_part <- jobs[[2]][[job]]
      
      ## Calculate rowids of the current part:
      rowids <-
        seq(1:n_per_part) +
        (job_part - 1) * n_per_part
      
      ## Start inner loop to run model simulations:
      furrr::future_map_dfr(
        rowids,
        function(siminputrow) {
          simfun(
            nl = nl,
            seed = job_seed,
            siminputrow = siminputrow
          )
        }
      )
    }
  )
  
  return(nl_results)
}

## EXECUTE:
plan(sequential)
plan(multiprocess)
library(RNetLogo)
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos=1, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
results_rnetlogo <- run_nl_all_rnetlogo(nl=nl, split=1)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos=2, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos=3, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run Behavior Space benchmark:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="BehaviorSpace", pos=1, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
# Call BS:
NLcall <- paste0("\"", nlbat, "\"", " --model ", "\"",
                 modelpath, "\"", " --experiment ", "nlrx_benchmark",
                 " --table ", "\"", bsout, "\"", " --threads ", nseeds)
system(NLcall, wait = TRUE)
# Read data:
results_bs <- readr::read_csv(bsout, skip = 6)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="BehaviorSpace", pos=2, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="BehaviorSpace", pos=3, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Store benchmark results:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add sysname to mem for reference:
mem$sysname <- sysname
saveRDS(mem, paste0("3_Results/benchmark/nlrx_benchmark_ants_", sysname, ".rds"))


