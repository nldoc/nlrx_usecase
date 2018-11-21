
#### RUN LHS once with nlrx and once with RNetLogo:
mem <- tibble::tibble()


### NLRX:
library(nlrx)
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/Wolf Sheep Predation_nlrx.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
load_model_parameters(nl)
eval_variables_constants(nl)

nl@experiment <- experiment(expname = "eFast",
                            outpath = "3_Results/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 5,
                            evalticks = seq(1,5),
                            metrics = c("r.sheep", "r.wolves", "r.sheep.prop", "r.wolves.prop", "r.sheep.age", "r.wolves.age"),
                            variables = list("initial-number-sheep" = list(min=50, max=150, step=10, qfun="qunif"),
                                             "initial-number-wolves" = list(min=50, max=150, step=10, qfun="qunif"),
                                             "grass-regrowth-time" = list(min=0, max=100, step=10, qfun="qunif"),
                                             "sheep-gain-from-food" = list(min=0, max=50, step=10, qfun="qunif"),
                                             "wolf-gain-from-food" = list(min=0, max=100, step=10, qfun="qunif"),
                                             "sheep-reproduce" = list(min=0, max=20, step=5, qfun="qunif"),
                                             "wolf-reproduce" = list(min=0, max=20, step=5, qfun="qunif")
                            ),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "show-energy?" = "false")
)

nl@simdesign <- simdesign_lhs(nl=nl,
                              samples=3,
                              nseeds=1)

## Run parallel
#plan(multisession)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos="pkgload", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
results <- nlrx::run_nl_all(nl = nl,
                            split = 1,
                            cleanup = "all")
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos="final", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))



####################################################################################
#### RNETLOGO:

## Wolf Sheep eFast, with RNetLogo:
Sys.setenv(JAVA_HOME="1_Helper/NetLogo 6.0.4/runtime") 
options(java.parameters = "-Xmx1g")

## PARALLELE PROCESSING NETLOGO INITIALIZATION FUNCTION:
prepro <- function(dummy, gui, nl.path, model.path) {
  library(RNetLogo)
  NLStart(nl.path, gui=gui)
  NLLoadModel(model.path)
}

postpro <- function() {
  NLQuit()
}

simfun <- function(nl, seed, siminputrow) {
  
  
  model.seed <- seed
  parameter.names <- names(nl@simdesign@siminput)
  param.set <- nl@simdesign@siminput[siminputrow,]
  var.names <- nl@experiment@metrics
  
  eval.values <- NULL
  NLCommand(paste0("set rnd-seed ", model.seed))
  
  ### SET ALL PARAMETERS:
  lapply(seq(1:length(parameter.names)), function(x) {NLCommand(paste0("set ", parameter.names[x], " ", param.set[x]))})
  
  NLCommand(nl@experiment@idsetup)
  
  cal.crit <- NLDoReport(nl@experiment@runtime, nl@experiment@idgo, var.names, as.data.frame=T, df.col.names=var.names)
  eval.values <- rbind(eval.values, cal.crit)
  
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



###### EXECUTE:
## Load required packages:
library(RNetLogo)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos="pkgload", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))

prepro(gui=FALSE, nl.path=nl@nlpath, model.path=nl@modelpath)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos="prepro", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))

results_rnetlogo <- run_nl_all_rnetlogo(nl=nl, split=1)
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos="runnl", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))

postpro()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="RNetLogo", pos="postpro", mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))



### Memory:

mem <- tibble()
