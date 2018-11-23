
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Benchmark to test RAM demand for nlrx and RNetLogo:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prerequirements, both approaches:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(future)
# Initialize tibble to collect RAM demand data:
mem <- tibble::tibble()

# Define number of seeds, runs and runtime:
nseeds <- 8
runtime <- 500
samples <- 100
nlpath <- "C:/Program Files/NetLogo 6.0.4"
modelpath <- "C:/Program Files/NetLogo 6.0.4/app/models/Wolf Sheep Predation_nlrx.nlogo"
javapath <- "C:/Program Files/Java/jdk1.8.0_171"
jvmmem <- 4  ##gb

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# nlrx:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(nlrx)

nl <- nl(nlversion = "6.0.4",
         nlpath = nlpath,
         modelpath = modelpath,
         jvmmem = (jvmmem * 1024))

nl@experiment <- experiment(expname = "eFast",
                            outpath = "3_Results/",
                            repetition = 1,      
                            tickmetrics = "true",
                            idsetup = "setup",   
                            idgo = "go",         
                            idfinal = NA_character_,
                            runtime = runtime,
                            evalticks = seq(1,runtime),
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
                              samples=samples,
                              nseeds=nseeds)

## Run parallel
plan(sequential)
plan(multiprocess)
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=1, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
results <- nlrx::run_nl_all(nl = nl,
                            split = 1,
                            cleanup = "all")
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=2, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))
gc()
mem <- mem %>% dplyr::bind_rows(tibble::tibble(pkg="nlrx", pos=3, time=Sys.time(), mem=as.numeric(gsub("\r","",gsub("FreePhysicalMemory=","",system('wmic OS get FreePhysicalMemory /Value',intern=TRUE)[3])))/1024/1024))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RNetLogo
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
simfun <- function(nl, seed, siminputrow) {
  
  prepro(gui=FALSE, nl.path="C:/Program Files/NetLogo 6.0.4/app", model.path="C:/Program Files/NetLogo 6.0.4/app/models/Wolf Sheep Predation_nlrx.nlogo")
  model.seed <- seed
  parameter.names <- names(nl@simdesign@siminput)
  param.set <- nl@simdesign@siminput[siminputrow,]
  
  
  eval.values <- NULL
  NLCommand(paste0("random-seed ", model.seed))
  
  ### SET ALL PARAMETERS:
  lapply(seq(1:length(parameter.names)), function(x) {NLCommand(paste0("set ", parameter.names[x], " ", param.set[x]))})
  
  NLCommand(nl@experiment@idsetup)
  
  var.names <- c("behaviorspace-run-number", names(nl@experiment@variables), names(nl@experiment@constants), "ticks", nl@experiment@metrics)
  cal.crit <- NLDoReport(nl@experiment@runtime, nl@experiment@idgo, var.names, as.data.frame=T, df.col.names=var.names)
  
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
# Data postprocessing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Store rawdata:
saveRDS(mem, "3_Results/nlrx_benchmark_memory.rds")

## Pkgs:
library(tidyverse)
library(ggthemes)
## Add names for measurement positions:
mem$posname <- ifelse(mem$pos == 1, "Start", ifelse(mem$pos == 2, "Final", "After gc"))
mem$posname <- factor(mem$posname, levels = c("Start", "Final", "After gc"))

## Calculate time demand:
time <- mem %>% 
  group_by(pkg) %>% 
  filter(pos==max(pos) | pos==min(pos)) %>% 
  mutate(diff = time - lag(time, default = first(time))) %>% 
  dplyr::filter(diff > 0) %>% 
  dplyr::select(pkg, diff)

# Plot time:
ggplot(time, aes(x=pkg, y=diff, fill=pkg)) +
  geom_bar(stat="identity") +
  scale_y_time() +
  scale_fill_solarized() +
  guides(fill="none") +
  xlab("R package") +
  ylab("Execution time") +
  theme_classic() +
  theme(axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=14, color="black"))

ggsave("4_Plots/nlrx_benchmarks_time.png", width=4, height=4)


# Postpro memory:
basemem <- mem %>% filter(pos==1)
memplot <- mem 
memplot$basemem <- rep(basemem$mem, each=3) 
memplot <- memplot %>% mutate(memdiff = mem - basemem)
memplot$pkglabel <- ifelse(memplot$pos==3, memplot$pkg, "")

# Plot Memory:
ggplot(memplot, aes(x=posname, y=memdiff, color=pkg, group=pkg)) +
  geom_point(stat='summary', fun.y=sum, size=5) +
  stat_summary(fun.y=sum, geom="line", size=2) +
  geom_text(aes(label=pkglabel), nudge_y=0.3, size=5) +
  ylab("Free Physical Memory [GB]") +
  xlab("Measurement position") +
  scale_color_solarized() +  
  guides(color="none") +
  theme_classic() +
  theme(axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=14, color="black"))

ggsave("4_Plots/nlrx_benchmarks_memory.png", width=6, height=4, dpi=300)


