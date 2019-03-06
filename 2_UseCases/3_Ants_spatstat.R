library(nlrx)
library(spatstat)
library(ggplot2)
library(dplyr)
library(tidyr)

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/NetLogo 6.0.4/app/models/Sample Models/Biology/Ants.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
report_model_parameters(nl)

nl@experiment <- experiment(expname = "ants_sobol",
                            outpath = "3_Results/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 0,
                            stopcond = "not any? patches with [food > 0]",
                            evalticks = 1:20000,
                            metrics = c("ticks"),
                            metrics.turtles = c("who", "pxcor", "pycor", "breed", "color"),
                            constants = list("population" = 125,
                                             'diffusion-rate' = 50,
                                             'evaporation-rate' = 10)
                            )

eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


## Step4: Run simulations:
results <- run_nl_all(nl = nl)



## Step5: Attach results to nl and reformat spatial data with get_nl_spatial()
setsim(nl, "simoutput") <- results
nl_spatial <- get_nl_spatial(nl, format = "tibble", patches = FALSE)

col_ind <- nl_spatial %>%
  group_by(step) %>% 
  summarise(count = n_distinct(color)) %>% 
  filter(count == 2) %>% 
  pull(step)

nl_spatial <- nl_spatial  %>% filter(step %in% col_ind)


color_scale <- c("#440154FF",
                 "#238A8DFF",
                 "#FDE725FF")
ants_list <- list()
j <- 1
for(i in unique(nl_spatial$step)) {
  
  nl_sp_step <- nl_spatial %>% dplyr::filter(step == i)
  
  ants_pp <-
    ppp(
      nl_sp_step$pxcor,
      nl_sp_step$pycor,
      c(-35, 35),
      c(-35, 35),
      marks = factor(nl_sp_step$color)
    )
  labels = c("clustering", "randomness", "segregation")
  
  p <-
    spatstat::envelope(
      ants_pp,
      fun = spatstat::pcfcross,
      funargs = list(i = "26", j = "15"),
      nsim = 199,
      verbose = FALSE
    )
  
  data <- tibble::as_tibble(p)
  names(data) <- c("r", "obs", "theo", "lo", "hi")
  data <- data[stats::complete.cases(data),]
  
  data <-
    dplyr::mutate(data,
                  type = dplyr::case_when(obs > hi ~ labels[1],
                                          obs >=  lo &
                                            obs <=  hi ~ labels[2],
                                          obs < lo ~ labels[3]))
  
  data <- dplyr::filter(data,!is.na(type))
  
  data$step <- i
  
  ants_list[[j]] <- data
  j <- j + 1
  cat(i)
}

ants_quantum <- dplyr::bind_rows(ants_list)

ants_quantum <- ants_quantum %>%
  tidyr::complete(step, tidyr::crossing(r))


ggplot(ants_quantum) +
  geom_tile(aes(r, step, fill = type)) +
  scale_fill_viridis_d(na.value	= "grey") + 
  theme_bw()

ggsave()