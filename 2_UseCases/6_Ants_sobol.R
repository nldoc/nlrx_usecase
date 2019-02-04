
### ANts model sobol sensitivity analysis:


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
                            tickmetrics = "false",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 0,
                            stopcond = "not any? patches with [food > 0]",
                            evalticks = NA_integer_,
                            metrics = c("ticks"),
                            variables = list("population" = list(min=10, max=200, step=10, qfun="qunif"),
                                             "diffusion-rate" = list(min=1, max=99, step=10, qfun="qunif"),
                                             "evaporation-rate" = list(min=1, max=99, step=10, qfun="qunif")
                            ))

eval_variables_constants(nl)

nl@simdesign <- simdesign_sobol(nl=nl,
                                samples=1000,
                                sobolorder=2,
                                sobolnboot = 100,
                                sobolconf=0.95,
                                nseeds=6,
                                precision = 3)

#nl@simdesign <- simdesign_lhs(nl=nl, samples= 10, nseeds = 1, precision = 3)

## Run parallel
plan(multisession)
results <- nlrx::run_nl_all(nl = nl, split = 2)

# Attach results to nl
setsim(nl, "simoutput") <- results

# Store nl object
saveRDS(nl, "3_Results/Ants_sobol_nl.rds")

# Get morris indices:
sobol.out <- analyze_nl(nl)
sobol.out$facet <- ifelse(sobol.out$parameter %in% c("population", "diffusion-rate", "evaporation-rate"), "main effect", "interaction")
sobol.out$facet <- factor(sobol.out$facet, levels = c("main effect", "interaction"))

ggplot(sobol.out, aes(x=parameter, y=original)) +
  facet_wrap(~facet, scales="free", ncol=1) +
  coord_flip() +
  geom_boxplot()

sobol.out.agg$original.min <- ifelse((sobol.out.agg$original.mu - sobol.out.agg$original.sd) < 0, 0, sobol.out.agg$original.mu - sobol.out.agg$original.sd)
sobol.out.agg$original.max <- sobol.out.agg$original.mu + sobol.out.agg$original.sd

ggplot(sobol.out.agg, aes(x=parameter, y=original.mu)) +
  facet_wrap(~facet, scales="free", ncol=1) +
  coord_flip() +
  geom_linerange(aes(ymin=original.min, ymax=original.max), size=1) +
  geom_point(size=2) +
  theme_ipsum(base_size = 11, axis_text_size = 11, axis_title_size = 11, strip_text_size = 14) +
  theme(panel.spacing = unit(0.5, "lines"))

ggsave("4_Plots/Ants_sobol.png", width = 6.0, height = 4.0, dpi=300)
