

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/NetLogo 6.0.4/app/models/Sample Models/Biology/Ants.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
report_model_parameters(nl)

nl@experiment <- experiment(expname = "ants_genalg",
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



nl@simdesign <- simdesign_GenAlg(nl = nl,
                                 popSize = 200, 
                                 iters = 100, 
                                 evalcrit = 1,
                                 elitism = NA,
                                 mutationChance = NA,
                                 nseeds = 1)


results <- run_nl_dyn(nl = nl,
                      seed = getsim(nl, "simseeds")[1])

# Attach results to nl
setsim(nl, "simoutput") <- results

# Store nl object
saveRDS(nl, "3_Results/Ants_genAlg_nl.rds")



#### Plot 1: Fitness function:
fitness <- data.frame(generation=seq(1:length(results$mean)), evaluation=results$mean)

fitness.plot <- ggplot(fitness, aes(x=generation, y=evaluation)) +
  geom_line(size=1) +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14)) + 
  theme_ipsum(axis_title_size = 14)

results.summary <- strsplit(summary(results), " ")
results.summary <- tibble(parameter = names(nl@experiment@variables),
                          value = round(as.numeric(results.summary[[1]][(length(results.summary[[1]]) - 3):(length(results.summary[[1]]) - 1)]), digits = 2))


fitness.best.table <- tableGrob(results.summary, rows=rep("",nrow(results.summary)))

fitness.plot <- fitness.plot + annotation_custom(fitness.best.table, xmin=50, ymin=2000)

##################################
## Use best found setting to simulate one run of wolf sheep over 2000 ticks:
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/NetLogo 6.0.4/app/models/Sample Models/Biology/Ants.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
report_model_parameters(nl)

nl@experiment <- experiment(expname = "ants_genalg_valid",
                            outpath = "3_Results/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 0,
                            stopcond = "not any? patches with [food > 0]",
                            evalticks = NA_integer_,
                            metrics = c("ticks", 
                                        "sum [food] of patches with [pcolor = cyan]", 
                                        "sum [food] of patches with [pcolor = sky]",
                                        "sum [food] of patches with [pcolor = blue]"),
                            constants = list("population" = results.summary$value[1],
                                             "diffusion-rate" = results.summary$value[2],
                                             "evaporation-rate" = results.summary$value[3]
                            ))

nl@simdesign <- simdesign_simple(nl, nseeds = 10)

## Run
plan(multisession)
results.best <- run_nl_all(nl)
# Attach results to nl
setsim(nl, "simoutput") <- results.best
# Store nl object
saveRDS(nl, "3_Results/Ants_genAlg_valid_nl.rds")


results.best.agg <- results.best %>% dplyr::select(`random-seed`, nl@experiment@metrics)
names(results.best.agg) <- c("seed", "step", "Foodsource 1", "Foodsource 2", "Foodsource 3")
results.best.agg <- results.best.agg %>% 
  gather(foodsource, food, -step, -seed) %>% 
  dplyr::filter(food > 0)

ggplot(results.best.agg, aes(x=step, y=food)) +
  facet_wrap(~foodsource) +
  geom_line(aes(color=factor(seed)), size=2) + 
  guides(color="none") +
  scale_color_jco() +
  theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(panel.spacing = unit(0.5, "lines"))
  

#ggsave("4_Plots/Ants_sobol.png", width = 6.0, height = 4.0, dpi=300)

### Try alternate plot counting finished runs:
results.best.agg <- results.best %>% dplyr::select(`random-seed`, nl@experiment@metrics)
names(results.best.agg) <- c("seed", "step", "Foodsource 1", "Foodsource 2", "Foodsource 3")
results.best.agg <- results.best.agg %>% 
  gather(foodsource, food, -step, -seed) %>% 
  dplyr::filter(food > 0) %>% 
  group_by(seed, foodsource) %>% 
  filter(step == max(step)) %>% 
  arrange(step) %>% 
  ungroup() %>% 
  group_by(foodsource) %>% 
  mutate(id=row_number()) %>% 
  mutate(percentage.finished = id / length(nl@simdesign@simseeds) * 100)


validplot <- ggplot(results.best.agg, aes(x=step, y=percentage.finished, color=foodsource)) +
  geom_step(size=2) +
  guides(color=guide_legend(title="")) +
  scale_color_jco() +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="Foodsource 1")$step), lty=2) +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="Foodsource 2")$step), lty=2) +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="Foodsource 3")$step), lty=2) +
  theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(panel.spacing = unit(0.5, "lines"),
        legend.position = "top")


plots <- list(fitness.plot, validplot)
lay <- rbind(c(1,2)) 
grid.arrange(grobs=plots, layout_matrix=lay)

ggsave("4_Plots/Ants_genalg.png", width=10, height=3, arrangeGrob(grobs=plots, layout_matrix=lay))  



