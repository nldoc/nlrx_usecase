

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

### DEFINE PLOT MARGIN:
pmarg <- 10

#### Plot 1: Fitness function:
fitness <- data.frame(generation=seq(1:length(nl@simdesign@simoutput$mean)), evaluation=nl@simdesign@simoutput$mean)

fitness.plot <- ggplot(fitness, aes(x=generation, y=evaluation)) +
  geom_line(size=1) +
  theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(plot.margin = margin(t = pmarg, r = pmarg, b = pmarg, l = pmarg, unit = "pt")) +
  ylab("evaluation [ticks]")
  

results.summary <- strsplit(summary(results), " ")
results.summary <- tibble(parameter = names(nl@experiment@variables),
                          value = round(nl@simdesign@simoutput$population[nrow(nl@simdesign@simoutput$population), ]), digits = 2)


fitness.best.table <- tableGrob(results.summary, rows=rep("",nrow(results.summary)))

#fitness.plot <- fitness.plot + annotation_custom(fitness.best.table, xmin=50, ymin=2000)

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

# 
# results.best.agg <- results.best %>% dplyr::select(`random-seed`, nl@experiment@metrics)
# names(results.best.agg) <- c("seed", "step", "Foodsource 1", "Foodsource 2", "Foodsource 3")
# results.best.agg <- results.best.agg %>% 
#   gather(foodsource, food, -step, -seed) %>% 
#   dplyr::filter(food > 0)
# 
# ggplot(results.best.agg, aes(x=step, y=food)) +
#   facet_wrap(~foodsource) +
#   geom_line(aes(color=factor(seed)), size=2) + 
#   guides(color="none") +
#   scale_color_jco() +
#   theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
#   theme(panel.spacing = unit(0.5, "lines"))
  

#ggsave("4_Plots/Ants_sobol.png", width = 6.0, height = 4.0, dpi=300)

### Try alternate plot counting finished runs:
results.best.agg <- nl@simdesign@simoutput %>% dplyr::select(`random-seed`, nl@experiment@metrics)
names(results.best.agg) <- c("seed", "step", "1", "2", "3")
results.best.agg <- results.best.agg %>% 
  gather(foodsource, food, -step, -seed) %>% 
  dplyr::filter(food > 0) %>% 
  group_by(seed, foodsource) %>% 
  filter(step == max(step)) %>% 
  arrange(step) %>% 
  ungroup() %>% 
  group_by(foodsource) %>% 
  mutate(id=row_number()) %>% 
  mutate(runs.finished = id / length(nl@simdesign@simseeds) * 100)


validplot <- ggplot(results.best.agg, aes(x=step, y=runs.finished, color=foodsource)) +
  geom_step(size=1) +
  guides(color=guide_legend(title="foodsource")) +
  scale_color_jco() +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="1")$step), lty=2) +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="2")$step), lty=2) +
  geom_vline(xintercept = max(dplyr::filter(results.best.agg, foodsource=="3")$step), lty=2) +
  theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  ylab("runs finished [%]") +
  theme(panel.spacing = unit(0.5, "lines"),
        legend.position = "top",
        plot.margin = margin(t = pmarg, r = pmarg, b = pmarg, l = pmarg, unit = "pt"))

### Cumulative per seed:

results.best.agg <- nl@simdesign@simoutput %>% dplyr::select(`random-seed`, nl@experiment@metrics)
names(results.best.agg) <- c("seed", "step", "food1", "food2", "food3")
results.best.agg <- results.best.agg %>% 
  mutate(foodsum=food1 + food2 + food3) %>% 
  group_by(seed) %>% 
  mutate(maxfood=max(foodsum)) %>% 
  mutate(percentage.eaten=round(((maxfood - foodsum) / maxfood) * 100, digits=3)) %>% 
  mutate(maxstep=max(step))

cumplot <- ggplot(results.best.agg, aes(x=step, y=percentage.eaten, color=factor(seed))) +
  geom_step(size=1) +
  guides(color="none") +
  scale_color_jco() +
  ylab("food gathered [%]") +
  geom_vline(aes(xintercept = maxstep, color=factor(seed)), lty=2) +
  theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(plot.margin = margin(t = pmarg, r = pmarg, b = pmarg, l = pmarg, unit = "pt"))




plots <- list(fitness.plot, fitness.best.table, cumplot, validplot)
lay <- rbind(c(1,2), c(3,4)) 
grid.arrange(grobs=plots, layout_matrix=lay)

ggsave("4_Plots/Ants_genalg.png", width=7, height=6, arrangeGrob(grobs=plots, layout_matrix=lay))  

