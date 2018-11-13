#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 2: Genetic Algorithm
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.3",
         nlpath = "C:/Program Files/NetLogo 6.0.3/",
         modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/wolfsheepnlrx/Wolf Sheep Predation_nlrx.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

nl@experiment <- experiment(expname = "nlrx_examples",
                            outpath = "C:/Users/Jan/ownCloud/Rdev/nlrx_manuscript/nlrx_paper_analysis/output",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 1000,
                            evalticks = seq(1,1000),
                            metrics = c("fitness", "r.sheep", "r.wolves", "r.sheep.prop", "r.wolves.prop", "r.sheep.age", "r.wolves.age"),
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


nl@simdesign <- simdesign_GenAlg(nl = nl,
                                 popSize = 100,
                                 iters = 10,
                                 evalcrit = 1,
                                 elitism = NA,
                                 mutationChance = NA,
                                 nseeds = 1)


results <- run_nl_dyn(nl = nl,
                      seed = getsim(nl, "simseeds")[1],
                      cleanup = "all")



# Attach results to nl
setsim(nl, "simoutput") <- results
# Store nl object
saveRDS(nl, paste0(getwd(), "/output/wolfsheep_genAlg_nl.rds"))


#### Plot 1: Fitness function:
fitness <- data.frame(generation=seq(1:length(results$mean)), evaluation=results$mean)

fitness.plot <- ggplot(fitness, aes(x=generation, y=evaluation)) +
  geom_line(size=1) +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))

results.summary <- summary(results)
# Best Solution : 122.445671865717 135.564143280499 4.72592818550766 1.47040947340429 32.7673053834587 14.9240314308554 19.3318424234167 \n"

fitness.best <- data.frame(parameter=c("initial-number-sheep", "initial-number-wolves", "grass-regrowth-time",
                                       "sheep-gain-from-food", "wolf-gain-from-food", "sheep-reproduce",
                                       "wolf-reproduce"),
                           value=c(122,136,4.73,1.47,32.77,14.92,19.33))


fitness.best.table <- tableGrob(fitness.best, rows=rep("",nrow(fitness.best)))

## Use best found setting to simulate one run of wolf sheep over 2000 ticks:

nl <- nl(nlversion = "6.0.3",
         nlpath = "C:/Program Files/NetLogo 6.0.3/",
         modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/wolfsheepnlrx/Wolf Sheep Predation_nlrx.nlogo",
         jvmmem = 1024)

nl@experiment <- experiment(expname = "nlrx_examples",
                            outpath = "C:/Users/Jan/ownCloud/Rdev/nlrx_manuscript/nlrx_paper_analysis/output",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 2000,
                            evalticks = seq(1,2000),
                            metrics = c("fitness", "r.sheep", "r.wolves", "r.sheep.prop", "r.wolves.prop", "r.sheep.age", "r.wolves.age"),
                            variables = list(),
                            constants = list("initial-number-sheep" = 122,
                                             "initial-number-wolves" = 136,
                                             "grass-regrowth-time" = 4.73,
                                             "sheep-gain-from-food" = 1.47,
                                             "wolf-gain-from-food" = 32.77,
                                             "sheep-reproduce" = 14.92,
                                             "wolf-reproduce" = 19.33,
                                             "model-version" = "\"sheep-wolves-grass\"",
                                             "show-energy?" = "false"))


nl@simdesign <- simdesign_simple(nl, nseeds = 1)

## Run
results.best <- run_nl_all(nl, cleanup = "all")


individuals.plot <- ggplot(results.best, aes(x=`[step]`)) +
  geom_line(aes(y=r.sheep), color="coral", size=1) +
  geom_line(aes(y=r.wolves), color="black", size=1) +
  geom_smooth(aes(y=r.sheep), color="coral", fill="coral") +
  geom_smooth(aes(y=r.wolves), color="black", fill="black") +
  xlab("step") +
  ylab("individuals") +
  theme_minimal() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14))

library(gridExtra)

plots <- list(fitness.plot, individuals.plot, fitness.best.table)
lay <- rbind(c(1, 3), c(2)) 
grid.arrange(grobs=plots, layout_matrix=lay)
ggsave("output/wolfsheep_genalg.png", width=10, height=5, arrangeGrob(grobs=plots, layout_matrix=lay))  


