
# Load libraries
library(nlrx)
library(future)
library(furrr)
library(future.batchtools)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)
library(tidyverse)
library(reshape2)
library(viridis)
library(ggsci)
library(ggrepel)
library(ggthemes)
library(RColorBrewer)
fontsize <- 12


#options(future.makeNodePSOCK.rshopts = c("-i", "C:/Users/Jan/ownCloud/Coding/GWDG_HPC_Cluster/puttyprivate.ppk"))
options(future.makeNodePSOCK.rshopts = c("-i", "D:/owncloud/Coding/GWDG_HPC_Cluster/puttyprivate.ppk"))
options(future.makeNodePSOCK.rshcmd = c("plink", "-ssh"))
login <- tweak(remote, workers="gwdu101.gwdg.de", user="jsaleck")

## Define plan for future environment:
bsub <- tweak(batchtools_lsf, template = 'lsf.tmpl',
              resources = list(job.name = 'wolfsheep',
                               log.file = 'wolfsheep.log',
                               queue = 'mpi',
                               walltime = '12:00',   #12:00
                               coremem = 2000,   # mb RAM per core
                               processes = 24))   #8

# Initialize plan:
plan(list(login,
          bsub,
          multisession))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.3",
         nlpath = "/home/uni08/jsaleck/NetLogo_6.0.3/",
         modelpath = "/home/uni08/jsaleck/NetLogo_6.0.3/app/models/wolfsheepnlrx/Wolf Sheep Predation_nlrx.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
load_model_parameters(nl)
eval_variables_constants(nl)

nl@experiment <- experiment(expname = "nlrx_examples",
                            outpath = "C:/Users/Jan/ownCloud/Rdev/nlrx_manuscript/nlrx_paper_analysis/output",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 500,
                            evalticks = seq(200,500),
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 1: Morris Screening
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nl@simdesign <- simdesign_morris(nl=nl,
                                 morristype="oat",
                                 morrislevels=4,
                                 morrisr=1000,
                                 morrisgridjump=2,
                                 nseeds=5)
  
  



## Run on HPC
results %<-% furrr::future_map_dfr(getsim(nl, "simseeds"), function(seed){
  furrr::future_map_dfr(seq_len(nrow(getsim(nl, "siminput"))), function(siminputrow) {
    
    run_nl(nl = nl,
           seed = seed,
           siminputrow = siminputrow,
           cleanup = "all")
  })
})


# Attach results to nl
setsim(nl, "simoutput") <- results

# Store nl object
saveRDS(nl, paste0(getwd(), "/output/wolfsheep_morris_nl.rds"))

# Get morris indices:
morris.out <- analyze_nl(nl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Aggregate the data of the three runs:

## Select the outputs:
unique(morris.out$metric)
out.select <- unique(morris.out$metric)
out.select <- c("r.sheep_mean", "r.sheep.prop_mean", "r.sheep.age_mean", 
                "r.wolves_mean", "r.wolves.prop_mean", "r.wolves.age_mean"
                )

## Mu.starsd:
morris <- subset(morris.out, index != "mu")
morris <- morris %>% spread(index, value) %>% 
                     filter(metric %in% out.select) %>% 
                     group_by(metric, parameter) %>% 
                     summarise(mustar.mu=mean(mustar), mustar.sd=sd(mustar), sigma.mu=mean(sigma), sigma.sd=sd(sigma))
morris$metric <- factor(morris$metric, levels=out.select)


## Ribbons:
ribbon <- NULL
dummy <- NULL
for (i in morris$metric)
{
  morris.i <- morris %>% filter(metric == i)
  max.x <- max(morris.i$mustar.mu) + max(morris.i$mustar.mu) * 0.25
  max.y <- max(morris.i$sigma.mu) + max(morris.i$sigma.mu) * 0.25
  step <- (max.x / 100)
  xrange <- seq(0, max.x, step)
  ymaxrange <- rep(max(max.x, max.y), length(xrange))
  groups <- c("non.linear and/or non.monotonic", "almost.monotonic", "monotonic", "linear")
  ribbon.i <- NULL
  
  for (j in groups)
  {
    slope <- ifelse(j == "non.linear and/or non.monotonic", 1,
                    ifelse(j == "almost.monotonic", 0.5,
                           ifelse(j == "monotonic", 0.1, 0)))
    
    yminrange <- xrange * slope
    ribbon.ij <- data.frame(x=xrange, ymin=yminrange, ymax=ymaxrange, metric=i, index=j)
    ribbon.i <- rbind(ribbon.i, ribbon.ij)
    ## update ymaxrange:
    ymaxrange <- yminrange
  }
  ribbon <- rbind(ribbon, ribbon.i)
  
  ## Output dummy layer for axis limit control:
  dummy.i <- data.frame(metric = i, x=0, y=((max.y * 0.15) * -1))
  dummy <- rbind(dummy, dummy.i)
  
}


##Plot
ggplot(data=morris) +
  facet_wrap(~metric, ncol=3, scales="free") +
  geom_ribbon(data=ribbon, aes(x=x, ymin=ymin, ymax=ymax, fill=index), color="white", alpha=0.3) +
  geom_point(data=morris, aes(x=mustar.mu, y=sigma.mu, color=parameter), size=3) +
  geom_errorbar(data=morris, aes(x=mustar.mu, ymin=sigma.mu-sigma.sd, ymax=sigma.mu+sigma.sd)) +
  geom_errorbarh(data=morris, aes(y=sigma.mu, xmin=mustar.mu-mustar.sd, xmax=mustar.mu+mustar.sd)) +
  geom_text_repel(data=morris, aes(x=mustar.mu, y=sigma.mu, label=parameter, color=parameter), size=4, force=3, max.iter=3000, segment.alpha = 0.5) +
 # geom_blank(data=dummy, aes(x=x, y=y)) +
  expand_limits(y=0, x=0) +
#  scale_x_sqrt() +
#  scale_y_sqrt() +
  scale_color_nejm() +
  scale_fill_grey() +
  guides(color="none") +
  xlab(expression(mu*"* absolute mean of morris elementary effects (sqrt scale)")) +
  ylab(expression(sigma*" standard deviation of morris elementary effects")) +
  theme_minimal() +
  theme(axis.text.x=element_text(size=fontsize, color="black"),
        axis.text.y=element_text(size=fontsize, color="black"),
        axis.title=element_text(size=fontsize, color="black"),
        strip.text=element_text(size=fontsize, color="black"),
        panel.background=element_blank(),
        panel.border=element_rect(colour = "grey", size = 0.5, fill = NA),
         panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(0.5, "lines"),
        plot.background=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=fontsize, color="black"),
        legend.text=element_text(size=fontsize, color="black"),
        legend.key.width = unit(3,"line"),
        plot.margin=margin(2, 2, 2, 2))

ggsave(paste0("wolfsheep_morris_mustar_sigma.png"), path=paste0(getwd(), "/output/"), width = 12.0, height = 10.0, dpi=300)



### Mu.star Mu plot:
morris <- subset(morris.out, index != "sigma")
morris <- morris %>% spread(index, value) %>%
                     filter(metric %in% out.select) %>% 
                     group_by(metric, parameter) %>% 
                     summarise(mustar.mu=mean(mustar), mustar.sd=sd(mustar), mu.mu=mean(mu), mu.sd=sd(mu))
morris$metric <- factor(morris$metric, levels=out.select)




##Plot
ggplot(data=morris) +
  facet_wrap(~metric, ncol=3, scales="free") +
  geom_point(data=morris, aes(x=mustar.mu, y=mu.mu, color=parameter), size=3) +
  geom_errorbar(data=morris, aes(x=mustar.mu, ymin=mu.mu-mu.sd, ymax=mu.mu+mu.sd)) +
  geom_errorbarh(data=morris, aes(y=mu.mu, xmin=mustar.mu-mustar.sd, xmax=mustar.mu+mustar.sd)) +
  geom_text_repel(data=morris, aes(x=mustar.mu, y=mu.mu, label=parameter, color=parameter), size=4, force=2, max.iter=3000, segment.alpha = 0.5) +
  expand_limits(y=0, x=0) +
  scale_color_nejm() +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(intercept = 0, slope=-1) +
  guides(color="none") +
  xlab(expression(mu*"* absolute mean of morris elementary effects")) +
  ylab(expression(mu*" mean of morris elementary effects")) +
  theme_minimal() +
  theme(axis.text.x=element_text(size=fontsize, color="black"),
        axis.text.y=element_text(size=fontsize, color="black"),
        axis.title=element_text(size=fontsize, color="black"),
        strip.text=element_text(size=fontsize, color="black"),
        panel.background=element_blank(),
        panel.border=element_rect(colour = "grey", size = 0.5, fill = NA),
        # panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(1, "lines"),
        plot.background=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=fontsize, color="black"),
        legend.text=element_text(size=fontsize, color="black"),
        legend.key.width = unit(3,"line"),
        plot.margin=margin(2, 2, 2, 2))

ggsave(paste0("wolfsheep_morris_mustar_mu.png"), path=paste0(getwd(), "/output/"), width = 12.0, height = 10.0, dpi=300)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 2: eFAST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nl@simdesign <- simdesign_eFast(nl=nl,
                                samples=1000,
                                nseeds=5)

## Run on HPC
results %<-% furrr::future_map_dfr(getsim(nl, "simseeds"), function(seed){
  furrr::future_map_dfr(seq_len(nrow(getsim(nl, "siminput"))), function(siminputrow) {
    
    run_nl(nl = nl,
           seed = seed,
           siminputrow = siminputrow,
           cleanup = "all")
  })
})


# Attach results to nl
setsim(nl, "simoutput") <- results

# Store nl object
saveRDS(nl, paste0(getwd(), "/output/wolfsheep_eFast_nl.rds"))

#nl <- readRDS(paste0(getwd(), "/output/wolfsheep_eFast_nl.rds"))
# Get morris indices:
eFast.out <- analyze_nl(nl)

# Aggregate:
eFast.short <- eFast.out %>% spread(index, value) %>% 
  group_by(parameter, metric) %>% 
  summarize(first.order.mean=mean(`first-order`), first.order.sd=sd(`first-order`),
            total.mean=mean(total), total.sd=sd(total))

out.select <- c("r.sheep", "r.sheep.prop", "r.sheep.age", 
                "r.wolves", "r.wolves.prop", "r.wolves.age")

eFast.short$metric <- factor(eFast.short$metric, levels=out.select)


ggplot(data=eFast.short) +
  facet_wrap(~metric, scales="free") +
  geom_point(data=eFast.short, aes(x=first.order.mean, y=total.mean, color=parameter), size=3) +
  geom_errorbar(data=eFast.short, aes(x=first.order.mean, ymin=total.mean-total.sd, ymax=total.mean+total.sd)) +
  geom_errorbarh(data=eFast.short, aes(y=total.mean, xmin=first.order.mean-first.order.sd, xmax=first.order.mean+first.order.sd)) +
  geom_text_repel(data=eFast.short, aes(x=first.order.mean, y=total.mean, label=parameter, color=parameter), size=4, force=2, max.iter=3000, segment.alpha = 0.5) +
  scale_color_nejm() +
  guides(color="none") +
  theme_minimal() +
  theme(strip.text=element_text(size=fontsize),
        axis.text=element_text(size=fontsize),
        axis.title=element_text(size=fontsize),
        legend.position="top")


ggsave(paste0("wolfsheep_eFast.png"), path=paste0(getwd(), "/output/"), width = 12.0, height = 10.0, dpi=300)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 3: Genetic Algorithm
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


