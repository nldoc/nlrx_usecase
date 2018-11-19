#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 1: eFAST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

nl@simdesign <- simdesign_eFast(nl=nl,
                                samples=1000,
                                nseeds=5)

## Run parallel
plan(multisession)
results <- nlrx::run_nl_all(nl = nl,
                            split = 1,
                            cleanup = "all")

# Attach results to nl
setsim(nl, "simoutput") <- results

# Store nl object
saveRDS(nl, "3_Results/wolfsheep_eFast_nl.rds")

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
        legend.position="top")+ 
  theme_ipsum(axis_title_size = 14)


ggsave("4_Plots/wolfsheep_eFast.png", width = 12.0, height = 10.0, dpi=300)







