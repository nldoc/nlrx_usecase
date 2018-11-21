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
                            repetition = 1,      
                            tickmetrics = "true",
                            idsetup = "setup",   
                            idgo = "go",         
                            idfinal = NA_character_,  
                            idrunnum = NA_character_,
                            runtime = 100,
                            evalticks = seq(1,100),
                            metrics = c("count sheep","count wolves"),
                            metrics.turtles = c("who", "pxcor", "pycor", "breed"),
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             'initial-number-sheep' = 100,
                                             'initial-number-wolves' = 50,
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))

# Attach simdesign simple using only constants
nl@simdesign <- simdesign_simple(nl=nl,
                                 nseeds=1)

# Run simulations and store output in results
results <- run_nl_all(nl = nl, cleanup = "all")

# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Report spatial data as tibble:
results_spatial_tibble <- get_nl_spatial(nl,
                                         turtles = TRUE,
                                         patches = TRUE,
                                         turtle_coords = "px",
                                         format="tibble")
results_spatial_tibble$ticks <- " Ticks"
results_spatial_tibble %>% tidyr::unite(stepticks, step, ticks)


results_spatial_tibble$breed[results_spatial_tibble$breed == "wolves"] <- "Wolves"
results_spatial_tibble$breed[results_spatial_tibble$breed == "sheep"] <- "Sheep"

results_spatial_tibble %>% dplyr::filter(step %in% seq(0,80,10)) %>% 
  ggplot() +
  facet_wrap(~step, ncol=4) +
  coord_equal() +
  geom_tile(aes(x=patches_x, y=patches_y, fill=factor(pcolor))) +
  geom_point(aes(x = pxcor, y = pycor, color = breed), size=2) +
  scale_fill_manual(breaks=c("35", "55"),
                    values = c("35" = "#D9AF6B", "55" = "#68855C"),
                    labels = c("Bare Soil", "Grass")) +
  scale_color_manual(breaks=c("Sheep", "Wolves"), values = c("Sheep" = "beige", "Wolves" = "black")) +
  guides(fill  = guide_legend(title="Land Cover"),
         color = guide_legend(title="Breed")) +
  xlab("Easting") + 
  ylab("Northing") + 
  theme_ipsum(axis_title_size = 14)


ggsave("4_Plots/wolfsheep_spatial.png", width = 12.0, height = 10.0, dpi=300)

