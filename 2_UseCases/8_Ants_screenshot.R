

### ANts model sobol sensitivity analysis:

library(nlrx)
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/NetLogo 6.0.4/app/models/Sample Models/Biology/Ants.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "ants",
                            outpath = "3_Results/",
                            repetition = 1,      
                            tickmetrics = "true",
                            idsetup = "setup",   
                            idgo = "go",         
                            runtime = 1000,
                            evalticks = seq(1,1000),
                            metrics.turtles = list("turtles" = c("who", "pxcor", "pycor", "breed")),
                            metrics.patches = c("pxcor", "pycor", "pcolor", "chemical", "food", "food-source-number"),
                            constants = list("population" = 125,
                                             'diffusion-rate' = 50,
                                             'evaporation-rate' = 10))

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


## Step4: Run simulations:
results <- run_nl_all(nl = nl)

## Step5: Attach results to nl and reformat spatial data with get_nl_spatial()
setsim(nl, "simoutput") <- results
nl_spatial <- unnest_simoutput(nl)

nl_spatial <- nl_spatial %>% 
  dplyr::filter(`[step]` %in% seq(0,1000,100))

## Step6: Prepare data for plotting
# Extract infromation on food sources and select maximum step as simulation length:
food_dat <- nl_spatial %>% dplyr::filter(food > 0) %>% dplyr::select(pxcor, pycor, `[step]`)
nmax <- max(food_dat$`[step]`)
food_dat <- food_dat %>% dplyr::filter(`[step]` %in% 1:nmax)
# Extract information on chemicals and apply minimum treshhold for coloring:
chem_dat <- nl_spatial %>% dplyr::filter(`[step]` %in% 1:nmax) %>% dplyr::select(pxcor, pycor, chemical, `[step]`)
chem_dat$chemical <- ifelse(chem_dat$chemical <= 0.2, NA, chem_dat$chemical)
# Extract information on turtle positions:
turt_dat <- nl_spatial %>% dplyr::filter(!is.na(who)) %>% dplyr::filter(`[step]` %in% 1:nmax) %>% dplyr::select(pxcor, pycor, who, `[step]`)
# Create a new data frame to overlay the nest position (in this case the center of the world 0,0)
nest_dat <- food_dat
nest_dat$pxcor <- 0
nest_dat$pycor <- 0

## Step7: Plotting
ggplot(food_dat) +
  geom_tile(data=chem_dat, aes(x=pxcor, y=pycor, fill=sqrt(chemical))) +
  geom_point(data=food_dat, aes(x=pxcor, y=pycor), color="black", shape=15, size=2, alpha=1) +
  geom_text(data=turt_dat, aes(x=pxcor, y=pycor, group=who, color=as.numeric(who)), size=2, alpha=1, label="￥") +
  geom_point(data=nest_dat, aes(x=pxcor, y=pycor), color="brown", fill="white", size=3, stroke=1, shape=21) +
  scale_fill_viridis_c(direction=-1, option="magma", na.value = "white") +
  scale_color_gradient_tableau(palette="Orange") +
  facet_wrap(~`[step]`) +
  guides(fill="none", color="none") +
  coord_equal() +
  theme_void() +
  theme(panel.border = element_rect(color="black", fill=NA))

ggsave("4_Plots/Ants_spatial.png", width = 4.0, height = 4.0, dpi=300)
