#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simulation 4: Segregation Model - Spatial usecase
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "1_Helper/NetLogo 6.0.4/",
         modelpath = "1_Helper/NetLogo 6.0.4/app/models/Sample Models/Social Science/Segregation.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "segregation",
                            outpath = outpath,
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "false",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            runtime = 0,
                            stopcond = "all? turtles [ happy? ]",
                            evalticks = NA_integer_,
                            metrics = c("percent-similar", "count turtles with [not happy?]"),
                            metrics.turtles = c("who", "pxcor", "pycor", "color", "happy?", "similar-nearby", "other-nearby", "total-nearby"),
                            variables = list("density" = list(min=50, max=80, step=10),
                                             "%-similar-wanted" = list(min=40, max=70, step=10)))
## Full factorial simdesign:
nl@simdesign <- simdesign_ff(nl = nl, nseeds = 1)

## Run simulations
results <- nlrx::run_nl_all(nl = nl)
setsim(nl, "simoutput") <- results

## Get_nl_spatial
res.sp <- get_nl_spatial(nl, patches = FALSE, turtles = TRUE, turtle_coords = "px", format="spatial")
res.tb <- get_nl_spatial(nl, patches = FALSE, turtles = TRUE, turtle_coords = "px", format="tibble")

## Create list of rasters from turtle point measurements
r.stack <- purrr::map(seq(nrow(res.sp)), function(x) {
  # Select current points sf object
  points <- res.sp$metrics.turtles[[x]]
  # Create raster
  rast <- raster()
  points.extent <- extent(points) # this might be unnecessary 
  extent(rast) <- points.extent
  ncol(rast) <- points.extent@xmax - points.extent@xmin
  nrow(rast) <- points.extent@ymax - points.extent@ymin
  rast <- rasterize(points, rast, points$color, fun=mean)
  return(rast)
})

# Make stack
r.stack <- raster::stack(r.stack)
## Calculate metrics: Patch density
r.stack.pd <- landscapemetrics::lsm_l_pd(r.stack)
## Bind metric to results:
results.select <- results %>% dplyr::select(siminputrow, density, "%-similar-wanted", "percent-similar") %>% dplyr::bind_cols(pd=r.stack.pd$value) %>% dplyr::rename("patch density" = pd)
results.select$`patch density` <- round(results.select$`patch density`, digits=2)

## Use tibble to visualize:
ggplot(res.tb, aes(x=pxcor, y=pycor)) +
  geom_raster(aes(fill=factor(color))) +
  coord_equal() +
  geom_point(data=results.select, aes(x=0, y=0, color=`patch density`), pch=22, size=62, stroke=4) +
  facet_grid(density~`%-similar-wanted`, labeller = "label_both") +
  scale_fill_jco() +
  guides(fill="none") +
  scale_color_viridis_c(option="A") +
  #scale_color_gradient(low="#80796C", high="#AD4047") +
  theme_ipsum(axis_title_size = 14) +
  theme(panel.spacing = unit(0.5, "lines"))

ggsave("4_Plots/segregation.png", width=10, height=8.5, dpi=300)



