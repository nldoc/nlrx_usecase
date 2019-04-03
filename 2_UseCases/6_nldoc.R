## Create model documentation with nlrx:
modelfiles <- c("https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nlogo",
                "https://raw.githubusercontent.com/nldoc/nldoc_pg/master/WSP.nls")

outpath <- "5_Documentation"

nldoc(modelfiles = modelfiles,
      infotab=TRUE,
      gui=TRUE,
      bs=TRUE,
      outpath = outpath,
      output_format = "html",
      number_sections = TRUE,
      theme = "cosmo",
      date = date(),
      toc = TRUE)

nldoc(modelfiles = modelfiles,
      infotab=TRUE,
      gui=TRUE,
      bs=TRUE,
      outpath = outpath,
      output_format = "pdf",
      number_sections = TRUE,
      theme = "cosmo",
      date = date(),
      toc = TRUE)

nw <- nldoc_network(modelfiles = modelfiles)

# Determine communities within the network and plot using Igraph package:
library(igraph)
com <- walktrap.community(nw)
V(nw)$community <- com$membership
rain <- rainbow(14, alpha=.5)
V(nw)$color <- rain[V(nw)$community]

png(filename = file.path(outpath, "WolfSheep_network.png"), width = 500, units = "px")
plot(nw,
     edge.arrow.size=1,
     vertex.label.color="black",
     vertex.label.dist=3,
     vertex.size=10,
     edge.curved=0,
     vertex.label.cex=1.5,
     layout=layout_with_fr(nw, niter = 2000))
dev.off()
