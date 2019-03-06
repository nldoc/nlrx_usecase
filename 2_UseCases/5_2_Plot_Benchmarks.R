
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot Benchmark results of nlrx and RNetLogo:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pkgs:
library(tidyverse)
library(ggthemes)

## Read rds files in benchmark folder:
benchmarks <- list.files("3_Results/benchmark", pattern = ".rds", full.names = TRUE) %>%
  map_dfr(readRDS)

## Add names for measurement positions:
benchmarks$posname <- ifelse(benchmarks$pos == 1, "Start", ifelse(benchmarks$pos == 2, "Final", "After gc"))
benchmarks$posname <- factor(benchmarks$posname, levels = c("Start", "Final", "After gc"))

## Calculate time demand:
time <- benchmarks %>% 
  group_by(pkg, sysname) %>% 
  filter(pos==max(pos) | pos==min(pos)) %>% 
  mutate(diff = time - lag(time, default = first(time))) %>% 
  dplyr::filter(diff > 0) %>% 
  dplyr::select(pkg, sysname, diff) %>% 
  spread(pkg, diff) %>% 
  mutate_at(c("nlrx", "RNetLogo"), funs(as.numeric(.))) %>% 
  mutate(nlrx_percent = ((nlrx - RNetLogo) / RNetLogo) * 100)

windowsFonts(A = windowsFont("Roboto"))

pt <- ggplot(time, aes(x=0, y=nlrx_percent)) +
  geom_boxplot(fill="#268BD2") +
  coord_flip() +
  theme_classic() +
  ylab("Execution time difference of nlrx compared to RNetLogo [%]") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=14, color="black", family="A"),
        axis.title.x = element_text(size=14, color="black", family="A"))

ggsave(plot=pt, "4_Plots/nlrx_benchmarks_time.png", width=6, height=2, dpi=300)

# Postpro memory:
basemem <- benchmarks %>% filter(pos==1)
memplot <- benchmarks 
memplot$basemem <- rep(basemem$mem, each=3) 
memplot <- memplot %>% mutate(memdiff = mem - basemem)

## Aggregate across systems:
memplot <- memplot %>% group_by(pkg, posname) %>% summarize(memdiff.mu = mean(memdiff), memdiff.sd = sd(memdiff))

## Add label variable:
memplot$pkglabel <- ifelse(memplot$posname=="After gc", memplot$pkg, "")


# Plot Memory:
pm <- ggplot(memplot, aes(x=posname, y=memdiff.mu, color=pkg, group=pkg)) +
  geom_point(stat='summary', fun.y=sum, size=5) +
  geom_errorbar(aes(ymin = memdiff.mu - memdiff.sd, ymax = memdiff.mu + memdiff.sd), width = 0.1) +
  stat_summary(fun.y=sum, geom="line", size=2) +
  geom_text(aes(label=pkglabel), nudge_y=1, size=5, family="A", fontface="italic") +
  ylab("Free Physical Memory [GB]") +
  xlab("Measurement position") +
  scale_color_solarized() +  
  guides(color="none") +
  theme_classic() +
  theme(axis.text = element_text(size=14, color="black", family="A"),
        axis.title = element_text(size=14, color="black", family="A"))

ggsave(plot=pm, "4_Plots/nlrx_benchmarks_memory.png", width=6, height=4, dpi=300)

### Arrange plots:
library(gridExtra)
pall <- grid.arrange(grobs = list(pt, pm), ncol=1, heights=c(1,4))
ggsave(plot=pall, filename = "4_Plots/nlrx_benchmarks.png", width=8, height=8, dpi=300)



