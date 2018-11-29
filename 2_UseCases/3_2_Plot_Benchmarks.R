
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot Benchmark results of nlrx and RNetLogo:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pkgs:
library(tidyverse)
library(ggthemes)


## Read rds files in benchmark folder:
benchmarks <- list.files("3_Results/benchmark", pattern = ".rds", full.names = TRUE) %>%
  map_dfr(readRDS)






######### ADJUST FOLLOWING CODE FOR MORE THAN ONE SYSNAME:



## Add names for measurement positions:
mem$posname <- ifelse(mem$pos == 1, "Start", ifelse(mem$pos == 2, "Final", "After gc"))
mem$posname <- factor(mem$posname, levels = c("Start", "Final", "After gc"))

## Calculate time demand:
time <- mem %>% 
  group_by(pkg) %>% 
  filter(pos==max(pos) | pos==min(pos)) %>% 
  mutate(diff = time - lag(time, default = first(time))) %>% 
  dplyr::filter(diff > 0) %>% 
  dplyr::select(pkg, diff)

# Plot time:
ggplot(time, aes(x=pkg, y=diff, fill=pkg)) +
  geom_bar(stat="identity") +
  scale_y_time() +
  scale_fill_solarized() +
  guides(fill="none") +
  xlab("R package") +
  ylab("Execution time") +
  theme_classic() +
  theme(axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=14, color="black"))

ggsave("4_Plots/nlrx_benchmarks_time.png", width=4, height=4)


# Postpro memory:
basemem <- mem %>% filter(pos==1)
memplot <- mem 
memplot$basemem <- rep(basemem$mem, each=3) 
memplot <- memplot %>% mutate(memdiff = mem - basemem)
memplot$pkglabel <- ifelse(memplot$pos==3, memplot$pkg, "")

# Plot Memory:
ggplot(memplot, aes(x=posname, y=memdiff, color=pkg, group=pkg)) +
  geom_point(stat='summary', fun.y=sum, size=5) +
  stat_summary(fun.y=sum, geom="line", size=2) +
  geom_text(aes(label=pkglabel), nudge_y=0.3, size=5) +
  ylab("Free Physical Memory [GB]") +
  xlab("Measurement position") +
  scale_color_solarized() +  
  guides(color="none") +
  theme_classic() +
  theme(axis.text = element_text(size=14, color="black"),
        axis.title = element_text(size=14, color="black"))

ggsave("4_Plots/nlrx_benchmarks_memory.png", width=6, height=4, dpi=300)


