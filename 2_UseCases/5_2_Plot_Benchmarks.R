
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
benchmarks$posname <- ifelse(benchmarks$pos == 1, "Pre simulation", ifelse(benchmarks$pos == 2, "Post simulation", "Post gc"))
benchmarks$posname <- factor(benchmarks$posname, levels = c("Pre simulation", "Post simulation", "Post gc"))

## Calculate time demand:
time <- benchmarks %>% 
  group_by(pkg, sysname) %>% 
  filter(pos==max(pos) | pos==min(pos)) %>% 
  mutate(diff = time - lag(time, default = first(time))) %>% 
  dplyr::filter(diff > 0) %>% 
  dplyr::select(pkg, sysname, diff) %>% 
  spread(pkg, diff) %>% 
  mutate_at(c("nlrx", "RNetLogo"), funs(as.numeric(.))) %>% 
  mutate(nlrx_percent = ((RNetLogo - nlrx) / RNetLogo) * 100)

windowsFonts(A = windowsFont("Roboto"))

pt <- ggplot(time, aes(x=0, y=nlrx_percent)) +
  geom_boxplot(fill="#268BD2") +
  coord_flip() +
  theme_classic() +
  ylab("Execution time reduction of nlrx compared to RNetLogo [%]") +
  xlab("\n") +
  ggtitle("Simulation runtime") +
  theme(axis.title.y = element_text(color="white"),
        axis.text.y = element_text(color="white"),
        axis.ticks.y = element_blank(),
        #axis.line.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=14, color="black", family="A"),
        axis.title.x = element_text(size=14, color="black", family="A"),
        title = element_text(size=16, color="black", family="A"),
        strip.text = element_text(size=14, color="black", family="A"),
        strip.background = element_rect(color="black", fill="gray95"),
        plot.margin = margin(0,4,30,2, unit = "pt"))

ggsave(plot=pt, "4_Plots/nlrx_benchmarks_time.png", width=6, height=2, dpi=300)

## Postpro memory:
benchmarks_meta <- list.files("3_Results/benchmark", pattern = ".txt", full.names = TRUE)
systemram <- map_dfr(benchmarks_meta, function(x) {
  xl <- read_lines(x)
  ram <- tibble(systemram = as.numeric(substring(xl[grep("System RAM", xl)], 13, 14)))
})
systemram$sysname <- unique(benchmarks$sysname)

# Postpro memory:
mem_nlrx <- benchmarks %>% filter(pkg=="nlrx") %>% select(posname, sysname, mem) %>% rename(mem_nlrx = mem)
mem_rnet <- benchmarks %>% filter(pkg=="RNetLogo") %>% select(posname, sysname, mem) %>% rename(mem_rnet = mem)
memplot <- mem_nlrx %>%left_join(mem_rnet) %>% 
  left_join(systemram) %>% 
  mutate(nlrx = ((systemram - mem_nlrx) / systemram) * 100,
         RNetLogo = ((systemram - mem_rnet) / systemram) * 100) %>% 
  select(-mem_nlrx, -mem_rnet, -systemram) %>% 
  gather(pkg, ram_p, nlrx, RNetLogo)

memplot$posname <- factor(memplot$posname, 
                          levels=c("Pre simulation", "Post simulation", "Post gc"),
                          labels=c("Pre\nsimulation", "Post\nsimulation", "Post\ngc"))

pm <- ggplot(memplot, aes(x=posname, y=ram_p, fill=pkg)) +
  facet_wrap(~pkg, ncol=2) +
  geom_boxplot() +
  ylab("System memory in use [%]") +
  xlab("") +
  scale_fill_solarized() +
  guides(fill="none") +
  theme_classic() +
  ggtitle("Memory usage") +
  theme(axis.text = element_text(size=14, color="black", family="A"),
        axis.title = element_text(size=14, color="black", family="A"),
        title = element_text(size=16, color="black", family="A"),
        strip.text = element_text(size=14, color="black", family="A"),
        strip.background = element_rect(color="black", fill="gray95"))


ggsave(plot=pm, "4_Plots/nlrx_benchmarks_memory.png", width=7, height=4, dpi=300)


### Arrange plots:
library(gridExtra)
pall <- grid.arrange(grobs = list(pt, pm), ncol=1, heights=c(2,4))
ggsave(plot=pall, filename = "4_Plots/nlrx_benchmarks.png", width=7, height=5, dpi=300)



