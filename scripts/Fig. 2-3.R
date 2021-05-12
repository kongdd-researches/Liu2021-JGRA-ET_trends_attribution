library(ECOF)
library(abind)
library(plyr)
library(plotrix)
library(magrittr)
library(RColorBrewer)
library(ggplot2)
library(patchwork)

rm(list = ls())
setwd('F:/Research/16. Human E/R/Code and Data')
library(magrittr)
library(dplyr)
library(ggplot2)
load("Attribution.RData")
load("Detection.RData")

# Detection <- Attribution <- list()
for(v in 1:3){
  Data2 <-  Detection[[v]]
  Contri <- Attribution[[v]]

  P1 <- ggplot(Data2, aes(x = Scenario, y = V2, colour = Scenario))+
    geom_point()+
    geom_errorbar(aes(ymin = betalow, ymax = betaup), width = 0.3)+
    facet_wrap(.~Contitents, scales="free", nrow = 1)+
    # geom_vline(xintercept = 3.5, color = 'darkgray', size = 0.5)+
    xlab('')+  ylab('Scaling factors')+
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    guides(colour = FALSE)+
    theme(axis.title = element_text(size = rel(1), face = "bold"),
          axis.text = element_text(size = rel(1),face = "bold"),
          axis.text.x = element_text(size = rel(1),face = "bold", angle = 40, hjust = 1, vjust = 1),
          strip.text = element_text(size = rel(1),face = "bold"),
          legend.text = element_text(size = rel(1),face = "bold"),
          legend.title= element_blank(),legend.background = element_blank(),legend.position = c(0.85,0.21))

  P2 <- ggplot(Contri, aes(x = Scenario, y = V2, fill = Scenario))+
    geom_bar(stat = "identity", width = 0.6, position = 'stack')+
    geom_errorbar(aes(ymin = betalow, ymax = betaup), width = 0.2)+
    # geom_vline(xintercept = 3.5, color = 'darkgray', size = 0.5)+
    facet_wrap(.~Contitents,  scales="free", nrow = 1)+  #
    xlab('Driving factors')+ ylab('Trend in ET (mm/year)')+
    ylim(-1.5,1.5)+
    guides(fill = FALSE)+
    theme(axis.title = element_text(size = rel(1), face = "bold"),
          axis.text = element_text(size = rel(1),face = "bold"),
          axis.text.x = element_text(size = rel(1),face = "bold", angle = 40, hjust = 1, vjust = 1),
          strip.text = element_text(size = rel(1),face = "bold"),
          legend.text = element_text(size = rel(1),face = "bold"),
          legend.title= element_blank(),legend.background = element_blank(),legend.position = c(0.85,0.21))


  library(patchwork)
  # png(paste0('Fig. 3 ',Var1, '_mean_correct.png'), width = 10, height = 7, res = 400, units = 'in')
  print(P1 / P2)
  # dev.off()
}
