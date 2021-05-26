# Two-signal detection. 20210427, LJY
library(ggthemes)
library(ggforce)

load(file = 'data-raw/Data for figure 4.RData')

Xlab <- c("NAT","NAT",'NAT',"AER")
Ylab <- c("AER","GHG",'ANT',"ANT")
F1 <- list()

theme_set(
  theme_grey(base_size = 14, base_family = "Times")
)
for (s in 1:4) {
  data1 <- data2 <- data3 <- list()
  for(v in 1:3){
    x = Detect[[s]][[v]]
    data1[[v]] <- Ellipse(x)
    data2[[v]]  <- data.frame(x = x[2,1], xend = x[2,3], y = x[1,2], yend = x[1,2])
    data3[[v]]  <- data.frame(x = x[2,2], xend = x[2,2], y = x[1,1], yend = x[1,3])
  }
  data1b <- abind(data1, along = 1) %>% as.data.frame()
  data2b <- abind(data2, along = 1) %>% as.data.frame()
  data3b <- abind(data3, along = 1) %>% as.data.frame()
  data1b$Var <-data2b$Var <- data3b$Var <- c('E', 'Et', 'Ev')

  # ----------------------------------------------------- 4. Figure
  F1[[s]] <- ggplot() +
    geom_ellipse(data = data1b, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, color = Var)) +
    geom_segment(data=data2b, mapping=aes(x=x, y=y, xend=xend, yend=yend, color=Var)) +
    geom_segment(data=data3b, mapping=aes(x=x, y=y, xend=xend, yend=yend, color=Var)) +
    geom_vline(xintercept = 0, linetype = 'dashed')+
    geom_hline(yintercept = 0, linetype = 'dashed')+
    xlab(Xlab[s]) + ylab(Ylab[s])+
    xlim(-4,4) + ylim(-4,4)+
    coord_fixed()+
    theme_few()+
    theme(panel.background = element_rect(fill = 'transparent',color = 'black'), #gray50
          axis.title = element_text(size = rel(1),face = "bold"),
          axis.text = element_text(size = rel(1),face = "bold"),
          strip.text = element_text(size = rel(0.8),face = "bold"),
          legend.text = element_text(size = rel(0.8),face = "bold"),
          legend.position = c(0.9, 0.85),
          legend.title= element_blank())
}

g <- F1[[1]] + F1[[2]] + F1[[3]] + F1[[4]] + plot_layout(nrow = 2)
write_fig(g, "Figures/Fig. 6 two-signal detection_globe.pdf", 8, 8)
