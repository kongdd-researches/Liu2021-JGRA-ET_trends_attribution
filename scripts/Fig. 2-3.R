# Dongdong Kong ----------------------------------------------------------------
source("scripts/main_pkgs.R")

load("data-raw/Attribution.RData")
load("data-raw/Detection.RData")

levs <- c("Obs", "historical2", "ANT", "hist-GHG", "hist-aer", "hist-nat")
levs_new <- c("OBS", "ALL", "ANT", "GHG", "AER", "NAT")

df_attri <- melt_list(Attribution, "band")
df_attri$Scenario %<>% factor(levs,levs_new)

tbl1 <- df_attri %>%
  # .[Contitents == "(a) The globe"] %>%
  dt_round(digits = 4) %>%
  mutate(str = sprintf("%.4f [%.4f, %.4f]", V2, betalow, betaup)) %>%
  .[order(band, Scenario)] %>%
  reorder_name(c("band", "Scenario", "str"))
tbl_attri <- tbl1[, -(4:6)] %>% dcast2("Contitents", "str")

df_scale <- melt_list(Detection, "band")
df_scale$Scenario %<>% factor(levs,levs_new)
tbl_scale <- df_scale[Contitents == "(a) The globe"] %>%
  dt_round() %>%
  mutate(str = sprintf("%.2f [%.2f, %.2f]", V2, betalow, betaup)) %>%
  .[order(band, Scenario)] %>%
  reorder_name(c("band", "Scenario", "str"))
write_list2xlsx(listk(tbl_attri, tbl_scale), "table1.xlsx")

# Detection <- Attribution <- list()
for(v in 1:3){
  name <- names(Attribution)[v]
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

  outfile = glue::glue("Figures/Figure3_{name}_fingerprint.pdf")
  # png(paste0('Fig. 3 ',Var1, '_mean_correct.png'), width = 10, height = 7, res = 400, units = 'in')
  write_fig(P1 / P2, outfile)
  # dev.off()
}
