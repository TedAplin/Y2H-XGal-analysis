
PanelPlot <- function(Input, Group, show_y_axis = FALSE, show_legend = FALSE) {
  SingleGroup <- Input %>% filter(OD == Group)
  SingleGroup$Combination <- gsub(" ", "\n", SingleGroup$Combination)
  SingleGroup$Combination <- factor(SingleGroup$Combination, levels = c("HAB1\n+\nPYR1", "EMPTY\n+\nEMPTY", "EMPTY\n+\nCPK30", "EMPTY\n+\nCPK32", "PIF4\n+\nEMPTY", "PIF4\n+\nCPK30", "PIF4\n+\nCPK32"))
  Output <- ggplot(SingleGroup, aes(x = Combination, y = Mean, colour = Type)) +
    geom_errorbar(aes(ymin = Mean - STD, ymax = Mean + STD, width = 0.5)) +
    geom_point(size = 2.5, shape = "diamond") +
    theme_bw()+
    scale_color_manual(values = c("#DA2B67","#00460A", "#165E9C"))+
    labs(title = paste("OD = ", Group),
         x = "Combination",
         y = if (show_y_axis) "Grey Value (Mean +/- SD)" else NULL) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 5.5),
      # if not showing the Y axis, remove text/ticks/line
      axis.title.y = if (show_y_axis) element_text() else element_blank(),
      axis.text.y  = if (show_y_axis) element_text() else element_blank(),
      axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
      axis.line.y  = if (show_y_axis) element_line() else element_blank(),
      legend.position = if(show_legend == FALSE) "none")
  
  return(Output)
}