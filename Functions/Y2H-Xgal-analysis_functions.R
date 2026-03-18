
PanelPlot <- function(Input, Group, show_y_axis = FALSE) {
  SingleGroup <- Input %>% filter(OD == Group)
  SingleGroup$Combination <- gsub(" ", "\n", SingleGroup$Combination)
  Output <- ggplot(SingleGroup, aes(x = Combination, y = Mean)) +
    geom_errorbar(aes(ymin = Mean - STD, ymax = Mean + STD, width = 0.5)) +
    geom_point(size = 2) +
    theme_bw()+
    labs(title = paste("OD = ", Group),
         x = "Combination",
         y = if (show_y_axis) "Grey Value" else NULL) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 10),
      # if not showing the Y axis, remove text/ticks/line
      axis.title.y = if (show_y_axis) element_text() else element_blank(),
      axis.text.y  = if (show_y_axis) element_text() else element_blank(),
      axis.ticks.y = if (show_y_axis) element_line() else element_blank(),
      axis.line.y  = if (show_y_axis) element_line() else element_blank()
    )
  return(Output)
}