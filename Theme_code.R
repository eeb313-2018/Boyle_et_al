#make theme for all plots
fte_theme <- function(){
  color.background = 'ghostwhite'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill=color.background,color = color.background)) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 8, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 10, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 12, color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 12, color = color.axis.title, vjust = 1.25)) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) 
  
}
