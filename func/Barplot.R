Barplot <- function(matrix=distriPlot){
  
  plot <- ggplot(matrix, aes(x=class, y=num,fill=color,color=color)) + 
    geom_boxplot(notch=T,notchwidth = 0.6,width = 0.6,lwd=1.5)+ # 
    stat_boxplot(geom ='errorbar',width = 0.4)+
    labs(title="", x="", y="")+   
    theme(axis.title.x = element_text(size = 15,family ="serif"),axis.title.y = element_text(size = 15,family ="serif"),
          plot.title = element_text( color="black", size=15,face="plain",hjust = 0.5,family ="serif"),
          axis.text.x = element_text(size=15,family ="serif"),axis.text.y = element_text(size=15,family ="serif"),axis.ticks = element_blank(),
          legend.position="none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    scale_fill_manual(values=c("#1CAC78","#4682B4","#724046","#C364C5","#FFA343","#CD5C5C"))+
    scale_color_manual(values=c("#1CAC78","#4682B4","#724046","#C364C5","#FFA343","#CD5C5C"))
  
  return(plot)
}
