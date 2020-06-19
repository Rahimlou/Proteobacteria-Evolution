library(ggplot2)
data <- read.csv2("size_data_proteobacteria.csv")
data$Genome_Size <- as.numeric(as.character(data$Genome_Size))
data$GC_Content <- as.numeric(as.character(data$GC_Content))
data$Protein_Count <- as.numeric(as.character(data$Protein_Count))
fancy_scientific <- function(l) {
  x <- l / 10^6
  return(x)
}
legend_title <- "Symbiocity"
ggplot(data, aes(x = data$Genome_Size, y = data$GC_Content)) + 
  geom_point(aes(color = data$Nodulation)) +
  scale_x_continuous(name = "Genome Size (Million base)", labels = fancy_scientific) +
  scale_y_continuous(name = "GC Content (%)") + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 8, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c("cornflowerblue", "red2")) 

ggplot(data, aes(x = data$Genome_Size, y = data$GC_Content)) + 
  geom_point(aes(color = data$Class)) +
  scale_x_continuous(name = "Genome Size (Million base)", labels = fancy_scientific) +
  scale_y_continuous(name = "GC Content (%)") + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 8, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c("green", "red", "dodgerblue")) 
  
ggplot(data, aes(x = data$Genome_Size, y = data$Protein_Count)) + 
  geom_point(aes(color = data$Class)) +
  scale_x_continuous(name = "Genome Size (Million base)", labels = fancy_scientific) +
  scale_y_continuous(name = "Protein Count") + 
  theme(panel.background = element_blank(), 
        axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 8, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        legend.title = element_blank()) + 
  scale_color_manual(values = c("dodgerblue", "red", "green")) 
  
