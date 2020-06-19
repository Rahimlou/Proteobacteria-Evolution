library(ggplot2)
setwd("C:/Users/barabi/Desktop")
data <- read.csv2("sequencing_platform_data.csv")
data$genome_size <- as.numeric(as.character(data$genome_size))
data$Platform <- relevel(data$Platform, "PacBio")
model <- lm(data$genome_size ~ data$Platform)
summary(model)

fancy_scientific <- function(l) {
  x <- l / 10^6
  return(x)
}
ggplot(data, aes(x = data$Platform, y = data$genome_size)) + 
  geom_boxplot() +
  scale_x_discrete(name = "Sequencing Platform") +
  scale_y_continuous(name = "Genome Size (Million Base)", labels = fancy_scientific) + 
  geom_jitter(aes(color = data$Nodulation), shape=16, position=position_jitter(0.2)) + 
  scale_color_manual(values = c("dimgray", "red2")) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 11, face = "bold"),
        axis.text.y =  element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"), 
        legend.title = element_blank())
