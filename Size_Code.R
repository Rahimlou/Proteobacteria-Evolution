setwd("C:/Users/barabi/Desktop")
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)
data <- read.csv2("size_data_proteobacteria.csv")
attach(data)

#data$Family1 <- factor(data$Family)
#gc.content_logmodel <- glm(factor(data$GC_Content) ~ data$Family1, family = binomial()) 
#summary(gc.content_logmodel)

data$Family2 = relevel(data$Family, ref="Rhizobiaceae")
gc.content_model <- lm(as.numeric(as.character(data$GC_Content)) ~ factor(data$Family2))
summary(gc.content_model)

gensize_model <- lm(as.numeric(as.character(data$Genome_Size)) ~ factor(data$Family2))
summary(gensize_model)

#protcount_model <- lm(as.numeric(data$Protein_Count) ~ factor(data$Family2))
#summary(protcount_model)

gc.content_model2 <- lm(as.numeric(as.character(data$GC_Content)) ~ factor(data$Class))
summary(gc.content_model2)

gensize_model2 <- lm(as.numeric(as.character(data$Genome_Size)) ~ factor(data$Class))
summary(gensize_model2)

par(mar=c(4,5,1,1))
boxplot(as.numeric(as.character(data$GC_Content)) ~ data$Class, ylab = "GC Content (%)",
        col = c("skyblue", "chocolate2", "darkolivegreen"), pch = 20)

par(mar=c(7,5,1,1))
boxplot(as.numeric(as.character(data$GC_Content)) ~ data$Family, ylab = "GC Content (%)", pch = 20, las = 3,
        cex.axis = 0.7, col = c("antiquewhite3", "aquamarine3", "chocolate3", "burlywood4", "lightblue3", 
                                "cyan3", "darkgoldenrod", "darkkhaki", "darkolivegreen3", "darkorchid3",
                                "deeppink3", "orchid1"))
par(mar=c(5,5,1,1))
boxplot(as.numeric(as.character(data$Genome_Size)) ~ data$Class, ylab = "Genome Size (base)",
        col = c("skyblue", "chocolate2", "darkolivegreen"), pch = 20)


par(mar=c(7,5,1,1))
boxplot(as.numeric(as.character(data$Genome_Size)) ~ data$Family, ylab = "Genome Size (base)", pch = 20, las = 3,
        cex.axis = 0.7, col = c("antiquewhite3", "aquamarine3", "chocolate3", "burlywood4", "lightblue3", 
                                "cyan3", "darkgoldenrod", "darkkhaki", "darkolivegreen3", "darkorchid3",
                                "deeppink3", "orchid1"))

nod_model_1 <- lm(as.numeric(as.character(data$GC_Content)) ~ data$Class + data$Nodulation)
summary(nod_model_1)

nod_model_2 <- lm(as.numeric(as.character(data$Genome_Size)) ~ data$Class + data$Nodulation)
summary(nod_model_2)

data$Family2 = relevel(data$Family, ref="Rhizobiaceae")
nod_model_3 <- lm(as.numeric(as.character(data$Genome_Size)) ~ data$Family2 + factor(data$Nodulation))
summary(nod_model_3)

nod_model_4 <- lm(as.numeric(as.character(data$Genome_Size)) ~ data$Nodulation)
summary(nod_model_4)

boxplot(as.numeric(as.character(data$Genome_Size)) ~ factor(data$Nodulation), pch = 19, 
        col = c("lightblue2", "chocolate3" ), ylab = "Genome Size (base)")

nod_model_5 <- lm(as.numeric(as.character(data$GC_Content)) ~ data$Nodulation)
summary(nod_model_5)

boxplot(as.numeric(as.character(data$GC_Content)) ~ factor(data$Nodulation), pch = 19, 
        col = c("lightblue2", "chocolate3" ), ylab = "GC Content (%)")

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}


ggplot(data, aes(x = data$Class, y = data$Genome_Size, fill = data$Class)) + 
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "Genome Size (base)", labels = fancy_scientific) + 
  scale_x_discrete(name = "") + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 12, face = "bold"),
        axis.text.y =  element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")


ggplot(data, aes(x = data$Family, y = data$Genome_Size, fill = data$Family)) + 
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "Genome Size (base)", labels = fancy_scientific)+
  scale_x_discrete(labels=abbreviate, name = "") + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 11, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")



y <- as.numeric(as.character(data$GC_Content))
ggplot(data, aes(x = data$Family, y = y, fill = data$Family)) + 
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "GC Content (%)")+
  scale_x_discrete(labels=abbreviate, name = "") + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 11, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

ggplot(data, aes(x = data$Class, y = y, fill = data$Class)) + 
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "GC Content (%)") + 
  scale_x_discrete(name = "") + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 12, face = "bold"),
        axis.text.y =  element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

ggplot(data, aes(x = data$Nodulation, y = data$Genome_Size, fill = data$Nodulation)) + 
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6) +
  scale_y_continuous(name = "Genome Size (base)", labels = fancy_scientific)+
  scale_x_discrete(name = "") + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size=0.5, colour = "black"),
        axis.text.x =  element_text(size = 11, face = "bold"),
        axis.text.y =  element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none")




       