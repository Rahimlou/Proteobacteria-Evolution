library(ape)
library(phytools)
library(plotrix)
# setting the working place
setwd("C:/Users/barabi/Desktop")

#importing modified tree with 683 tips
tree <- read.tree("modified&rooted_tree.nwk")
tree <- read.tree("pr_55_rooted.nwk")
tree$tip.label <- gsub("_", " ", tree$tip.label)

tree <- reroot(tree, 88)

#Identifying the node lable of Rhizobium sullae species
node <- fastMRCA(tree, "Rhizobium sullae", "Rhizobium etli")
node <- fastMRCA(tree, "Acidobacterium capsulatum", 
                 "Methylobacillus rhizosphaerae")
node <- fastMRCA(tree, "Devosia elaeis", 
                 "Neorhizobium galegae")
node
clade <- extract.clade(tree, 61)
plot(clade)
plot(tree, cex = 0.5)

# Removing internal branches of length zero
tree$edge.length[tree$edge.length == 0] <- 1e-8

#Molecular dating with penalized likelihood
chronograph <- chronopl(tree, 0, age.min = 26.3, age.max = 32.3, 
                        node = 101)
chronograph <- chronopl(tree, 0, age.min = 1786,
                        age.max = 2012, node = 72)
# Ancestral state reconstruction
data <- read.csv("nod_status.csv")
#nif_data <- data$nif
#names(nif_data) <- data$Taxa
nod_data <- data$nod_status
names(nod_data) <- data$species
chronograph$tip.label <- gsub("Pandoraea thiooxydans", "pandoraea thiooxidans", chronograph$tip.label)
fitER <- ace(data$nod_status, chronograph, model="ER", type="discrete", marginal = TRUE)

#ANC <- fastAnc(chronograph, nod_nif_data)


# Plotting the chronograph
plotTree(chronograph,type="fan",tips=seq(5,57, by=57/60),maxY=55,
         fsize = 0.5, lwd = 0.5)

T<-max(nodeHeights(chronograph))
tick.spacing<-500
min.tick<-500
obj<-axis(1,pos=0,at=seq(T,min.tick,by=-tick.spacing),cex.axis=0.5,
          labels=FALSE)

for(i in 1:length(obj)){
  a1<-0
  a2<-2*pi
  draw.arc(0,0,radius=obj[i],a1,a2,lwd=1,
           col=make.transparent("blue",0.1))
}

axis(1,pos=0,at=seq(T,min.tick,by=-tick.spacing),cex.axis=0.5,
     labels=FALSE)
text(obj,rep(-350,length(obj)),T-obj,cex=0.6)
text(mean(obj),-600,"time (mya)",cex=0.8)

# plotting ancestral states on chronograph
cols<-setNames(palette()[1:length(unique(nod_data))],sort(unique(nod_data)))
nodelabels(node=1:chronograph$Nnode+Ntip(chronograph),
           pie=fitER$lik.anc,piecol=cols,cex=0.1)


