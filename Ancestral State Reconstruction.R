library(ape)
library(phytools)
setwd("C:/Users/barabi/Desktop/Ancestral_State_Reconstruction")
tree <- read.tree("proteobacteria.nwk")
is.rooted(tree)
tips <- c("Rhodopseudomonas_pentothenatexigens", "Brucella_neotomae", "Brucella_suis", "Brucella_canis", "Brucella_ovis",
          "Brucella_microti", "Brucella_ceti", "Brucella_pinnipedialis", "Brucella_abortus", 
          "Ochrobactrum_thiophenivorans", "Mesorhizobium_jarvisii", "Mesorhizobium_delmotii", "Pararhizobium_polonicum",
          "Ensifer_shofinae", "Agrobacterium_albertimagni", "Rhizobium_naphthalenivorans", "Pseudorhizobium_pelagicum",
          "Rhizobium_favelukesii", "Rhizobium_sophoriradicis", "Rhizobium_aethiopicum", "Methylovorus_sp.",
          "Ralstonia_pseudosolanacearum", "Cupriavidus_nantongensis", "Pandoraea_sputorum", "Burkholderia_ubonensis",
          "Burkholderia_diffusa", "burkholderia_anthina", "Burkholderia_reimsis", "Burkholderia_contaminans",
          "Caballeronia_telluris", "Caballeronia_catudaia", "Caballeronia_turbans", "Paraburkholderia_silvatlantica",
          "Paraburkholderia_piptadeniae", "Paraburkholderia_megapolitana", "Paraburkholderia_sediminicola", 
          "Paraburkholderia_xenovorans", "Paraburkholderia_insulsa", "Kosakonia_radicincitans", "Lelliottia_jeotgali",
          "Enterobacter_mori", "Enterobacter_chuandaensis", "Lelliottia_nimipressuralis", "Enterobacter_chengduensis",
          "Raoultella_ornithinolytica", "Klebsiella_oxytoca", "Citrobacter_werkmanii", "Citrobacter_braakii", 
          "Citrobacter_amalonaticus", "Shigella_boydii", "Pseudomonas_abyssi", "Pseudomonas_bauzanensis", 
          "Pseudomonas_zeshuii", "Pseudomonas_aeruginosa", "Pseudomonas_psychrotolerans", "Pseudomonas_kuykendallii",
          "Pseudomonas_straminea", "Pseudomonas_wadenswilerensis", "Pseudomonas_monteilii", "Pseudomonas_coleopterorum",
          "Pseudomonas_avellanae", "Pseudomonas_coronafaciens", "Pseudomonas_caricapapayae", "Pseudomonas_ficuserectae",
          "Pseudomonas_fuscovaginae", "Pseudomonas_kilonensis", "Pseudomonas_moraviensis", "Pseudomonas_mohnii", 
          "Pseudomonas_veronii", "Pseudomonas_paralactis", "Pseudomonas_salomonii", "Pseudomonas_azotoformans", 
          "Rhodomicrobium_vannielii", "Microvirga_guangxiensis", "Microvirga_ossetica", "Methylobacterium_tarhaniae", 
          "Methylobacterium_aquaticum", "Methylorubrum_extorquens", "Methylobacterium_phyllosphaerae", 
          "Methylobacterium_organophilum", "Ancylobacter_rudongensis", "Afipia_birgiae", "Bradyrhizobium_erythrophlei",
          "Bradyrhizobium_algeriense", "Bradyrhizobium_pachyrhizi", "Bradyrhizobium_stylosanthis", 
          "Devosia_epidermidihirudinis", "Paramesorhizobium_deserti", "Ochrobactrum_pseudogrignonense", 
          "Ochrobactrum_anthropi", "Brucella_inopinata", "Mesorhizobium_ephedrae", "Mesorhizobium_plurifarium", 
          "Rhizobium_subbaraonis", "Agrobacterium_arsenijevicii", "Rhizobium_leucaenae", "Rhizobium_hainanense", 
          "Rhizobium_mongolense", "Rhizobium_ecuadorense", "Methylophilus_methylotrophus", "Duganella_phyllosphaerae",
          "Herbaspirillum_aquaticum", "Herminiimonas_fonticola", "Pandoraea_faecigallinarum", "Burkholderia_latens", 
          "Paraburkholderia_soli", "Caballeronia_humi", "Paraburkholderia_eburnea", "Franconibacter_pulveris", 
          "Cronobacter_muytjensii", "Cronobacter_malonaticus", "Kosakonia_pseudosacchari", "Enterobacter_cloacae", 
          "Klebsiella_quasipneumoniae", "Kluyvera_georgiana", "Citrobacter_sedlakii", "Pseudomonas_aestusnigri", 
          "Pseudomonas_citronellolis", "Pseudomonas_anguilliseptica", "Pseudomonas_hunanensis", 
          "Pseudomonas_syringae_pv._coryli", "Pseudomonas_umsongensis", "Pseudomonas_proteolytica", 
          "Pseudomonas_palleroniana", "Klebsiella_quasivariicola", "Pseudomonas_brassicacearum")

tree_dropped <- drop.tip(tree, tip = tips)
tree_dropped$tip.label <- gsub("_", " ", tree_dropped$tip.label)
tree_dropped <- unroot(tree_dropped)
is.rooted(tree_dropped)
root <- c("Acidobacterium capsulatum", "Pyrinomonas methylalipathogenes", "Bryobacter aggregatus", 
          "Acidipila dinghuensis", "Bryocella elongata", "Edaphobacter dinghuensis", "Silvibacterium bohemicum", 
          "Terracidiphilus gabretensis", "Terriglobus roseus")
tree_rooted <- reroot(tree_dropped, node = 1163)
is.rooted(tree_rooted)
node <- fastMRCA(tree_dropped, "Pyrinomonas methylalipathogenes", "Acidipila dinghuensis")
node

#tree_rooted <- ladderize(tree_rooted)


plot(tree_rooted, type = "fan", cex = 0.1, label.offset = 0.1, root.edge = T)

x <- write.tree(tree_rooted)
write(x, file = "modified_tree.nwk")

data <- read.csv2("genome_size.csv")
#modified.tree <- read.tree("modified_tree.nwk")
genome.size <- data$genome.size
names(genome.size) <- data$Species
tree_rooted$edge.length[tree_rooted$edge.length == 0] <- 1e-8
#AS_pic <- ace(genome.size, multi2di(tree_rooted), type = "discrete")
#options(max.print=100000)
#AS_GLS <- ace(genome.size, tree_rooted, type = "discrete", method = "GLS",
#             corStruct = corBrownian(1, tree_rooted))
#AS <- fastAnc(tree_dropped, genome.size, CI=T )

node <- fastMRCA(tree_dropped, "Pyrinomonas methylalipathogenes", "Acidipila dinghuensis")
root.clade <- extract.clade(tree_dropped, node)
plot(root.clade)
#nodelabels()
#ii <- sapply(root, grep, tree_dropped$tip.label)
#ii
#clade <- extract.clade(tree_rooted, 685)
#labels <- clade$tip.label
#plot(clade)
#labels

#node.data <- read.csv2("AS_PIC.csv")
#plot(node.data$Node_Number, node.data$Genome_Size,
     #ylim = c(20000, 7000000),xlab = "Internal Node Number", ylab = "Estimated Genome Size", pch = 19)
#cor(node.data$Node_Number, node.data$Genome_Size)
#cor.test(node.data$Node_Number, node.data$Genome_Size)

# Molecular dating
node <- fastMRCA(tree_rooted, "Rhizobium sullae", "Rhizobium etli")
node
clade <- extract.clade(tree_rooted, 1156)
plot(clade)
chronograph <- chronopl(tree_rooted, 0, age.min = 26.3, age.max = 32.3, node = 1156)
obj <- contMap(chronograph, genome.size, plot = FALSE)
obj$lims <- c(1560469, 11527706)

# Plotting the ancestral state
plot(obj,type="fan", root.edge = TRUE, fsize = c(0.1, 0.2), legend = 0.6, lwd = 0.5, outline = FALSE)

