library(ape)
library(tidyverse)
library(Polychrome)

ulp_tree <- ape::read.tree(file = "tree.nwk")
ulp_tree_rooted <- root(ulp_tree, 1)
ulp_tree_rooted_ultra <- chronos(ulp_tree_rooted)
ulp_tree_rooted_ultra<-drop.tip(ulp_tree_rooted_ultra,1)

# check these 3 requirements
is.ultrametric(ulp_tree_rooted_ultra)
is.binary.tree(ulp_tree_rooted_ultra)
is.rooted(ulp_tree_rooted_ultra)

cut_tree <- cutree(as.hclust(ulp_tree_rooted_ultra), k = 50)
pallete = createPalette(50,  c("#ff0000", "#00ff00", "#0000ff"))

sub_clusters <- as.data.frame(cut_tree) %>% 
  mutate(colour = case_when(cut_tree== "1" ~ pallete[[1]],
                            cut_tree== "2" ~ pallete[[2]],
                            cut_tree== "3" ~ pallete[[3]],
                            cut_tree== "4" ~ pallete[[4]],
                            cut_tree== "5" ~ pallete[[5]],
                            cut_tree== "6" ~ pallete[[6]],
                            cut_tree== "7" ~ pallete[[7]],
                            cut_tree== "8" ~ pallete[[8]],
                            cut_tree== "9" ~ pallete[[9]],
                            cut_tree== "10" ~ pallete[[10]],
                            cut_tree== "11" ~ pallete[[11]],
                            cut_tree== "12" ~ pallete[[12]],
                            cut_tree== "13" ~ pallete[[13]],
                            cut_tree== "14" ~ pallete[[14]],
                            cut_tree== "15" ~ pallete[[15]],
                            cut_tree== "16" ~ pallete[[16]],
                            cut_tree== "17" ~ pallete[[17]],
                            cut_tree== "18" ~ pallete[[18]],
                            cut_tree== "19" ~ pallete[[19]],
                            cut_tree== "20" ~ pallete[[20]],
                            cut_tree== "21" ~ pallete[[21]], 
                            cut_tree== "22" ~ pallete[[22]],
                            cut_tree== "23" ~ pallete[[23]],
                            cut_tree== "24" ~ pallete[[24]],
                            cut_tree== "25" ~ pallete[[25]],
                            cut_tree== "26" ~ pallete[[26]],
                            cut_tree== "27" ~ pallete[[27]],
                            cut_tree== "28" ~ pallete[[28]],
                            cut_tree== "29" ~ pallete[[29]],
                            cut_tree== "30" ~ pallete[[30]],
                            cut_tree== "31" ~ pallete[[31]],
                            cut_tree== "32" ~ pallete[[32]],
                            cut_tree== "33" ~ pallete[[33]],
                            cut_tree== "34" ~ pallete[[34]],
                            cut_tree== "35" ~ pallete[[35]],
                            cut_tree== "36" ~ pallete[[36]],
                            cut_tree== "37" ~ pallete[[37]],
                            cut_tree== "38" ~ pallete[[38]],
                            cut_tree== "39" ~ pallete[[39]],
                            cut_tree== "40" ~ pallete[[40]],
                            cut_tree== "41" ~ pallete[[41]],
                            cut_tree== "42" ~ pallete[[42]],
                            cut_tree== "43" ~ pallete[[43]],
                            cut_tree== "44" ~ pallete[[44]],
                            cut_tree== "45" ~ pallete[[45]],
                            cut_tree== "46" ~ pallete[[46]],
                            cut_tree== "47" ~ pallete[[47]],
                            cut_tree== "48" ~ pallete[[48]],
                            cut_tree== "49" ~ pallete[[49]],
                            cut_tree== "50" ~ pallete[[50]], TRUE ~ "black"))

write.csv(sub_clusters %>% select(colour, cut_tree), "sub_clusters-correctStructure_50.csv", quote = F)
  