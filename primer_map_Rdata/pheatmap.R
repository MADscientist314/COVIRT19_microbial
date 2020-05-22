library(ggplot2)
#import the SNP SNV and primer tables
SNV<-read.csv("SNV.txt", sep="\t", header = T, row.names = NULL)
SNP<-read.csv("SNP.txt",  sep="\t", header = T, row.names = NULL)
primer<-read.csv("primer_maps_NA.txt",  sep="\t", header=T, row.names=1)


#make an empty matrix that has the same dimensions as the primer map
mat<-matrix(0, 272, 30)
#annotate the rows to match the primer map
rownames(mat)<-rownames(primer)
#convert it to a dataframe and append the rownames to a column 
mat<-as.data.frame(mat)
mat$names<-row.names(mat)

#convert the primer names from the SNV and SNP list to ca character class
#I dont know why they wer imported as factors in the first place honestly
SNV$primer<-as.character(SNV$primer)
SNP$primer<-as.character(SNP$primer)

#iterate throught the lists in order to identify the SNP positions
#add 1 to the existing value in that specific region

for(i in 1:nrow(SNV))
{
  for(j in 1:length(mat$names))
  {
    if (SNV$primer[i]==mat$names[j]){
    print(paste(SNV$primer[i],"matches",mat$names[j],"at position",SNV$primer_pos[i],sep = " "))
    p<-as.numeric(SNV$primer_pos[i])
    print(mat[j,p])
    mat[j,p]<-as.numeric(mat[j,p])+1
    print(mat[j,p])
    }
  }
}
mat$names<-NULL
mat2<-as.matrix.data.frame(mat)
colnames(mat2)<-c(1:30)
pheatmap::pheatmap(mat2)
library(hclust)
library(RColorBrewer)
pheatmap::pheatmap(mat2, 
         color = colorRampPalette((brewer.pal(n = 4, name ="BuGn")))(100), 
         kmeans_k = NA, 
         breaks = NA, 
         border_color = "grey60",
         cellwidth = NA, 
         cellheight = NA, 
         scale = "none", 
         cluster_rows = TRUE,
         cluster_cols = FALSE, 
         clustering_distance_rows = "euclidean",
         clustering_method = "complete",
         cutree_rows = NA, 
         legend = TRUE, 
         legend_breaks = NA,
         legend_labels = NA, 
         annotation_row = NA, 
         annotation_col = NA,
         annotation = NA, 
         annotation_colors = NA, 
         annotation_legend = TRUE,
         annotation_names_row = TRUE, 
         annotation_names_col = TRUE,
         drop_levels = TRUE, 
         show_rownames = T, 
         show_colnames = T, 
         main = NA,
         fontsize = 12, 
         fontsize_row = 12, 
         fontsize_col = 12,
         angle_col = c("270", "0", "45", "90", "315"), 
         display_numbers = F,
         number_format = "%.2f", 
         number_color = "grey30", 
         fontsize_number = 1.0 * fontsize, 
         gaps_row = NULL, 
         gaps_col = NULL, 
         labels_row = NULL,
         labels_col = NULL, 
         filename = NA, 
         width = NA, 
         height = NA,
         silent = FALSE, 
         na_col = "#000000")


primerstats<-rowSums(mat2)
hist(primerstats, main = "SNVs Frequency Distubution")
  