library(ggplot2)
library(hclust)
library(RColorBrewer)
library(grDevices)
#import the SNP SNP and primer tables
SNP<-read.csv("SNP.txt", sep="\t", header = T, row.names = NULL)
primer<-read.csv("primer_maps_NA.txt",  sep="\t", header=T, row.names=1)


#make an empty matrix that has the same dimensions as the primer map
mat<-matrix(0, 272, 30)
#annotate the rows to match the primer map
rownames(mat)<-rownames(primer)
#convert it to a dataframe and append the rownames to a column 
mat<-as.data.frame(mat)
mat$names<-row.names(mat)

#convert the primer names from the SNP and SNP list to ca character class
#I dont know why they wer imported as factors in the first place honestly

SNP$primer<-as.character(SNP$primer)

#iterate throught the lists in order to identify the SNP positions
#add 1 to the existing value in that specific region

for(i in 1:nrow(SNP))
{
  for(j in 1:length(mat$names))
  {
    if (SNP$primer[i]==mat$names[j]){
      print(paste(SNP$primer[i],"matches",mat$names[j],"at position",SNP$primer_pos[i],sep = " "))
      p<-as.numeric(SNP$primer_pos[i])
      print(mat[j,p])
      mat[j,p]<-as.numeric(mat[j,p])+1
      print(mat[j,p])
    }
  }
}



mat$names<-NULL
matSNP<-as.matrix.data.frame(mat)
colnames(matSNP)<-c(1:30)


pheatmap::pheatmap(matSNP, 
                   color = colorRampPalette((brewer.pal(n = 7, name ="Blues")))(100), 
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
                   main = "SNP locations in primer sequences",
                   fontsize = 10, 
                   fontsize_row = 10, 
                   fontsize_col = 10,
                   angle_col = 0, 
                   display_numbers = F,
                   number_format = "%.2f", 
                   number_color = "grey30", 
                   fontsize_number = 0.8 * fontsize, 
                   gaps_row = NULL, 
                   gaps_col = NULL, 
                   #labels_row = "Primer Name",
                   #labels_col = "Location in Primer Sequence", 
                   silent = FALSE, 
                   na_col = "#000000")

