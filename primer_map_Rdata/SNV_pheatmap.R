library(ggplot2)
library(RColorBrewer)
library(grDevices)
#import the SNV SNV and primer tables
SNV<-read.csv("SNV.txt",  sep="\t", header = T, row.names = NULL)
primer<-read.csv("primer_maps_NA.txt",  sep="\t", header=T, row.names=1)


#make an empty matrix that has the same dimensions as the primer map
mat<-matrix(0, 272, 30)
#annotate the rows to match the primer map
rownames(mat)<-rownames(primer)
#convert it to a dataframe and append the rownames to a column 
mat<-as.data.frame(mat)
mat$names<-row.names(mat)

#convert the primer names from the SNV and SNV list to ca character class
#I dont know why they wer imported as factors in the first place honestly

SNV$primer<-as.character(SNV$primer)

#iterate throught the lists in order to identify the SNV positions
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
matSNV<-as.matrix.data.frame(mat)
#this is me messing with the SNV that has 14 hits in the same space (DEBUG TESTING CODE ONLY)
#mat4<-as.data.frame(t(matSNV))
#mat4$`Artic_nCoV-2019_39_LEFT`[11]<-1
#matSNV<-as.matrix.data.frame(t(mat4))
colnames(matSNV)<-c(1:30)

pheatmap::pheatmap(matSNV, 
                   color = colorRampPalette((brewer.pal(n = 9, name ="Reds")))(100), 
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
                   main = "SNV locations in primer sequences",
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



