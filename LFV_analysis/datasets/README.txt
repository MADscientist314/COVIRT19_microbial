This full alignment (msa_0513.fasta) is based on 21,554 submissions to EpiCoV.
Both duplicate and low-quality sequences (>5% NNNNs) have been removed, using only complete sequences (length >29,000 bp). The resulting alignment of 20,114 sequences is created using mafft https://doi.org/10.1093/molbev/mst010 with the following command:

mafft --thread -1 --nomemsave  gisaid_cov_sequences.fasta > msa_0513.fasta