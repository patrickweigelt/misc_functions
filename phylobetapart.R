# Partition phylogenetic beta diversity (Sorensen) into nestedness and turnover components (Baselga 2012, GEB; Leprieur et al. 2012, PlosONE),
# like in phylo.beta.pair {betapart} but faster thanks to {PhyloMeasures} (Tsirogiannis & Sandel 2016, Ecography)

library(PhyloMeasures)

beta.pair.phylomeasures <- function(tree, matrix){

  # Common Branch Lengths
  spec_cbl <- cbl.query(tree, matrix, standardize = FALSE)

  # Faith'S PD
  spec_PD <- pd.query(tree, matrix)

  # Phylogenetic Sorensen beta diversity
  phylobeta_sor <- 1-phylosor.query(tree, matrix)

  # Pairwise minimum PD  
  spec_PD_min <- sapply(spec_PD, function(x) sapply(spec_PD, function(y) min(x,y)))

  # Phylogenetic Simpson beta diversity
  phylobeta_sim <- 1-(spec_cbl/spec_PD_min)

  # Phylogenetic nestedness component of beta diversity
  phylobeta_sne <- phylobeta_sor - phylobeta_sim

  return(list(phylobeta.sim=phylobeta_sim, phylobeta.sne=phylobeta_sne, phylobeta.sor=phylobeta_sor))
}
