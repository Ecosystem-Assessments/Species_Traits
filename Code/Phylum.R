library(magrittr)
library(tidyverse)
library(stringr)
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(sp)

# load taxonomy
load('./Data/Taxonomy/Taxonomy.RData')

# =-=-=-=-=-=-=-=-=-=- Species attributes from worms -=-=-=-=-=-=-=-=-=-= #
taxonomy <- str_split(taxonomy$taxonomy, pattern = ' | ', simplify = T)

# Extract Phylum per taxa
phylum <- data.frame(taxa = sp$species, phylum = taxonomy[,3],
                     stringsAsFactors = F)

# Spread data
phylum <- phylum %>%
          mutate(value = 1) %>%
          spread(phylum, value, fill = 0) %>%
          select(-taxa) %>%
          as.matrix()

rownames(phylum) <- sp$species

# Export
save(phylum, file = './Data/SpeciesTraits/Phylum.RData')
