# =-=-=-=-=-=-=-=-=-=- ST. LAWRENCE SPECIES -=-=-=-=-=-=-=-=-=-= #
library(magrittr)
library(tidyverse)
library(stringr)
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

classif <- taxize::classification(spList$aphiaID, db = "worms")
dat <- lapply(
  classif, 
  function(x) {
    ifelse(
      "Phylum" %in% x$rank,
      x$name[x$rank == "Phylum"],
      x$name[x$rank == "Phylum (Division)"]
    )
  }
)
phylum <- data.frame(
  species = spList$species,
  phylum = unlist(dat)
)

# Spread data
phylum <- phylum %>%
          mutate(value = 1) %>%
          spread(phylum, value, fill = 0) %>%
          select(-species) %>%
          as.matrix()

rownames(phylum) <- spList$species

# Export
write.csv(phylum, file = './Data/SpeciesTraits/Phylum.csv')
save(phylum, file = './Data/SpeciesTraits/Phylum.RData')
