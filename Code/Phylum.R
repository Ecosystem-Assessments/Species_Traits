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
    if (any(!is.na(x))) {
      ifelse(
        "Phylum" %in% x$rank,
        x$name[x$rank == "Phylum"],
        x$name[x$rank == "Phylum (Division)"]
      )      
    }
  }
)
names(dat) <- spList$species
phylum <- purrr::discard(dat, function(x) is.null(x)) 
phylum <- data.frame(species = names(phylum), phylum = unlist(phylum))
phylum <- dplyr::left_join(spList, phylum, by = "species")

# Some manual entries. Don't know why, don't care.
phylum$phylum[phylum$species == "Bassogigas gilli"] <- "Chordata"
phylum$phylum[phylum$species == "Careproctus reinhardti"] <- "Chordata"
phylum$phylum[phylum$species == "Cataetyx laticeps"] <- "Chordata"
phylum$phylum[phylum$species == "Catharacta maccormicki"] <- "Chordata"
phylum$phylum[phylum$species == "Catharacta skua"] <- "Chordata"
phylum$phylum[phylum$species == "Phalaropus"] <- "Chordata"

# Spread data
phylum <- phylum %>%
          select(species, phylum) %>%
          mutate(value = 1) %>%
          spread(phylum, value, fill = 0) %>%
          select(-species) %>%
          as.matrix()

rownames(phylum) <- spList$species

# Export
write.csv(phylum, file = './Data/SpeciesTraits/Phylum.csv')
save(phylum, file = './Data/SpeciesTraits/Phylum.RData')
