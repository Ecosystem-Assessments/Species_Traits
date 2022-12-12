# =-=-=-=-=-=-=-=-=-=- ST. LAWRENCE SPECIES -=-=-=-=-=-=-=-=-=-= #
library(magrittr)
library(tidyverse)
library(stringr)
load('./Data/SpeciesList/SpeciesStLawrence.RData')
nSp <- nrow(sp)

# load taxonomy
load('./Data/Taxonomy/Taxonomy.RData')

# =-=-=-=-=-=-=-=-=-=- Species attributes from worms -=-=-=-=-=-=-=-=-=-= #
taxonomy <- stringr::str_split(taxonomy$taxonomy, pattern = ' | ', simplify = T)

# Extract Phylum per taxa
phylum_stl <- data.frame(taxa = sp$species, phylum = taxonomy[,3],
                     stringsAsFactors = F)

# =-=-=-=-=-=-=-=-=-=- Atlantic SPECIES -=-=-=-=-=-=-=-=-=-= #
# Load species
spList <- read.csv('./Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv', sep=",", row.names = NULL)

# see missing entries
which(is.na(spList$Phylum), arr.ind=TRUE)

# manual entry for:
#Palmaria palmata
spList[353,]$Phylum <- 'Rhodophyta'

#Ulva
spList[487,]$Phylum <- 'Chlorophyta'

#Zostera marina
spList[507,]$Phylum <- 'Tracheophyta'


phylum_atl <- data.frame(spList$SPEC,spList$Phylum)
colnames(phylum_atl) <- c("taxa","phylum")

# =-=-=-=-=-=-=-=-=-=- Combine SPECIES -=-=-=-=-=-=-=-=-=-= #
load("Data/SpeciesList/SpeciesList.RData")
phylum <- dplyr::bind_rows(phylum_stl, phylum_atl) |>
          unique()
sp <- dplyr::select(spList, -Count) |>
      dplyr::left_join(phylum, by = c("species" = "taxa"))

# Spread data
phylum <- sp %>%
          mutate(value = 1) %>%
          spread(phylum, value, fill = 0) %>%
          select(-species) %>%
          as.matrix()

rownames(phylum) <- sp$species

# Export
write.csv(phylum, file = './Data/SpeciesTraits/Phylum.csv')
save(phylum, file = './Data/SpeciesTraits/Phylum.RData')
