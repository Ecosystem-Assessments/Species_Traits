
# Load species
spList <- read.csv('./Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv', sep=",", row.names = NULL)
head(spList)
# see missing entries
which(is.na(spList$Phylum), arr.ind=TRUE)

# manual entry for:
spList[353,]
spList[487,]
spList[507,]

phylum <- data.frame(spList$SPEC,spList$Phylum)
colnames(phylum) <- c("Species","Phylum")

# Export
save(phylum, file = './Data/SpeciesTraits/Phylum_AjoutAtlantic.RData')
