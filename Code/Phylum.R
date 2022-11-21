
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


phylum <- data.frame(spList$SPEC,spList$Phylum)
colnames(phylum) <- c("Species","Phylum")

phylum

# Export
save(phylum, file = './Data/SpeciesTraits/Phylum_AjoutAtlantic.RData')
