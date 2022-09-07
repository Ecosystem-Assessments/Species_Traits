# Not reproducible
load('../eBiotic/Biotic/BioticData/SpeciesList/Data/SpeciesList.RData')
save(sp, file = './Data/SpeciesList/SpeciesList.RData')

# Marine mammamls
load('../eBiotic/Biotic/BioticData/MarineMammals/Data/Biotic/MarineMammalsSP.RData')
save(mmSp, file = './Data/SpeciesList/MarineMammalsSP.RData')

# Atlantic
sp_atl <- read.csv("Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv")
save(sp_atl, file = './Data/SpeciesList/SpeciesAtlantic.RData')

