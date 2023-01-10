# Not reproducible
# load('../../NCEA_StLawrence/eBiotic/Biotic/BioticData/SpeciesList/Data/SpeciesList.RData')
# save(sp, file = './Data/SpeciesList/SpeciesStLawrence.RData')
load('./Data/SpeciesList/SpeciesStLawrence.RData')

# Marine mammals
# load('../../NCEA_StLawrence/eBiotic/Biotic/BioticData/MarineMammals/Data/Biotic/MarineMammalsSP.RData')
# save(mmSp, file = './Data/SpeciesList/MarineMammalsSP.RData')
load('./Data/SpeciesList/MarineMammalsSP.RData')

# Marine mammals Atlantic
# mm_atl <- read.csv("Data/SpeciesList/species_list_marine_mammals_birds-7c150fc3.csv")
# save(mm_atl, file = './Data/SpeciesList/MarineMammalsAtlantic.RData')
load('./Data/SpeciesList/MarineMammalsAtlantic.RData')

# Atlantic
# sp_atl <- read.csv("Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv")
# save(sp_atl, file = './Data/SpeciesList/SpeciesAtlantic.RData')
load('./Data/SpeciesList/SpeciesAtlantic.RData')


# Single list 
sp_atl <- dplyr::select(sp_atl, species = SPEC, Count = Freq)
mmSp <- dplyr::rename(mmSp, Count = CountRec)
mm_atl <- dplyr::rename(mm_atl, species = ScientificName)
spList <- dplyr::bind_rows(sp_atl, sp, mmSp, mm_atl) |>
          dplyr::group_by(species) |>
          dplyr::summarize(Count = sum(Count)) |>
          dplyr::arrange(species) |>
          as.data.frame()
save(spList, file = './Data/SpeciesList/SpeciesList.RData')
write.csv(spList, file = './Data/SpeciesList/SpeciesList.csv')
