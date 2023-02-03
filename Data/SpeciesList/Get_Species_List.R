# ------------------------------------------------------------------------------------------------
# Not reproducible
# load('../../NCEA_StLawrence/eBiotic/Biotic/BioticData/SpeciesList/Data/SpeciesList.RData')
# sp <- dplyr::mutate(sp, species = stringr::str_replace(species, " sp\\.", ""))
# save(sp, file = './Data/SpeciesList/SpeciesStLawrence.RData')
load('./Data/SpeciesList/SpeciesStLawrence.RData')
sp$species[sp$species == "Aphroditella hastata"] <- "Aphrodita hastata"
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Marine mammals
# load('../../NCEA_StLawrence/eBiotic/Biotic/BioticData/MarineMammals/Data/Biotic/MarineMammalsSP.RData')
# save(mmSp, file = './Data/SpeciesList/MarineMammalsSP.RData')
load('./Data/SpeciesList/MarineMammalsSP.RData')
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Marine mammals Atlantic
 #mm_atl <- read.csv("Data/SpeciesList/species_list_marine_mammals_birds-7c150fc3.csv")
# save(mm_atl, file = './Data/SpeciesList/MarineMammalsAtlantic.RData')
load('./Data/SpeciesList/MarineMammalsAtlantic.RData')
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Atlantic
# sp_atl <- read.csv("Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv")
# save(sp_atl, file = './Data/SpeciesList/SpeciesAtlantic.RData')
load('./Data/SpeciesList/SpeciesAtlantic.RData')
# ------------------------------------------------------------------------------------------------
library("dplyr")
library("eaMethods")

# ------------------------------------------------------------------------------------------------
# Single list
sp_atl <- dplyr::select(sp_atl, species = SPEC, Count = Freq, aphiaID)
mmSp <- dplyr::rename(mmSp, Count = CountRec)
mm_atl <- dplyr::rename(mm_atl, species = ScientificName)
spList <- dplyr::bind_rows(sp_atl, sp, mmSp, mm_atl) |>
#spList <- dplyr::bind_rows(sp_atl, sp, mmSp) |>
          dplyr::group_by(species) |>
          dplyr::summarize(Count = sum(Count)) |>
          dplyr::arrange(species) |>
          dplyr::left_join(sp_atl[,c("species", "aphiaID")], by = "species")

# Get aphiaIDs
uid <- is.na(spList$aphiaID)
dat <- spList[uid, ] |> dplyr::select(-aphiaID)
dat <- eaMethods::get_aphia(dat, field = "species")

# Manual entries
dat$aphiaID[dat$species == "Actitis macularius"] <- 159081
dat$aphiaID[dat$species == "Alcyonidium"] <- 110993
dat$aphiaID[dat$species == "Balaenoptera musculus"] <- 137090
dat$aphiaID[dat$species == "Delphinus capensis"] <-137093
dat$aphiaID[dat$species == "Larus tridactyla"] <- 137152
dat$aphiaID[dat$species == "Melita dentata"] <- 102837
dat$aphiaID[dat$species == "Mesoplodon bidens"] <- 137121
dat$aphiaID[dat$species == "Mesoplodon densirostris"] <- 137122
dat$aphiaID[dat$species == "Mesoplodon hectori"] <- 137125
dat$aphiaID[dat$species == "Phalaropus lobatus"] <- 137169
dat$aphiaID[dat$species == "Phalaropus"] <- 137049
dat$aphiaID[dat$species == "Recurvirostra americana"] <- 159140
dat$aphiaID[dat$species == "Sepioloidea"] <- 341459
dat$aphiaID[dat$species == "Thunnus alalunga"] <- 127026
dat$aphiaID[dat$species == "Tringa solitaria"] <- 110993

# Remove extinct species 
rem <- c("Camptorhynchus labradorius","Pinguinus impennis")
dat <- dat[!dat$species %in% rem, ]

# Add to list
spList <- dplyr::bind_rows(spList[!uid, ], dat) |>
          dplyr::arrange(species) |>
          as.data.frame()
# spList
# Export
save(spList, file = './Data/SpeciesList/SpeciesList.RData')
write.csv(spList, file = './Data/SpeciesList/SpeciesList.csv')
