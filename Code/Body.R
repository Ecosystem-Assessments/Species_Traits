library(magrittr)
library(tidyverse)
load('./Data/SpeciesList/SpeciesStLawrence.RData')
nSp <- nrow(sp)
# =-=-=-=-=-=-=-=-=-=- Species aphia ID from worms -=-=-=-=-=-=-=-=-=-= #
# library(worrms)
# Get AphiaIDs
# aphiaid <- vector('list', nSp)
# names(aphiaid) <- sp$species
# for(i in 1:nSp) aphiaid[[i]] <- try(wm_records_taxamatch(sp$species[i]))

# # Identify missing taxa ids
# id0 <- logical(nSp)
# for(i in 1:nSp) id0[i] <- class(aphiaid[[i]]) == 'try-error'
# nm <- sp$species[id0]
# 
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Not reproducible
# # Staurostoma mertensii: 346
# aphiaid[["Staurostoma mertensii"]] <- list(data.frame(AphiaID = 594013))
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# Vector of ids
# aid <- numeric(nSp)
# for(i in 1:nSp) aid[i] <- aphiaid[[i]][[1]]$AphiaID[1]

# # Get attributes from worms
# spAttr <- vector('list', nSp)
# for(i in 1:nSp) spAttr[[i]] <- try(wm_attr_data(id = aid[i], include_inherited = T))
# names(spAttr) <- sp$species
# For now, export aphiaid & attributes, just to avoid loading querying averything again
# save(aphiaid, file = './Data/SpAttributes/aphiaid.RData')
# save(spAttr, file = './Data/SpAttributes/spAttr.RData')
load('./Data/SpAttributes/aphiaid.RData')
# Vector of ids
aid <- numeric(nSp)
for(i in 1:nSp) aid[i] <- aphiaid[[i]][[1]]$AphiaID[1]
sp$aphiaID <- aid
sp <- dplyr::select(sp, species, aphiaID)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
spList <- read.csv(
  './Data/SpeciesList/species_list_nw_atlantic-893b37e8.csv', 
  sep=",", 
  row.names = NULL
) |>
dplyr::select(species=SPEC, aphiaID)

# ---Combine
sp <- dplyr::bind_rows(sp, spList) |>
      unique()

# Add to species list 
load("Data/SpeciesList/SpeciesList.RData")
spList <- dplyr::left_join(spList, sp, by = "species")
nSp <- nrow(spList)

# # Get attributes from worms
# aid <- spList$aphiaID
# library(worrms)
# spAttr <- vector('list', nSp)
# for(i in 1:nSp) spAttr[[i]] <- try(wm_attr_data(id = aid[i], include_inherited = T))
# names(spAttr) <- spList$species

# For now, export aphiaid & attributes, just to avoid loading querying averything again
# save(spAttr, file = './Data/SpAttributes/spAttr2.RData')
load('./Data/SpAttributes/spAttr2.RData')

# =-=-=-=-=-=-=-=-=-=- Check all for body composition -=-=-=-=-=-=-=-=-=-= #
# Empty list
comp <- vector('list', nSp)
names(comp) <- spList$species


# Go through all species data to get body composition, if available
for(i in names(comp)) {
  if (i %in% names(spAttr)) {
    # Object with data for "simpler" code
    dat <- spAttr[[i]]

    # Check if data is available
    if (any(class(dat) == "data.frame")) {
      # Check if body composition is available
      uid <- dat$measurementTypeID == 47
      if (any(uid)) {
        uid <- which(uid)

        # Data.frame to store composition
        comp[[i]] <- data.frame(taxa = spList$species[i],
                                structure = character(length(uid)),
                                composition = character(length(uid)),
                                stringsAsFactors = F)

        # Extract composition information
        for(j in 1:length(uid)) {
          # Structure
          comp[[i]]$structure[j] <- dat$children[[uid[j]]]$measurementValue

          # Composition
          comp[[i]]$composition[j] <- dat$children[[uid[j]]]$children[[1]]$measurementValue
        }
      # else check if it's fish and put as cartilaginous
      # TODO: check for bony fish manually afterwards
      } else {
        uid <- dat$measurementTypeID == 13
        if (any(uid)) {
          uid <- which(uid)[1]
          # Data.frame to store composition
          comp[[i]] <- data.frame(taxa = spList$species[i],
                                  structure = character(1),
                                  composition = character(1),
                                  stringsAsFactors = F)

          comp[[i]]$structure <- 'Solid'
          comp[[i]]$composition <- 'Cartilaginous'
        }
      }
    }  
  }
}

# =-=-=-=-=-=-=-=-=-=- Marine mammals as bone body composition manually -=-=-=-=-=-=-=-=-=-= #
# Load species list
load('./Data/SpeciesList/MarineMammalsSP.RData')
mmSp$species <- as.character(mmSp$species)

# Insert data.frame with body composition
for(i in mmSp$species) {
  comp[[i]] <- data.frame(taxa = i,
                        structure = 'Solid',
                        composition = 'Bone',
                        stringsAsFactors = F)
}

# =-=-=-=-=-=-=-=-=-=- Missing species -=-=-=-=-=-=-=-=-=-= #
nm <- spList$species[unlist(lapply(comp, is.null))]
options(stringsAsFactors = FALSE)

#https://eol.org/pages/46561171
comp[['Acipenser oxyrhynchus']] <- data.frame(taxa = 'Acipenser oxyrhynchus',
structure = 'skeleton', composition = 'Bone')

# https://eol.org/pages/420985
comp[['Actinauge sp.']] <- data.frame(taxa = 'Actinauge sp.',
structure = 'tissue', composition = 'non-calcifying')

#https://eol.org/pages/51548193
comp[['Actiniidae']] <- data.frame(taxa = 'Actiniidae',
structure = 'tissue', composition = 'non_calcifying')

# https://eol.org/pages/421113
comp[['Actinostola']] <- data.frame(taxa = 'Actinostola sp.',
structure = 'tissue', composition = 'non-calcifying')

#From genus: https://eol.org/pages/963560
comp[['Agarum cribrosum']] <- data.frame(taxa = 'Agarum cribrosum',
structure = 'tissue', composition = 'non-calcifying')

# From Securiflustra securifrons; https://eol.org/pages/600560
comp[['Alcyonidium sp.']] <- data.frame(taxa = 'Alcyonidium sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/450281
comp[['Amicula vestita']] <- data.frame(taxa = 'Amicula vestita',
structure = c('skeleton','tissue'), composition = c('aragonite','calcium phosphate'))

# https://eol.org/pages/439768
comp[['Antalis sp.']] <- data.frame(taxa = 'Antalis sp.',
structure = 'skeleton', composition = 'aragonite')

#
comp[['Aphroditella hastata']] <- data.frame(taxa = 'Aphroditella hastata',
structure = 'hydroskeleton', composition = 'non-calcifying')

#TBD
comp[['Arenicola marina']] <- data.frame(taxa = 'Arenicola marina',
structure = 'NA', composition = 'NA')

# https://eol.org/pages/455235
comp[['Arrhoges occidentalis']] <- data.frame(taxa = 'Arrhoges occidentalis',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46583984
# https://eol.org/pages/46583985
comp[['Ascidiacea']] <- data.frame(taxa = 'Ascidiacea',
structure = 'tissue', composition = 'non-calcifying')

#https://eol.org/pages/46568876
comp[['Aspidophoroides olriki']] <- data.frame(taxa = 'Aspidophoroides olriki',
structure = 'skeleton', composition = 'Bone')

# meduse: https://eol.org/pages/46554120
comp[['Atolla wyvillei']] <- data.frame(taxa = 'Atolla wyvillei',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# https://eol.org/pages/46460933
comp[['Aulacofusus brevicauda']] <- data.frame(taxa = 'Aulacofusus brevicauda',
structure = 'skeleton', composition = 'calcium carbonate')

# meduse: https://eol.org/pages/46554351
comp[['Aurelia aurita']] <- data.frame(taxa = 'Aurelia aurita',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

#https://eol.org/pages/46565645
comp[['Bassogigas gilli']] <- data.frame(taxa = 'Bassogigas gilli',
structure = 'skeleton', composition = 'Bone')

#https://eol.org/pages/46562934
comp[['Bathylagus bericoides']] <- data.frame(taxa = 'Bathylagus bericoides',
structure = 'skeleton', composition = 'Bone')

# pieuvre: https://eol.org/pages/492279
comp[['Bathypolypus']] <- data.frame(taxa = 'Bathypolypus sp.',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46459700
comp[['Beringius turtoni']] <- data.frame(taxa = 'Beringius turtoni',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/49109568
comp[['Bolocera']] <- data.frame(taxa = 'Bolocera sp.',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/46584624
comp[['Boltenia ovifera']] <- data.frame(taxa = 'Boltenia ovifera',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/452436
comp[['Boreotrophon sp.']] <- data.frame(taxa = 'Boreotrophon sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/600560
# https://eol.org/pages/585865
comp[['Bryozoa']] <- data.frame(taxa = 'Bryozoa',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46459765
comp[['Buccinum']] <- data.frame(taxa = 'Buccinum sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/598045
comp[['Caberea ellisii']] <- data.frame(taxa = 'Caberea ellisii',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/46564880
comp[['Caelorinchus caelorinchus']] <- data.frame(taxa = 'Caelorinchus caelorinchus',
structure = 'skeleton', composition = 'Bone')

#From the family Calliostomatidae: https://eol.org/pages/2376
comp[['Calliostoma occidentale']] <- data.frame(taxa = 'Calliostoma occidentale',
structure = 'tissue', composition = 'calcium carbonate')

#From the family: https://eol.org/pages/46473459
comp[['Cerastoderma pinnulatum']] <- data.frame(taxa = 'Cerastoderma pinnulatum',
structure = 'solid', composition = 'aragonite')

#https://eol.org/pages/62756
comp[['Cirroteuthis']] <- data.frame(taxa = 'Cirroteuthis',
structure = 'tissue', composition = 'aragonite')

#https://eol.org/pages/1547
comp[['Colossendeis']] <- data.frame(taxa = 'Colossendeis',
structure = 'solid', composition = 'chitinous')

# https://eol.org/pages/46450434
comp[['Colga villosa']] <- data.frame(taxa = 'Colga villosa',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/46459938
comp[['Colus']] <- data.frame(taxa = 'Colus sp.',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/46562824
comp[['Conocara salmonea']] <- data.frame(taxa = 'Conocara salmonea',
structure = 'skeleton', composition = 'Bone')

#https://eol.org/pages/46578893
comp[['Cookeolus boops']] <- data.frame(taxa = 'Cookeolus boops',
structure = 'skeleton', composition = 'Bone')

#https://eol.org/pages/46569227
comp[['Cottunculus thompsoni']] <- data.frame(taxa = 'Cottunculus thompsoni',
structure = 'skeleton', composition = 'Bone')

# meduse: https://eol.org/pages/46554310/data
comp[['Cyanea capillata']] <- data.frame(taxa = 'Cyanea capillata',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# https://eol.org/pages/46450774
comp[['Dendronotus']] <- data.frame(taxa = 'Dendronotus sp.',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/51129111
comp[['Doridoxa ingolfiana']] <- data.frame(taxa = 'Doridoxa ingolfiana',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/46522542
comp[['Epimeria loricata']] <- data.frame(taxa = 'Epimeria loricata',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/200639
comp[['Epizoanthus erdmanni']] <- data.frame(taxa = 'Epizoanthus erdmanni',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/200658
comp[['Epizoanthus incrustatus']] <- data.frame(taxa = 'Epizoanthus incrustatus',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/46583078
comp[['Eudistoma vitreum']] <- data.frame(taxa = 'Eudistoma vitreum',
structure = 'tissue', composition = 'non-calcifying')

#https://eol.org/pages/2090
comp[['Flustra foliacea']] <- data.frame(taxa = 'Flustra foliacea',
structure = 'tissue', composition = 'calcium carbonate')

#https://eol.org/pages/46571413
comp[['Foetorepus agassizi']] <- data.frame(taxa = 'Foetorepus agassizi',
structure = 'skeleton', composition = 'Bone')

#
comp[['Gadus ogac']] <- data.frame(taxa = 'Gadus ogac',
structure = 'solid', composition = 'cartilaginous')

#TBD
comp[['Glycera capitata']] <- data.frame(taxa = 'Glycera capitata',
structure = 'NA', composition = 'NA')

# https://eol.org/pages/439714
comp[['Gonatus fabricii']] <- data.frame(taxa = 'Gonatus fabricii',
structure = 'tissue', composition = 'aragonite')

#https://eol.org/pages/586034
comp[['Gonatus steenstrupi']] <- data.frame(taxa = 'Gonatus steenstrupi',
structure = 'tissue', composition = 'aragonite')

#https://eol.org/pages/46574735
comp[['Gymnelis viridis']] <- data.frame(taxa = 'Gymnelis viridis',
structure = 'skeleton', composition = 'Bone')

# https://eol.org/pages/1163432
comp[['Halichondria panicea']] <- data.frame(taxa = 'Halichondria panicea',
structure = 'skeleton', composition = 'biogenic silica')

# https://eol.org/pages/46495230
comp[['Hamingia arctica']] <- data.frame(taxa = 'Hamingia arctica',
structure = 'hydroskeleton', composition = 'non-calcifying')

# https://eol.org/pages/46555727
comp[['Hemithiris psittacea']] <- data.frame(taxa = 'Hemithiris psittacea',
structure = 'tissue', composition = 'calcite')

#https://eol.org/pages/439709
comp[['Histioteuthis reversa']] <- data.frame(taxa = 'Histioteuthis reversa',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/704318
comp[['Hormathia nodosa']] <- data.frame(taxa = 'Hormathia nodosa',
structure = 'tissue', composition = 'non-calcifying')

#https://eol.org/pages/473520
comp[['Hyalinoecia tubicola']] <- data.frame(taxa = 'Hyalinoecia tubicola',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/451352
comp[['Illex illecebrosus']] <- data.frame(taxa = 'Illex illecebrosus',
structure = 'tissue', composition = 'aragonite')

#https://eol.org/pages/46548784
comp[['Keratoisis ornata']] <- data.frame(taxa = 'Keratoisis ornata',
structure = 'tissue', composition = 'calcite')

#https://eol.org/pages/3570
comp[['Laminaria longicruris']] <- data.frame(taxa = 'Laminaria longicruris',
structure = 'tissue', composition = 'non_calcifying')

# bird
comp[['Larus sp.']] <- data.frame(taxa = 'Larus sp.',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/52252393
comp[['Leathesia difformis']] <- data.frame(taxa = 'Leathesia difformis',
structure = 'tissue', composition = 'non_calcifying')

#https://eol.org/pages/46514301
comp[['Lebbeus zebra']] <- data.frame(taxa = 'Lebbeus zebra',
structure = 'solid', composition = 'calcium carbonate')

#https://eol.org/pages/46451507
comp[['Limacina']] <- data.frame(taxa = 'Limacina',
structure = 'solid', composition = 'calcium carbonate')

#https://eol.org/pages/46570120
comp[['Liopsetta putnami']] <- data.frame(taxa = 'Liopsetta putnami',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/2550449
comp[['Liponema multicorne']] <- data.frame(taxa = 'Liponema multicorne',
structure = 'tissue', composition = 'non-calcifying')

#From the family: https://eol.org/pages/2351
comp[['Loligo pealeii']] <- data.frame(taxa = 'Loligo pealeii',
structure = 'tissue', composition = 'aragonite')

#https://eol.org/pages/45275204
comp[['Lophelia pertusa']] <- data.frame(taxa = 'Lophelia pertusa',
structure = 'solid', composition = 'aragonite')

#https://eol.org/pages/46574696
comp[['Lumpenus lumpretaeformis']] <- data.frame(taxa = 'Lumpenus lumpretaeformis',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46574693
comp[['Lumpenus maculatus']] <- data.frame(taxa = 'Lumpenus maculatus',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46574695
comp[['Lumpenus medius']] <- data.frame(taxa = 'Lumpenus medius',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46574804
comp[['Lycenchelys verrilli']] <- data.frame(taxa = 'Lycenchelys verrilli',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46574862
comp[['Lycodes esmarki']] <- data.frame(taxa = 'Lycodes esmarki',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46567475
comp[['Macrorhamphosus scolopax']] <- data.frame(taxa = 'Macrorhamphosus scolopax',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46563367
comp[['Macrostomias longibarbatus']] <- data.frame(taxa = 'Macrostomias longibarbatus',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/401186
comp[['Margarites']] <- data.frame(taxa = 'Margarites sp.',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/51426
comp[['Mastigoteuthis']] <- data.frame(taxa = 'Mastigoteuthis',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46530090
comp[['Melita dentata']] <- data.frame(taxa = 'Melita dentata',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/421495
comp[['Metridium senile']] <- data.frame(taxa = 'Metridium senile',
structure = 'tissue', composition = 'non-calcifying')

# bird
comp[['Morus bassanus']] <- data.frame(taxa = 'Morus bassanus',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46503340
comp[['Munida valida']] <- data.frame(taxa = 'Munida valida',
structure = 'solid', composition = 'calcium carbonate')

# https://eol.org/pages/590174
# https://eol.org/pages/47012942
comp[['Naticidae']] <- data.frame(taxa = 'Naticidae',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46460933
comp[['Neptunea']] <- data.frame(taxa = 'Neptunea sp.',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/46563818
comp[['Notolepis rissoi']] <- data.frame(taxa = 'Notolepis rissoi',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/502990
comp[['Nucella lapillus']] <- data.frame(taxa = 'Nucella lapillus',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/511389
comp[['Nymphon']] <- data.frame(taxa = 'Nymphon sp.',
structure = 'tissue', composition = 'non-calcifying')

# bird
comp[['Oceanites sp.']] <- data.frame(taxa = 'Oceanites sp.',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/586032
comp[['Octopoteuthis']] <- data.frame(taxa = 'Octopoteuthis',
structure = 'tissue', composition = 'aragonite')

#From the genus:https://www.marinespecies.org/aphia.php?p=taxdetails&id=106485#attributes
comp[['Oithona spinirostris']] <- data.frame(taxa = 'Oithona spinirostris',
structure = 'solid', composition = 'chitinous')

# https://eol.org/pages/453373
comp[['Ommastrephes']] <- data.frame(taxa = 'Ommastrephes',
structure = 'tissue', composition = 'aragonite')

#TBD
comp[['Ophelia']] <- data.frame(taxa = 'Ophelia',
structure = 'NA', composition = 'NA')

#https://eol.org/pages/46549709
comp[['Pachycerianthus borealis']] <- data.frame(taxa = 'Pachycerianthus borealis',
structure = 'tissue', composition = 'non_calcifying')

# https://eol.org/pages/46450364
comp[['Palio dubia']] <- data.frame(taxa = 'Palio dubia',
structure = 'tissue', composition = 'non-calcifying')

#https://eol.org/pages/46563845
comp[['Paralepis atlantica']] <- data.frame(taxa = 'Paralepis atlantica',
structure = 'skeleton', composition = 'bone')

#https://eol.org/pages/46521658
comp[['Parathemisto']] <- data.frame(taxa = 'Parathemisto',
structure = 'solid', composition = 'calcium carbonate')

#https://eol.org/pages/46554343
comp[['Pelagia noctiluca']] <- data.frame(taxa = 'Pelagia noctiluca',
structure = 'tissue', composition = 'calcium_sulfate')

# https://eol.org/pages/46549373
comp[['Pennatula grandis']] <- data.frame(taxa = 'Pennatula grandis',
structure = 'skeleton', composition = 'calcite')

# meduse: https://eol.org/pages/46554174
comp[['Periphylla periphylla']] <- data.frame(taxa = 'Periphylla periphylla',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# https://eol.org/pages/51887238
comp[['Phascolion strombus strombus']] <- data.frame(taxa = 'Phascolion strombus strombus',
structure = 'hydroskeleton', composition = 'non-calcifying')

# https://eol.org/pages/45502527
comp[['Pleurobrachia pileus']] <- data.frame(taxa = 'Pleurobrachia pileus',
structure = 'tissue', composition = 'non-calcifying')

#
comp[['Polycheles sculptus']] <- data.frame(taxa = 'Polycheles sculptus',
structure = 'solid', composition = 'calcium carbonate')

#https://eol.org/pages/46516677
comp[['Polynoidae']] <- data.frame(taxa = 'Polynoidae',
structure = 'hydroskeleton', composition = 'non-calcifying')

# https://eol.org/pages/45322562
# https://eol.org/pages/46477078
comp[['Porifera']] <- data.frame(taxa = 'Porifera',
structure = 'skeleton', composition = 'biogenic silica')

#TBD
comp[['Potamilla neglecta']] <- data.frame(taxa = 'Potamilla neglecta',
structure = 'NA', composition = 'NA')

#https://eol.org/pages/38586
comp[['Pterygioteuthis']] <- data.frame(taxa = 'Pterygioteuthis',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46552246
comp[['Ptychogena lactea']] <- data.frame(taxa = 'Ptychogena lactea',
structure = 'tissue', composition = 'calcium phosphate')

#bird
comp[['Puffinus gravis']] <- data.frame(taxa = 'Puffinus gravis',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/46543649
comp[['Pycnogonum littorale']] <- data.frame(taxa = 'Pycnogonum litorale',
structure = 'tissue', composition = 'non-calcifying')

#fish
comp[['Raja bathyphila']] <- data.frame(taxa = 'Raja bathyphila',
structure = 'skeleton', composition = 'bone')

#fish
comp[['Raja eglanteria']] <- data.frame(taxa = 'Raja eglanteria',
structure = 'skeleton', composition = 'bone')

#fish
comp[['Raja garmani']] <- data.frame(taxa = 'Raja garmani',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/585865
comp[['Reteporella grimaldii']] <- data.frame(taxa = 'Reteporella grimaldii',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46447659
comp[['Scaphander punctostriatus']] <- data.frame(taxa = 'Scaphander punctostriatus',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/600560
comp[['Securiflustra securifrons']] <- data.frame(taxa = 'Securiflustra securifrons',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46475633
comp[['Sepioloidea sp.']] <- data.frame(taxa = 'Sepioloidea sp.',
structure = c('skeleton','tissue'), composition = 'aragonite')

# https://eol.org/pages/46502383
comp[['Sergia robusta']] <- data.frame(taxa = 'Sergia robusta',
structure = 'skeleton', composition = 'calcium phosphate')

#https://eol.org/pages/46552473
comp[['Sertularia polyzonias']] <- data.frame(taxa = 'Sertularia polyzonias',
structure = 'tissue', composition = 'calcium_phosphate')

#https://eol.org/pages/46465660
comp[['Solemya borealis']] <- data.frame(taxa = 'Solemya borealis',
structure = 'solid', composition = 'aragonite')

#From the genus: https://eol.org/pages/46514369
comp[['Spirontocaris fabricii']] <- data.frame(taxa = 'Spirontocaris fabricii',
structure = 'solid', composition = 'calcium carbonate')

# tissue
comp[['Staurostoma mertensii']] <- data.frame(taxa = 'Staurostoma mertensii',
structure = 'tissue', composition = 'calcium phosphate')

# https://eol.org/pages/493012
comp[['Stauroteuthis syrtensis']] <- data.frame(taxa = 'Stauroteuthis syrtensis',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/54304
comp[['Stephanauge sp.']] <- data.frame(taxa = 'Stephanauge sp.',
structure = 'tissue', composition = 'non-calcifying')

#fish
comp[['Stephanolepis hispidus']] <- data.frame(taxa = 'Stephanolepis hispidus',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/421131
comp[['Stomphia coccinea']] <- data.frame(taxa = 'Stomphia coccinea',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/45322457
comp[['Stylocordyla borealis']] <- data.frame(taxa = 'Stylocordyla borealis',
structure = 'skeleton', composition = 'biogenic silica')

#fish
comp[['Synagrops bella']] <- data.frame(taxa = 'Synagrops bella',
structure = 'skeleton', composition = 'bone')

#fish
comp[['Synaphobranchus kaupi']] <- data.frame(taxa = 'Synaphobranchus kaupi',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/51510532
comp[['Tachyrhynchus erosus']] <- data.frame(taxa = 'Tachyrhynchus erosus',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/51258428
comp[['Tealia felina']] <- data.frame(taxa = 'Tealia felina',
structure = 'tissue', composition = 'non_calcifying')

# https://eol.org/pages/46555849
comp[['Terebratulina septentrionalis']] <- data.frame(taxa = 'Terebratulina septentrionalis',
structure = 'tissue', composition = 'calcite')

#https://eol.org/pages/586270
comp[['Teuthowenia megalops']] <- data.frame(taxa = 'Teuthowenia megalops',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46552783
comp[['Thuiaria thuja']] <- data.frame(taxa = 'Thuiaria thuja',
structure = 'tissue', composition = 'calcium phosphate')

# https://eol.org/pages/46447389
comp[['Tonicella sp.']] <- data.frame(taxa = 'Tonicella sp.',
structure = c('tissue','skeleton'), composition = c('calcium carbonate','aragonite'))

#electric ray
comp[['Torpedo nobiliana']] <- data.frame(taxa = 'Torpedo nobiliana',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/46461545
comp[['Tritia sp.']] <- data.frame(taxa = 'Tritia sp.',
structure = 'skeleton', composition = 'calcium carbonate')

#fish
comp[['Urophycis chesteri']] <- data.frame(taxa = 'Urophycis chesteri',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/51258428
comp[['Urticina felina']] <- data.frame(taxa = 'Urticina felina',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/596153
comp[['Velutinidae']] <- data.frame(taxa = 'Velutinidae',
structure = 'skeleton', composition = 'calcium carbonate')

#https://eol.org/pages/46468542
comp[['Venericardia borealis']] <- data.frame(taxa = 'Venericardia borealis',
structure = 'solid', composition = 'aragonite')

#From the family: https://eol.org/pages/46469251
comp[['Venus mercenaria']] <- data.frame(taxa = 'Venus mercenaria',
structure = 'solid', composition = 'aragonite')

#fish
comp[['Vomer setapinnis']] <- data.frame(taxa = 'Vomer setapinnis',
structure = 'skeleton', composition = 'bone')

#fish
comp[['Zenopsis ocellata']] <- data.frame(taxa = 'Zenopsis ocellata',
structure = 'skeleton', composition = 'bone')

#algea
comp[['Zostera marina']] <- data.frame(taxa = 'Zostera marina',
structure = 'tissue', composition = 'non_calcifying')
# =-=-=-=-=-=-=-=-=-=- Review these species -=-=-=-=-=-=-=-=-=-= #

# https://eol.org/pages/46468382
comp[["Anomia sp."]] <- data.frame(taxa = "Anomia sp.",
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46469215
comp[["Arctica islandica"]] <- data.frame(taxa = "Arctica islandica",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473532
comp[["Cardium sp."]] <- data.frame(taxa = "Cardium sp.",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473695
comp[["Ciliatocardium ciliatum"]] <- data.frame(taxa = "Ciliatocardium ciliatum",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46468542
comp[["Cyclocardia borealis"]] <- data.frame(taxa = "Cyclocardia borealis",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473263/data
comp[["Cyrtodaria siliqua"]] <- data.frame(taxa = "Cyrtodaria siliqua",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473379/data
comp[["Ensis leei"]] <- data.frame(taxa = "Ensis leei",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473266/data
comp[["Hiatella arctica"]] <- data.frame(taxa = "Hiatella arctica",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46474372
# https://eol.org/pages/46474293/data
comp[["Macoma sp."]] <- data.frame(taxa = "Macoma sp.",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46471958/data
comp[["Mactromeris polynyma"]] <- data.frame(taxa = "Mactromeris polynyma",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46465297/data
comp[["Megayoldia thraciaeformis"]] <- data.frame(taxa = "Megayoldia thraciaeformis",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46469524/data
comp[["Mercenaria mercenaria"]] <- data.frame(taxa = "Mercenaria mercenaria",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46472118/data
comp[["Mesodesma sp."]] <- data.frame(taxa = "Mesodesma sp.",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46470499
comp[["Mya arenaria"]] <- data.frame(taxa = "Mya arenaria",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46470500/data
comp[["Mya truncata"]] <- data.frame(taxa = "Mya truncata",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46465116
comp[["Nuculana sp."]] <- data.frame(taxa = "Nuculana sp.",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473272/data
comp[["Panomya norvegica"]] <- data.frame(taxa = "Panomya norvegica",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473768
comp[["Parvicardium pinnulatum"]] <- data.frame(taxa = "Parvicardium pinnulatum",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46473702
comp[["Serripes groenlandicus"]] <- data.frame(taxa = "Serripes groenlandicus",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46471945
comp[["Spisula solidissima"]] <- data.frame(taxa = "Spisula solidissima",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46474253
comp[["Tellina sp."]] <- data.frame(taxa = "Tellina sp.",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46470786/data
comp[["Teredo navalis"]] <- data.frame(taxa = "Teredo navalis",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46470872
comp[["Xylophaga atlantica"]] <- data.frame(taxa = "Xylophaga atlantica",
structure = 'skeleton', composition = 'aragonite')

# https://eol.org/pages/46465320
comp[["Yoldia sp."]] <- data.frame(taxa = "Yoldia sp.",
structure = 'skeleton', composition = 'aragonite')

# Others
# https://eol.org/pages/46549486
comp[["Anthoptilum grandiflorum"]] <- data.frame(taxa = "Anthoptilum grandiflorum",
structure = 'soft', composition = 'calcite')

# https://eol.org/pages/46547902
comp[["Drifa glomerata"]] <- data.frame(taxa = "Drifa glomerata",
structure = 'soft', composition = 'calcite')

# https://eol.org/pages/46547904
comp[["Duva florida"]] <- data.frame(taxa = "Duva florida",
structure = 'soft', composition = 'calcite')

# https://eol.org/pages/46547909
comp[["Gersemia rubiformis"]] <- data.frame(taxa = "Gersemia rubiformis",
structure = 'soft', composition = 'calcite')

# https://eol.org/pages/46549359
comp[["Halipteris finmarchica"]] <- data.frame(taxa = "Halipteris finmarchica",
structure = 'soft', composition = 'calcite')

# https://eol.org/pages/46549372
comp[["Pennatula aculeata"]] <- data.frame(taxa = "Pennatula aculeata",
structure = 'soft', composition = 'calcite')


# =-=-=-=-=-=-=-=-=-=- All data in single data.frame and format categories -=-=-=-=-=-=-=-=-=-= #
for(i in 1:length(comp)) comp[[i]]$taxa <- names(comp)[i]
# Single data.frame
body <- bind_rows(comp)

# Format structure categories
# Will keep it only as solid or soft
message('WARNING: The difference between skeleton and exoskeleton would be important to consider')
body$structure <- body$structure %>%
                  gsub("hydroskeleton","soft",.) %>%
                  gsub("tissue","soft",.) %>%
                  gsub("Non-solid: particles","soft",.) %>%
                  gsub("Solid","solid",.) %>%
                  gsub("skeleton","solid",.)

# Format composition categories
body$composition <- body$composition %>%
                    gsub("Calcareous > Amorphous calcium carbonate", "calcite", .) %>%
                    gsub("Calcareous > High-magnesium calcite", "high_magnesium", .) %>%
                    gsub("Calcareous > Calcite", "calcite", .) %>%
                    gsub("Calcareous > Aragonite", "aragonite", .) %>%
                    gsub("calcium sulfate hemihydrate", "calcium_sulfate", .) %>%
                    gsub("Cartilaginous", "cartilaginous", .) %>%
                    gsub("non-calcifying", "non_calcifying", .) %>%
                    gsub("Chitinous", "chitinous", .) %>%
                    gsub("calcium carbonate", "calcite", .) %>%
                    gsub("aragonite", "aragonite", .) %>%
                    gsub("calcium phosphate", "calcium_phosphate", .) %>%
                    gsub("Bone", "bone", .) %>%
                    gsub("cartilaginous", "cartilaginous", .) %>%
                    gsub("biogenic silica", "biogenic_silica", .) %>%
                    gsub("calcite", "calcite", .) %>%
                    gsub("bone", "bone", .) %>%
                    gsub("Phosphatic\r\n", "phosphatic", .)


# Combine structure and composition
body$body <- paste(body$structure, body$composition, sep = '_')

# Simplify categories
body$body <- body$body %>%
             gsub('solid_cartilaginous','cartilaginous',.) %>%
             gsub('soft_non_calcifying','non_calcifying',.) %>%
             gsub('solid_chitinous','chitinous',.) %>%
             gsub('solid_bone','bone',.) %>%
             gsub('solid_biogenic_silica','biogenic_silica',.)

# Spread dataset
body <- body %>%
        select(taxa, body) %>%
        distinct() %>%
        mutate(value = 1) %>%
        spread(body, value, fill = 0)

# As matrix with rownames as species
if (!all(body$taxa == spList$species)) stop('Species are not the same between body dataset and species list')
rownames(body) <- body$taxa
body <- body %>%
        select(-taxa) %>%
        as.matrix()

# Export
save(body, file = './Data/SpeciesTraits/BodyComposition.RData')
write.csv(body, file = './Data/SpeciesTraits/BodyComposition.csv')
