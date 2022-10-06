library(magrittr)
library(tidyverse)
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

# =-=-=-=-=-=-=-=-=-=- Species attributes from worms -=-=-=-=-=-=-=-=-=-= #
library(worrms)
# Get AphiaIDs
aphiaid <- vector('list', nSp)
names(aphiaid) <- spList$species
for(i in 1:nSp) aphiaid[[i]] <- try(wm_records_taxamatch(spList$species[i]))

# Identify missing taxa ids
id0 <- logical(nSp)
for(i in 1:nSp) id0[i] <- class(aphiaid[[i]]) == 'try-error'
nm <- spList$species[id0]

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Not reproducible
# Staurostoma mertensii: 346
aphiaid[["Staurostoma mertensii"]] <- list(data.frame(AphiaID = 594013))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Vector of ids
aid <- numeric(nSp)
for(i in 1:nSp) aid[i] <- aphiaid[[i]][[1]]$AphiaID[1]

# Get attributes from worms
spAttr <- vector('list', nSp)
for(i in 1:nSp) spAttr[[i]] <- try(wm_attr_data(id = aid[i], include_inherited = T))
names(spAttr) <- spList$species

# For now, export aphiaid & attributes, just to avoid loading querying averything again
# save(aphiaid, file = './Data/Temp/aphiaid.RData')
# save(spAttr, file = './Data/Temp/spAttr.RData')
# load('./Data/Temp/aphiaid.RData')
# load('./Data/Temp/spAttr.RData')

# =-=-=-=-=-=-=-=-=-=- Check all for body composition -=-=-=-=-=-=-=-=-=-= #
# Empty list
comp <- vector('list', nSp)
names(comp) <- spList$species

# Go through all species data to get body composition, if available
for(i in 1:nSp) {
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

# https://eol.org/pages/420985
comp[['Actinauge sp.']] <- data.frame(taxa = 'Actinauge sp.',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/421113
comp[['Actinostola sp.']] <- data.frame(taxa = 'Actinostola sp.',
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

# https://eol.org/pages/455235
comp[['Arrhoges occidentalis']] <- data.frame(taxa = 'Arrhoges occidentalis',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46583984
# https://eol.org/pages/46583985
comp[['Ascidiacea']] <- data.frame(taxa = 'Ascidiacea',
structure = 'tissue', composition = 'non-calcifying')

# meduse: https://eol.org/pages/46554120
comp[['Atolla wyvillei']] <- data.frame(taxa = 'Atolla wyvillei',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# https://eol.org/pages/46460933
comp[['Aulacofusus brevicauda']] <- data.frame(taxa = 'Aulacofusus brevicauda',
structure = 'skeleton', composition = 'calcium carbonate')

# meduse: https://eol.org/pages/46554351
comp[['Aurelia aurita']] <- data.frame(taxa = 'Aurelia aurita',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# pieuvre: https://eol.org/pages/492279
comp[['Bathypolypus sp.']] <- data.frame(taxa = 'Bathypolypus sp.',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46459700
comp[['Beringius turtoni']] <- data.frame(taxa = 'Beringius turtoni',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/49109568
comp[['Bolocera sp.']] <- data.frame(taxa = 'Bolocera sp.',
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
comp[['Buccinum sp.']] <- data.frame(taxa = 'Buccinum sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/598045
comp[['Caberea ellisii']] <- data.frame(taxa = 'Caberea ellisii',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46450434
comp[['Colga villosa']] <- data.frame(taxa = 'Colga villosa',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/46459938
comp[['Colus sp.']] <- data.frame(taxa = 'Colus sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# meduse: https://eol.org/pages/46554310/data
comp[['Cyanea capillata']] <- data.frame(taxa = 'Cyanea capillata',
structure = 'tissue', composition = 'calcium sulfate hemihydrate')

# https://eol.org/pages/46450774
comp[['Dendronotus sp.']] <- data.frame(taxa = 'Dendronotus sp.',
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

#
comp[['Gadus ogac']] <- data.frame(taxa = 'Gadus ogac',
structure = 'solid', composition = 'cartilaginous')

# https://eol.org/pages/439714
comp[['Gonatus fabricii']] <- data.frame(taxa = 'Gonatus fabricii',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/1163432
comp[['Halichondria panicea']] <- data.frame(taxa = 'Halichondria panicea',
structure = 'skeleton', composition = 'biogenic silica')

# https://eol.org/pages/46495230
comp[['Hamingia arctica']] <- data.frame(taxa = 'Hamingia arctica',
structure = 'hydroskeleton', composition = 'non-calcifying')

# https://eol.org/pages/46555727
comp[['Hemithiris psittacea']] <- data.frame(taxa = 'Hemithiris psittacea',
structure = 'tissue', composition = 'calcite')

# https://eol.org/pages/704318
comp[['Hormathia nodosa']] <- data.frame(taxa = 'Hormathia nodosa',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/451352
comp[['Illex illecebrosus']] <- data.frame(taxa = 'Illex illecebrosus',
structure = 'tissue', composition = 'aragonite')

# bird
comp[['Larus sp.']] <- data.frame(taxa = 'Larus sp.',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/2550449
comp[['Liponema multicorne']] <- data.frame(taxa = 'Liponema multicorne',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/401186
comp[['Margarites sp.']] <- data.frame(taxa = 'Margarites sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46530090
comp[['Melita dentata']] <- data.frame(taxa = 'Melita dentata',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/421495
comp[['Metridium senile']] <- data.frame(taxa = 'Metridium senile',
structure = 'tissue', composition = 'non-calcifying')

# bird
comp[['Morus bassanus']] <- data.frame(taxa = 'Morus bassanus',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/590174
# https://eol.org/pages/47012942
comp[['Naticidae']] <- data.frame(taxa = 'Naticidae',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46460933
comp[['Neptunea sp.']] <- data.frame(taxa = 'Neptunea sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/502990
comp[['Nucella lapillus']] <- data.frame(taxa = 'Nucella lapillus',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/511389
comp[['Nymphon sp.']] <- data.frame(taxa = 'Nymphon sp.',
structure = 'tissue', composition = 'non-calcifying')

# bird
comp[['Oceanites sp.']] <- data.frame(taxa = 'Oceanites sp.',
structure = 'skeleton', composition = 'bone')

# https://eol.org/pages/453373
comp[['Ommastrephes sp.']] <- data.frame(taxa = 'Ommastrephes sp.',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/46450364
comp[['Palio dubia']] <- data.frame(taxa = 'Palio dubia',
structure = 'tissue', composition = 'non-calcifying')

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
comp[['Polynoidae']] <- data.frame(taxa = 'Polynoidae',
structure = 'hydroskeleton', composition = 'non-calcifying')

# https://eol.org/pages/45322562
# https://eol.org/pages/46477078
comp[['Porifera']] <- data.frame(taxa = 'Porifera',
structure = 'skeleton', composition = 'biogenic silica')

# https://eol.org/pages/46552246
comp[['Ptychogena lactea']] <- data.frame(taxa = 'Ptychogena lactea',
structure = 'tissue', composition = 'calcium phosphate')

# https://eol.org/pages/46543649
comp[['Pycnogonum litorale']] <- data.frame(taxa = 'Pycnogonum litorale',
structure = 'tissue', composition = 'non-calcifying')

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

# tissue
comp[['Staurostoma mertensii']] <- data.frame(taxa = 'Staurostoma mertensii',
structure = 'tissue', composition = 'calcium phosphate')

# https://eol.org/pages/493012
comp[['Stauroteuthis syrtensis']] <- data.frame(taxa = 'Stauroteuthis syrtensis',
structure = 'tissue', composition = 'aragonite')

# https://eol.org/pages/54304
comp[['Stephanauge sp.']] <- data.frame(taxa = 'Stephanauge sp.',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/421131
comp[['Stomphia coccinea']] <- data.frame(taxa = 'Stomphia coccinea',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/45322457
comp[['Stylocordyla borealis']] <- data.frame(taxa = 'Stylocordyla borealis',
structure = 'skeleton', composition = 'biogenic silica')

# https://eol.org/pages/51510532
comp[['Tachyrhynchus erosus']] <- data.frame(taxa = 'Tachyrhynchus erosus',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/46555849
comp[['Terebratulina septentrionalis']] <- data.frame(taxa = 'Terebratulina septentrionalis',
structure = 'tissue', composition = 'calcite')

# https://eol.org/pages/46552783
comp[['Thuiaria thuja']] <- data.frame(taxa = 'Thuiaria thuja',
structure = 'tissue', composition = 'calcium phosphate')

# https://eol.org/pages/46447389
comp[['Tonicella sp.']] <- data.frame(taxa = 'Tonicella sp.',
structure = c('tissue','skeleton'), composition = c('calcium carbonate','aragonite'))

# https://eol.org/pages/46461545
comp[['Tritia sp.']] <- data.frame(taxa = 'Tritia sp.',
structure = 'skeleton', composition = 'calcium carbonate')

# https://eol.org/pages/51258428
comp[['Urticina felina']] <- data.frame(taxa = 'Urticina felina',
structure = 'tissue', composition = 'non-calcifying')

# https://eol.org/pages/596153
comp[['Velutinidae']] <- data.frame(taxa = 'Velutinidae',
structure = 'skeleton', composition = 'calcium carbonate')

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
# Single data.frme
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
if (!all(body$taxa == sp$species)) stop('Species are not the same between body dataset and species list')
rownames(body) <- body$taxa
body <- body %>%
        select(-taxa) %>%
        as.matrix()


# Export
save(body, file = './Data/SpeciesTraits/BodyComposition.RData')
