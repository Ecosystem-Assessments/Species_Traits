# source('./Code/Traits/Mobility.R')
# Invertebrate traits DB
inv <- read.delim('./Data/InvertebratesTraits/BD_traits_20200416.csv', stringsAsFactors = F)
invSp <- inv$Species
head(inv)
# Fish traits DB
fish <- readRDS('./Data/FishTraits/Data_trait_fb_3OCC_bathyend_TL_CML.RDS')
fishSp <- paste(fish$Genus, fish$Species)

# Check species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

# Species for which traits are available
spTraits <- c(fishSp, invSp)

# =-=-=-=-=-=-=-=-=-=- Mobility -=-=-=-=-=-=-=-=-=-= #
# Data frame
mob <- c('sessile', 'crawler', 'swimmer', 'burrower', 'crawler_swimmer', 'mobile')
mobility <- matrix(nrow = nSp, ncol = length(mob), dimnames = list(spList$species, mob))

# Insert invertebrate traits DB
for(i in spList$species) {
  if (i %in% inv$Species) {
    uid <- which(inv$Species == i)
    mobility[i, ] <- unlist(c(inv[uid, c('SESS','CRA','SWI','BURR','CS')], 0))
  }
}

# Insert fish traits DB
# The information is not present for mobility, so we will assume that all fish
# species are mobile
for(i in spList$species) {
  if (i %in% fishSp) {
    uid <- which(fishSp == i)
    mobility[i, ] <- 0
    mobility[i, 'mobile'] <- 1
  }
}

# Marine mammals as mobile species
# Marine mammals list
mm <- c("Balaenoptera musculus", "Balaenoptera physalus", "Megaptera novaeangliae",
        "Balaenoptera acutorostrata", "Eubalaena glacialis", "Balaenoptera borealis",
        "Balaena mysticetus", "Stenella frontalis", "Lagenorhynchus acutus", "Delphinapterus leucas",
        "Tursiops truncatus", "Ziphius cavirostris", "Phocoena phocoena", "Orcinus orca",
        "Globicephala melas", "Monodon monoceros", "Hyperoodon ampullatus", "Kogia breviceps",
        "Grampus griseus", "Delphinus delphis", "Physeter macrocephalus", "Stenella coeruleoalba",
        "Lagenorhynchus albirostris", "Erignathus barbatus", "Halichoerus grypus", "Phoca vitulina",
        "Pagophilus groenlandicus", "Cystophora cristata", "Pusa hispida", "Odobenus rosmarus")

# species are mobile
for(i in mm) {
  mobility[i, ] <- 0
  mobility[i, 'mobile'] <- 1
}

# Manual entries for the rest
uid <- is.na(mobility[,1])
nm <- rownames(mobility)[uid]
tr <- matrix(data = 0, nrow = length(nm), ncol = 6, dimnames = list(nm, colnames(mobility)))

# Entries
tr['Aega psora', 'mobile'] <- 1 # Fish parasite (swimmer for our purposes)
tr['Alcyonidium sp.', 'sessile'] <- 1 # Bryozoaire
tr['Amicula vestita', 'crawler'] <- 1
tr['Ammodytes sp.', 'mobile'] <- 1
tr['Ampelisca sp.', 'swimmer'] <- 1
tr['Antalis sp.', 'burrower'] <- 1
tr['Arctica islandica', 'burrower'] <- 1
tr['Argis dentata', 'mobile'] <- 1
tr['Aristaeopsis edwardsiana', 'mobile'] <- 1
tr['Artediellus sp.', 'mobile'] <- 1
tr['Aspidophoroides monopterygius', 'mobile'] <- 1
tr['Aspidophoroides olrikii', 'mobile'] <- 1
tr['Asterias sp.', 'crawler'] <- 1
tr['Astropecten duplicatus', 'crawler'] <- 1
tr['Atlantopandalus propinqvus', 'mobile'] <- 1
tr['Aulacofusus brevicauda', c('crawler','burrower')] <- 1
tr['Balanidae', 'sessile'] <- 1
tr['Bathyarca sp.', 'swimmer'] <- 1
tr['Bathypolypus sp.', 'mobile'] <- 1
tr['Bathyraja spinicauda', 'mobile'] <- 1
tr['Benthodesmus elongatus', 'mobile'] <- 1
tr['Beringius turtoni', c('crawler','burrower')] <- 1
tr['Boreomysis sp.', 'mobile'] <- 1
tr['Bryozoa', 'sessile'] <- 1
tr['Caberea ellisii', 'sessile'] <- 1
tr['Cancer borealis', 'mobile'] <- 1
tr['Cancer irroratus', 'mobile'] <- 1
tr['Cardium sp.', 'swimmer'] <- 1
tr['Chiridota laevis', 'crawler'] <- 1
tr['Ciliatocardium ciliatum', 'swimmer'] <- 1
tr['Crangon septemspinosa', 'mobile'] <- 1
tr['Crenella faba', 'swimmer'] <- 1
tr['Cyclocardia borealis', 'swimmer'] <- 1
tr['Cyrtodaria siliqua', 'burrower'] <- 1
tr['Dipturus linteus', 'mobile'] <- 1
tr['Ensis leei', 'swimmer'] <- 1
tr['Epizoanthus erdmanni', 'sessile'] <- 1
tr['Epizoanthus incrustatus', 'sessile'] <- 1
tr['Eualus fabricii', 'mobile'] <- 1
tr['Eualus gaimardii', 'mobile'] <- 1
tr['Eualus macilentus', 'mobile'] <- 1
tr['Eudistoma vitreum', 'sessile'] <- 1
tr['Eumicrotremus spinosus', 'mobile'] <- 1
tr['Eusergestes arcticus', 'mobile'] <- 1
tr['Gaidropsarus sp.', 'mobile'] <- 1
tr['Gasterosteus aculeatus aculeatus', 'mobile'] <- 1
tr['Gonatus fabricii', 'mobile'] <- 1
tr['Gonostomatidae', 'mobile'] <- 1
tr['Gymnocanthus tricuspis', 'mobile'] <- 1
tr['Halichondria panicea', 'sessile'] <- 1
tr['Hamingia arctica', 'crawler'] <- 1
tr['Hemitripterus americanus', 'mobile'] <- 1
tr['Homarus americanus', 'mobile'] <- 1
tr['Hymenopenaeus debilis', 'mobile'] <- 1
tr['Hyperia galba', 'swimmer'] <- 1
tr['Icelus sp.', 'mobile'] <- 1
tr['Illex illecebrosus', 'mobile'] <- 1
tr['Kajikia albida', 'mobile'] <- 1
tr['Lampanyctus sp.', 'mobile'] <- 1
tr['Larus sp.', 'mobile'] <- 1
tr['Lebbeus groenlandicus', 'mobile'] <- 1
tr['Lebbeus microceros', 'mobile'] <- 1
tr['Lebbeus polaris', 'mobile'] <- 1
tr['Liparis liparis liparis', 'mobile'] <- 1
tr['Macoma sp.', 'swimmer'] <- 1
tr['Mactromeris polynyma', 'burrower'] <- 1
tr['Maera loveni', 'swimmer'] <- 1
tr['Melita dentata', 'swimmer'] <- 1
tr['Mercenaria mercenaria', 'burrower'] <- 1
tr['Mesodesma sp.', 'burrower'] <- 1
tr['Metridium senile', 'sessile'] <- 1
tr['Modiolus modiolus', 'swimmer'] <- 1
tr['Molpadia sp.', 'crawler'] <- 1
tr['Morus bassanus', 'mobile'] <- 1
tr['Munida valida', 'mobile'] <- 1
tr['Mya arenaria', 'burrower'] <- 1
tr['Mya truncata', 'burrower'] <- 1
tr['Myctophidae', 'mobile'] <- 1
tr['Myoxocephalus sp.', 'mobile'] <- 1
tr['Myxine glutinosa', 'mobile'] <- 1
tr['Neolithodes grimaldii', 'mobile'] <- 1
tr['Novodinia americana', 'crawler'] <- 1
tr['Nucella lapillus', 'crawler'] <- 1
tr['Oceanites sp.', 'mobile'] <- 1
tr['Oediceros saginatus', 'swimmer'] <- 1
tr['Ommastrephes sp.', 'mobile'] <- 1
tr['Osmerus mordax mordax', 'mobile'] <- 1
tr['Palio dubia', 'crawler'] <- 1
tr['Pandalus borealis', 'mobile'] <- 1
tr['Pandalus montagui', 'mobile'] <- 1
tr['Panomya norvegica', 'burrower'] <- 1
tr['Paraliparis copei copei', 'mobile'] <- 1
tr['Parvicardium pinnulatum', 'swimmer'] <- 1
tr['Pasiphaea multidentata', 'mobile'] <- 1
tr['Pasiphaea tarda', 'mobile'] <- 1
tr['Petromyzon marinus', 'mobile'] <- 1
tr['Plesionika martia', 'mobile'] <- 1
tr['Pleurobrachia pileus', 'mobile'] <- 1 # Upper waters at night
tr['Polynoidae', 'crawler'] <- 1
tr['Pontophilus norvegicus', 'mobile'] <- 1
tr['Psolus fabricii', 'sessile'] <- 1
tr['Pteraster obscurus', 'crawler'] <- 1
tr['Pycnogonum litorale', 'crawler'] <- 1
tr['Reteporella grimaldii', 'sessile'] <- 1
tr['Sabinea sarsii', 'mobile'] <- 1
tr['Sabinea septemcarinata', 'mobile'] <- 1
tr['Sclerocrangon boreas', 'mobile'] <- 1
tr['Scomberesox saurus saurus', 'mobile'] <- 1
tr['Sebastes sp.', 'mobile'] <- 1
tr['Securiflustra securifrons', 'sessile'] <- 1
tr['Sepioloidea sp.', 'mobile'] <- 1
tr['Sergia robusta', 'mobile'] <- 1
tr['Serrivomer beanii', 'mobile'] <- 1
tr['Spirontocaris liljeborgii', 'mobile'] <- 1
tr['Spirontocaris phippsii', 'mobile'] <- 1
tr['Spirontocaris spinus', 'mobile'] <- 1
tr['Spisula solidissima', 'burrower'] <- 1
tr['Staurostoma mertensii', 'sessile'] <- 1 # check name
tr['Stichaeus punctatus punctatus', 'mobile'] <- 1
tr['Stomias boa ferox', 'mobile'] <- 1
tr['Syscenus infelix', 'mobile'] <- 1 # fish parasite
tr['Tachyrhynchus erosus', 'swimmer'] <- 1
tr['Tautogolabrus adspersus', 'mobile'] <- 1
tr['Tellina sp.', 'swimmer'] <- 1
tr['Teredo navalis', 'burrower'] <- 1
tr['Thysanoessa longicaudata', 'swimmer'] <- 1
tr['Tonicella sp.', 'crawler'] <- 1
tr['Tremaster mirabilis', 'crawler'] <- 1
tr['Triglops sp.', 'mobile'] <- 1
tr['Tritia sp.', 'swimmer'] <- 1
tr['Urasterias lincki', c('crawler','burrower')] <- 1
tr['Urticina felina', 'sessile'] <- 1
tr['Wimvadocus torelli', 'swimmer'] <- 1
tr['Xiphias gladius', 'mobile'] <- 1
tr['Xylophaga atlantica', 'swimmer'] <- 1
tr['Yoldia sp.', 'swimmer'] <- 1

# Insert to mobility DB
for(i in nm) mobility[i, ] <- tr[i, ]

# Drop crawler_swimmer col
uid <- mobility[, 'crawler_swimmer'] == 1
mobility[uid, c('crawler','swimmer')] <- 1
mobility <- mobility[, colnames(mobility) != 'crawler_swimmer']


mobility_manual_entry <- read.csv('./Data/SpeciesTraits/Mobility_ManualEntry.csv', sep=",", row.names = NULL)
mobility_manual_entry <- mobility_manual_entry[,1:6] #remove comment for integration in db
mobility_manual_entry <- as.matrix( mobility_manual_entry)
mobility_manual_entry2 <- mobility_manual_entry[,-1]
rownames(mobility_manual_entry2) <- mobility_manual_entry[,1]

warning("Need to combine manual entry (mobility_manual_entry2 matrix) from csv file into DB")

#Verify if the dataset is complete
row_sub = apply(mobility, 1, function(row) all(row !=1 ))
see_missingsp=mobility[row_sub,]
#write.csv(see_missingsp,file="Mobility_ManualEntry.csv")

# Export
save(mobility, file = './Data/SpeciesTraits/Mobility.RData')
