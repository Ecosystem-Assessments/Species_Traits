# source('./Code/Traits/FeedingType.R')
library(magrittr)
# Species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

# =-=-=-=-=-=-=-=-=-=- Feeding types considered -=-=-=-=-=-=-=-=-=-= #
# suspension: particulate matter suspension/filter feeders
# deposit: particulate matter deposit feeders
# predator: hunting macrofauna
# scavenger: feeds on carrion, dead plant material, or refuse.
# grazer: feeds on growing plants
# parasite: lives in or on a host species
# plankton: selective plankton feeding
# filter: unselective plankton filtering
# xylophagous: feeding on or boring into wood
feedType <- c('suspension','deposit','predator','scavenger','grazer','parasite','plankton','filter','xylophagous') %>% sort()
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #


# =-=-=-=-=-=-=-=-=-=-  Get data from fishbase and sealifebase -=-=-=-=-=-=-=-=-=-= #
library(rfishbase)
cl <- 'FeedingType'
fb <- sb <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
for(i in 1:nSp) {
  # Fishbase
  dat <- ecology(spList$species[i], server = 'fishbase', fields = cl)
  if (nrow(dat) > 1) {
    fb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    fb[i,] <- unlist(as.data.frame(dat))
  }

  # Sealifebase
  dat <- ecology(spList$species[i], server = 'sealifebase', fields = cl)
  if (nrow(dat) > 1) {
    sb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    sb[i,] <- unlist(as.data.frame(dat))
  }
}

# Merge datasets
feed <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
for(i in 1:nrow(feed)) {
  if (any(!is.na(fb[i, ]))) {
    feed[i, ] <- as.matrix(fb[i, ])
  } else if (any(!is.na(sb[i, ]))) {
    feed[i, ] <- as.matrix(sb[i, ])
  } else {
    next
  }
}

# NAs for other and variable
feed[feed %in% 'other', ] <- NA
feed[feed %in% 'variable', ] <- NA

# =-=-=-=-=-=-=-=-=-=- Feeding types from genus -=-=-=-=-=-=-=-=-=-= #
# Missing taxa
uid <- apply(feed, 1, function(x) !any(!is.na(x)))
nm <- rownames(feed)[uid]

# Select only genus
gn <- gsub('\\s(.*)', '', nm)

# Extract length for all species in genus
fbgn <- sbgn <- vector('list', length(gn))
for(i in 1:length(gn)) {
  # Fishbase
  spid <- species_list(Genus = gn[i], server = 'fishbase')
  fbgn[[i]] <- ecology(spid, server = 'fishbase', fields = cl)

  # Sealifebase
  spid <- species_list(Genus = gn[i], server = 'sealifebase')
  sbgn[[i]] <- ecology(spid, server = 'sealifebase', fields = cl)
}

# All feeding types from genera
library(tidyverse)
library(magrittr)
for(i in 1:length(gn)) {
  # Fishbase
  fbgn[[i]] <- fbgn[[i]] %>%
               filter(!is.na(FeedingType)) %>%
               unique() %>%
               as.matrix() %>%
               sort() %>%
               paste(collapse = ' | ')

  # Sealifebase
  sbgn[[i]] <- sbgn[[i]] %>%
               filter(!is.na(FeedingType)) %>%
               unique() %>%
               as.matrix() %>%
               sort() %>%
               paste(collapse = ' | ')
}

# Transform "" to NA
for(i in 1:length(gn)) {
  if (fbgn[[i]] == "") fbgn[[i]] <- NA
  if (sbgn[[i]] == "") sbgn[[i]] <- NA
}

# Merge datasets
feedgn <- matrix(data = NA, nrow = length(nm), ncol = length(cl), dimnames = list(nm, cl))
for(i in 1:nrow(feedgn)) {
  if (any(!is.na(fbgn[[i]]))) {
    feedgn[i, ] <- as.matrix(fbgn[[i]])
  } else if (any(!is.na(sbgn[[i]]))) {
    feedgn[i, ] <- as.matrix(sbgn[[i]])
  } else {
    next
  }
}

# Merge with species scale dataset
for(i in rownames(feedgn)) feed[i, ] <- feedgn[i, ]

# =-=-=-=-=-=-=-=-=-=- Modify feeding types names -=-=-=-=-=-=-=-=-=-= #
feed <- gsub("hunting macrofauna \\(predator\\)","predator", feed)
feed <- gsub("grazing on aquatic plants","grazer", feed)
feed <- gsub("selective plankton feeding","plankton", feed)
feed <- gsub("filtering plankton","filter", feed)
feed <- gsub("feeding on a host \\(parasite\\)","parasite", feed)
feed <- gsub("browsing on substrate","deposit", feed)
feed <- gsub("other","NA", feed)
feed <- gsub("variable","NA", feed)
feed[feed %in% 'other', ] <- NA
feed[feed %in% 'variable', ] <- NA
feed[feed %in% 'NA', ] <- NA

# =-=-=-=-=-=-=-=-=-=- Trophic Mode from invertebrates dataset -=-=-=-=-=-=-=-=-=-= #
# Invertebrate traits DB
inv <- read.delim('./Data/InvertebratesTraits/BD_traits_20200416.csv', stringsAsFactors = F)
invSp <- inv$Species

# Data frame
ft <- c('suspension','deposit','predator','scavenger')
feedinv <- matrix(nrow = nSp, ncol = length(ft), dimnames = list(spList$species, ft))

# Insert invertebrate traits DB
for(i in spList$species) {
  if (i %in% inv$Species) {
    uid <- which(inv$Species == i)
    feedinv[i, ] <- unlist(inv[uid, c('SF','DF','PRE','SCA')])
  }
}

# Identify species w/ data
datid <- !is.na(feedinv[,1])

# Feeding type as character in tables
for(i in 1:ncol(feedinv)) {
  # Replace 0 with NAs
  uid <- which(feedinv[,i] == 0)
  feedinv[uid, i] <- NA

  # Replace 1 with feeding type name
  uid <- which(feedinv[,i] == 1)
  feedinv[uid, i] <- colnames(feedinv)[i]
}

# Single line er taxa
feedinv <- as.matrix(apply(feedinv, 1, paste, collapse = ' | '))

# Insert in feeding type db
feed[datid, ] <- feedinv[datid, ]


# =-=-=-=-=-=-=-=-=-=- Manual entries for missing taxa -=-=-=-=-=-=-=-=-=-= #
# Missing taxa
uid <- is.na(feed)
nm <- rownames(feed)[uid]
tr <- matrix(data = '', nrow = length(nm), ncol = 1, dimnames = list(nm, colnames(feed)))

# CaRNS St. Lawrence species check list :

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Acanella arbuscula', 1] <- 'suspension'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Acanthogorgia armata', 1] <- 'suspension'

#https://eol.org/pages/51548193
tr['Actiniidae', 1] <- 'predator'

#https://www.sealifebase.ca/summary/Actinostola-callosa.html
tr['Actinostola', 1] <- 'predator'

# http://www.marinespecies.org/carms/aphia.php?p=checklist&action=search&gu_id=10178&tRank=220&inc_sub=1&status=pv
# http://www.marinespecies.org/aphia.php?p=taxdetails&id=118827#notes
tr['Aega psora', 1] <- 'parasite'

#algea
tr['Agarum cribrosum', 1] <- 'NA'

#Info from Agriopoma gatunensis https://eol.org/pages/46469894
tr['Agriopoma morrhuanum', 1] <- 'suspension'

#photosynthetic
tr['Alaria esculenta', 1] <- 'NA'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Alcyonidium
tr['Alcyonidium sp.', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=159928#attributes
tr['Amicula vestita', 1] <- 'grazer'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Anonyx
tr['Anonyx', 1] <- 'deposit'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Anthias%20nicholsi
tr['Anthias nicholsi', 1] <- 'predator'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Anthomastus grandiflorus', 1] <- 'suspension'

#omnivore: https://eol.org/pages/598186
tr['Arbacia punctulata', 1] <- 'predator | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Arctica%20islandica
tr['Arctica islandica', 1] <- 'suspension'

#https://eol.org/pages/464119
tr['Aphrodita hastata', 1] <- 'predator'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Arenicola%20marina
tr['Arenicola marina', 1] <- 'deposit | grazer'

# From pandalus borealis diet: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Pandalus%20borealis
tr['Argis dentata', 1] <- 'scavenger | deposit | plankton | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Aristaeopsis%20edwardsiana
#tr['Aristaeopsis edwardsiana', 1] <- 'scavenger | deposit | plankton | grazer'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Arrhis%20phyllonyx
tr['Arrhis phyllonyx', 1] <- 'deposit'

#algea
tr['Ascophyllum nodosum', 1] <- 'NA'

# From pandalus borealis diet: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Pandalus%20borealis
tr['Atlantopandalus propinqvus', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=490735#attributes
tr['Aulacofusus brevicauda', 1] <- 'predator | scavenger'

#small decapod
tr['Axius serratus', 1] <- 'deposit | grazer | plankton | scavenger'

#Assumed to be predator as it is a Ray-finned fish: https://www.fishbase.in/summary/Bajacalifornia-megalops.html
tr['Bajacalifornia megalops', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Balaena%20mysticetus
tr['Balaena mysticetus', 1] <- 'predator | filter'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=106057#attributes
tr['Balanidae', 1] <- 'suspension'

#From Spectrunculus grandis: https://eol.org/pages/46565609
tr['Bassogigas gilli', 1] <- 'predator'

#From genus Bathygadus: https://eol.org/pages/46564476 https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Bathygadus
tr['Bathygadus melanobranchus', 1] <- 'predator'

#decapod
tr['Bathynectes longispina', 1] <- 'scavenger | deposit | plankton | grazer'

#decapod
tr['Bathynectes maravigna', 1] <- 'scavenger | deposit | plankton | grazer'

#Info from the family: Alepocephalidae, https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Alepocephalidae
tr['Bathytroctes microlepis', 1] <- 'predator'

# Bivalvia: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Bathyarca%20glacialis
#tr['Bathyarca sp.', 1] <- 'deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=138855#attributes
tr['Beringius turtoni', 1] <- 'predator | scavenger'

#https://eol.org/pages/52236201
tr['Bolocera', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Boreomysis
tr['Boreomysis sp.', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=111230#notes
tr['Bryozoa', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=111230#notes
tr['Caberea ellisii', 1] <- 'suspension'

#https://www.fishbase.se/summary/1726
tr['Caelorinchus caelorinchus', 1] <- 'predator'

#https://www.marinespecies.org/aphia.php?p=taxdetails&id=118497#attributes
tr['Calathura brachiata', 1] <- 'parasite'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=137734#attributes
tr['Cardium sp.', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Ceratias%20holboelli
tr['Ceratias holboelli', 1] <- 'predator'

# From Cucumaria frondosa
tr['Chiridota laevis', 1] <- 'suspension'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Chrysogorgia agassizii', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=139000#attributes
#tr['Ciliatocardium ciliatum', 1] <- 'suspension'

#From order Molpadida: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Molpadida
tr['Caudina arenata', 1] <- 'suspension'

#From family – Cirroteuthidae: https://eol.org/pages/2320
tr['Cirroteuthis', 1] <- 'predator'


# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Colossendeis
tr['Colossendeis', 1] <- 'predator'

#From: Conocara macropterum: https://eol.org/pages/46562821
tr['Conocara salmonea', 1] <- 'predator'

# Bivalvia: from Mytilus sp.
#tr['Crenella faba', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cryptopsaras%20couesii
tr['Cryptopsaras couesii', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cyclocardia
tr['Cyclocardia borealis', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=140102#attributes
#tr['Cyrtodaria siliqua', 1] <- 'suspension'

#https://www.marinespecies.org/aphia.php?p=taxdetails&id=139523#attributes
tr['Dendronotus', 1] <- 'predator'

#algea
tr['Desmarestia aculeata', 1] <- 'NA'

#From the family Pandalidae, a family of decapods: https://eol.org/pages/46516024
tr['Dichelopandalus leptocerus', 1] <- 'deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=101027#attributes
tr['Epizoanthus erdmanni', 1] <- 'suspension'

# From: http://www.marinespecies.org/aphia.php?p=taxdetails&id=101027#attributes
tr['Epizoanthus incrustatus', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Eualus%20gaimardii
tr['Eualus fabricii', 1] <- 'scavenger | deposit | plankton | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Eualus%20gaimardii
tr['Eualus gaimardii', 1] <- 'scavenger | deposit | plankton | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Eualus%20gaimardii
tr['Eualus macilentus', 1] <- 'scavenger | deposit | plankton | grazer'

#tr['Eudistoma vitreum', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=159817#notes
tr['Eumesogrammus praecisus', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=eusergestes%20arcticus
tr['Eusergestes arcticus', 1] <- 'scavenger | deposit | plankton | grazer'

#https://eol.org/pages/200235
tr['Flabellum', 1] <- 'suspension'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Foetorepus%20agassizii
tr['Foetorepus agassizi', 1] <- 'predator'

#photosynthetic
tr['Fucus distichus', 1] <- 'NA'

#photosynthetic
tr['Fucus vesiculosus', 1] <- 'NA'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Funiculina quadrangularis', 1] <- 'suspension'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Gersemia fruticosa', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127286#notes
tr['Gonostomatidae', 1] <- 'plankton'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Gorgonocephalus
tr['Gorgonocephalus', 1] <- 'predator'

#From the family Stomiidae: https://eol.org/pages/5074
tr['Grammatostomias dentatus', 1] <- 'predator'

#To be verified but likely a predator: https://eol.org/pages/46571187
tr['Grammicolepis brachiusculus', 1] <- 'predator'

#https://eol.org/pages/46574733
tr['Gymnelis viridis', 1] <- 'predator'

#https://eol.org/pages/46566160
tr['Halieutichthys aculeatus', 1] <- 'predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=110364#attributes
tr['Hamingia arctica', 1] <- 'suspension | deposit'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Heteropolypus', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Homola%20minima
tr['Homola minima', 1] <- 'predator'

#From the family Platytroctidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Platytroctidae
tr['Holtbyrnia anomala', 1] <- 'predator'

#From: Howella sherborni https://www.fishbase.se/summary/Howella-sherborni.html AND https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Howellidae
tr['Howella brodiei', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Howella%20sherborni
tr['Howella sherborni', 1] <- 'predator'

# Decapoda
tr['Hymenopenaeus debilis', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=103251#attributes
tr['Hyperia galba', 1] <- 'parasite'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Ichnopus
tr['Ichnopus', 1] <- 'predator | deposit'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Keratoisis ornata', 1] <- 'suspension'

#algea
tr['Laminaria digitata', 1] <- 'NA'

#algea
tr['Laminaria longicruris', 1] <- 'NA'

#algea
tr['Leathesia difformis', 1] <- 'NA'

# Decapoda
tr['Lebbeus groenlandicus', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Lebbeus microceros', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Lebbeus polaris', 1] <- 'scavenger | deposit | plankton | grazer'

#decapod
tr['Lebbeus zebra', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127191#notes
tr['Leptagonus decagonus', 1] <- 'predator'

#From Leptasterias arctica: https://eol.org/pages/598474 and more generally https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Leptasterias
tr['Leptasterias', 1] <- 'predator'

#https://eol.org/pages/1244469 and from Lophaster: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Lophaster
tr['Lophaster furcifer', 1] <- 'predator'

#https://oceana.ca/en/marine-life/lophelia-coral/
tr['Lophelia pertusa', 1] <- 'suspension'

#https://www.fishbase.se/summary/333
tr['Macrorhamphosus scolopax', 1] <- 'predator'

#From the family Stomiidae: https://eol.org/pages/5074
tr['Macrostomias longibarbatus', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Mactromeris
tr['Mactromeris polynyma', 1] <- 'deposit'

# Amphipoda: From Neohela monstrosa
tr['Maera loveni', 1] <- 'deposit'

#https://eol.org/search?utf8=%E2%9C%93&q=Mediaster+bairdi
tr['Mediaster bairdi', 1] <- 'grazer | predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Melanostigma%20atlanticum
# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127120#notes
tr['Melanostigma atlanticum', 1] <- 'plankton | predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Platytroctidae
tr['Mentodus rostratus', 1] <- 'plankton | predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Mesodesma
#tr['Mesodesma sp.', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=100982#notes
tr['Metridium senile', 1] <- 'suspension | predator'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Monolene%20sessilicauda
tr['Monolene sessilicauda', 1] <- 'predator'

#https://www.marinespecies.org/aphia.php?p=taxdetails&id=119780#attributes
tr['Monstrilla', 1] <- 'parasite'

# From Cucumaria frondosa
tr['Molpadia sp.', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Morus%20bassanus
#tr['Morus bassanus', 1] <- 'predator'

# Decapoda
#tr['Munida valida', 1] <- 'scavenger | deposit | plankton | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Myctophidae
tr['Myctophidae', 1] <- 'plankton | predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=126811#notes
tr['Naucrates ductor', 1] <- 'predator | scavenger'

# crabe, as decapoda
#tr['Neolithodes grimaldii', 1] <- 'scavenger | deposit | plankton | grazer'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Neptunea
#https://eol.org/pages/46460077
tr['Neptunea', 1] <- 'scavenger | predator'

# From other sea stars
tr['Novodinia americana', 1] <- 'predator'

# From Epimeria loricata
tr['Oediceros saginatus', 1] <- 'predator'

#decapod
tr['Oncopagurus', 1] <- 'scavenger | deposit | plankton | grazer'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Oplophorus%20spinosus
tr['Oplophorus spinosus', 1] <- 'predator'

#Likely predator (to be verified) From the family – Ophiomusaidae https://eol.org/pages/51261745
tr['Ophiomusa lymani', 1] <- 'predator'

#https://eol.org/pages/46549709
tr['Pachycerianthus borealis', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=182807#attributes
tr['Palio dubia', 1] <- 'predator'

#algea
tr['Palmaria palmata', 1] <- 'NA'

# Decapoda
#tr['Pandalus borealis', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
#tr['Pandalus montagui', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=140105#attributes       -> bivalvia
tr['Panomya norvegica', 1] <- 'suspension'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Paragorgia arborea', 1] <- 'suspension'

#decapod
tr['Parapasiphae sulcatifrons', 1] <- 'scavenger | deposit | plankton | grazer'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Parathemisto
tr['Parathemisto', 1] <- 'predator | deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=181343#attributes       -> bivalvia
tr['Parvicardium pinnulatum', 1] <- 'suspension'

# Decapoda
#tr['Pasiphaea multidentata', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
#tr['Pasiphaea tarda', 1] <- 'scavenger | deposit | plankton | grazer'

#Phyllophoridae is a family of echinoderms. They are omnivores. https://eol.org/pages/2024 AND https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Phyllophoridae
tr['Pentamera calcigera', 1] <- 'suspension'

#amphipods: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Amphipods
tr['Phronima', 1] <- 'scavenger | deposit | plankton | grazer'

#From the family Astropectinidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Astropectinidae
tr['Plutonaster agassizi', 1] <- 'predator'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Polymetme%20corythaeola
tr['Polymetme corythaeola', 1] <- 'predator'

# From: Aphrodita hastata
tr['Polynoidae', 1] <- 'predator'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Pontaster
tr['Pontaster', 1] <- 'deposit'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Porania%20pulvillus
tr['Porania pulvillus', 1] <- 'predator'

#Need to be verified. From the family Poraniidae https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Poraniidae
tr['Poraniomorpha', 1] <- 'plankton'

# Decapoda
#tr['Pontophilus norvegicus', 1] <- 'scavenger | deposit | plankton | grazer'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Primnoa resedaeformis', 1] <- 'suspension'

#From the family Chiasmodontidae https://eol.org/pages/5346
tr['Pseudoscopelus', 1] <- 'predator'

# From Cucumaria frondosa
tr['Psolus fabricii', 1] <- 'suspension'

# From Pteraster militaris
tr['Pteraster obscurus', 1] <- 'predator'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Radicipes gracilis', 1] <- 'suspension'

# From bryozoa
tr['Reteporella grimaldii', 1] <- 'suspension'

#From the family Alepocephalidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Alepocephalidae
tr['Rouleina attrita', 1] <- 'predator'

#From the family Alepocephalidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Alepocephalidae
tr['Rouleina maderensis', 1] <- 'predator'

# Decapoda
tr['Sabinea sarsii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Sabinea septemcarinata', 1] <- 'scavenger | deposit | plankton | grazer'

#algea
tr['Saccorhiza dermatodea', 1] <- 'NA'

#From Sclerasterias euplecta: https://eol.org/pages/4702561
tr['Sclerasterias tanneri', 1] <- 'predator'

#From the family Cucumariidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cucumariidae
tr['Stereoderma unisemita', 1] <- 'suspension'

# Decapoda
tr['Sclerocrangon boreas', 1] <- 'scavenger | deposit | plankton | grazer'

# From bryozoa
tr['Securiflustra securifrons', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Sepiida
#tr['Sepioloidea sp.', 1] <- 'predator'

# Decapoda
#tr['Sergia robusta', 1] <- 'scavenger | deposit | plankton | grazer'

#cnidarians - to be verified if they are selective filter feeder or passive
tr['Sertularia polyzonias', 1] <- 'suspension'

#https://www.marinespecies.org/aphia.php?p=taxdetails&id=158664#notes
#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Simenchelys%20parasitica
tr['Simenchelys parasitica', 1] <- 'parasite | predator'

#decapod
tr['Spinolambrus pourtalesii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris fabricii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris liljeborgii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris phippsii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris spinus', 1] <- 'scavenger | deposit | plankton | grazer'

# From Ptychogena lactea
tr['Staurostoma mertensii', 1] <- 'predator'

# Decapoda
tr['Stereomastis sculpta', 1] <- 'scavenger | deposit | plankton | grazer'
#
tr['Syscenus infelix', 1] <- 'parasite'

# From: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cerithioidea
tr['Tachyrhynchus erosus', 1] <- 'deposit | predator'

#Need to be verified. From the genus: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Tealia
tr['Tealia felina', 1] <- 'predator | suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=141607#attributes
#tr['Teredo navalis', 1] <- 'xylophagous'

#From the family Cucumariidae: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cucumariidae
tr['Thyonella pervicax', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Thysanoessa%20longicaudata
#tr['Thysanoessa longicaudata', 1] <- 'scavenger | deposit | plankton | grazer'

# From other sea stars
tr['Tremaster mirabilis', 1] <- 'predator'

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=737284#attributes
tr['Tritia sp.', 1] <- 'predator | scavenger'

#algea
tr['Ulva', 1] <- 'NA'

# From other sea stars
tr['Urasterias lincki', 1] <- 'predator'

#https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Venefica%20procera
tr['Venefica procera', 1] <- 'predator'

#From the family Carditidae: https://eol.org/pages/46468509
tr['Venericardia borealis', 1] <- 'suspension'

#https://eol.org/pages/46578013
tr['Vomer setapinnis', 1] <- 'predator'

# Amphipoda From Neohela monstrosa
tr['Wimvadocus torelli', 1] <- 'deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=126714#notes
# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Xenodermichthys%20copei
tr['Xenodermichthys copei', 1] <- 'plankton | predator'

#Likely predator (to be verified) From the family – Grammicolepididae (Tinselfishes) https://eol.org/pages/5050
tr['Xenolepidichthys dalgleishi', 1] <- 'predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=156497#attributes
tr['Xylophaga atlantica', 1] <- 'xylophagous'

#http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=141988#attributes
#tr['Yoldia', 1] <- 'deposit'

#algea
tr['Zostera marina', 1] <- 'NA'

# Insert to feed DB
for(i in nm) feed[i, ] <- tr[i, ]

# =-=-=-=-=-=-=-=-=-=- Format db -=-=-=-=-=-=-=-=-=-= #
# DB
feeding <- matrix(data = 0, nrow = nSp, ncol = length(feedType),
                      dimnames = list(spList$species, feedType))

for(i in feedType) feeding[, i] <- stringr::str_detect(feed[,1], i)

#Verify if the dataset is complete
row_sub = apply(feeding, 1, function(row) all(row !=1 ))
see_missingsp=feeding[row_sub,]
# write.csv(see_missingsp,file="FeedingType_ManualEntry.csv")

# Export
save(feeding, file = './Data/SpeciesTraits/FeedingType.RData')
write.csv(feeding ,file='./Data/SpeciesTraits/FeedingType.csv')
