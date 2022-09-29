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
feed
# CaRNS St. Lawrence species check list :
# http://www.marinespecies.org/carms/aphia.php?p=checklist&action=search&gu_id=10178&tRank=220&inc_sub=1&status=pv
# http://www.marinespecies.org/aphia.php?p=taxdetails&id=118827#notes
tr['Aega psora', 1] <- 'parasite'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Alcyonidium
tr['Alcyonidium sp.', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=159928#attributes
tr['Amicula vestita', 1] <- 'grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Arctica%20islandica
tr['Arctica islandica', 1] <- 'suspension'

# From pandalus borealis diet: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Pandalus%20borealis
tr['Argis dentata', 1] <- 'scavenger | deposit | plankton | grazer'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Aristaeopsis%20edwardsiana
#tr['Aristaeopsis edwardsiana', 1] <- 'scavenger | deposit | plankton | grazer'

# From pandalus borealis diet: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Pandalus%20borealis
tr['Atlantopandalus propinqvus', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=490735#attributes
tr['Aulacofusus brevicauda', 1] <- 'predator | scavenger'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Balaena%20mysticetus
tr['Balaena mysticetus', 1] <- 'predator | filter'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=106057#attributes
tr['Balanidae', 1] <- 'suspension'

# Bivalvia: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Bathyarca%20glacialis
#tr['Bathyarca sp.', 1] <- 'deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=138855#attributes
tr['Beringius turtoni', 1] <- 'predator | scavenger'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Boreomysis
tr['Boreomysis sp.', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=111230#notes
tr['Bryozoa', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=111230#notes
tr['Caberea ellisii', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=137734#attributes
tr['Cardium sp.', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Ceratias%20holboelli
tr['Ceratias holboelli', 1] <- 'predator'

# From Cucumaria frondosa
tr['Chiridota laevis', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=139000#attributes
#tr['Ciliatocardium ciliatum', 1] <- 'suspension'

# Bivalvia: from Mytilus sp.
#tr['Crenella faba', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cryptopsaras%20couesii
tr['Cryptopsaras couesii', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cyclocardia
tr['Cyclocardia borealis', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=140102#attributes
#tr['Cyrtodaria siliqua', 1] <- 'suspension'

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

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127286#notes
tr['Gonostomatidae', 1] <- 'plankton'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=110364#attributes
tr['Hamingia arctica', 1] <- 'suspension | deposit'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Howella%20sherborni
tr['Howella sherborni', 1] <- 'predator'

# Decapoda
tr['Hymenopenaeus debilis', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=103251#attributes
tr['Hyperia galba', 1] <- 'parasite'

# Decapoda
tr['Lebbeus groenlandicus', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Lebbeus microceros', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Lebbeus polaris', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127191#notes
tr['Leptagonus decagonus', 1] <- 'predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Mactromeris
tr['Mactromeris polynyma', 1] <- 'deposit'

# Amphipoda: From Neohela monstrosa
tr['Maera loveni', 1] <- 'deposit'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Melanostigma%20atlanticum
# http://www.marinespecies.org/aphia.php?p=taxdetails&id=127120#notes
tr['Melanostigma atlanticum', 1] <- 'plankton | predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Platytroctidae
tr['Mentodus rostratus', 1] <- 'plankton | predator'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Mesodesma
#tr['Mesodesma sp.', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=100982#notes
tr['Metridium senile', 1] <- 'suspension | predator'

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

# From other sea stars
tr['Novodinia americana', 1] <- 'predator'

# From Epimeria loricata
tr['Oediceros saginatus', 1] <- 'predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=182807#attributes
tr['Palio dubia', 1] <- 'predator'

# Decapoda
#tr['Pandalus borealis', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
#tr['Pandalus montagui', 1] <- 'scavenger | deposit | plankton | grazer'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=140105#attributes       -> bivalvia
tr['Panomya norvegica', 1] <- 'suspension'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=181343#attributes       -> bivalvia
tr['Parvicardium pinnulatum', 1] <- 'suspension'

# Decapoda
#tr['Pasiphaea multidentata', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
#tr['Pasiphaea tarda', 1] <- 'scavenger | deposit | plankton | grazer'

# From: Aphroditella hastata
tr['Polynoidae', 1] <- 'predator'

# Decapoda
#tr['Pontophilus norvegicus', 1] <- 'scavenger | deposit | plankton | grazer'

# From Cucumaria frondosa
tr['Psolus fabricii', 1] <- 'suspension'

# From Pteraster militaris
tr['Pteraster obscurus', 1] <- 'predator'

# From bryozoa
tr['Reteporella grimaldii', 1] <- 'suspension'

# Decapoda
tr['Sabinea sarsii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Sabinea septemcarinata', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Sclerocrangon boreas', 1] <- 'scavenger | deposit | plankton | grazer'

# From bryozoa
tr['Securiflustra securifrons', 1] <- 'suspension'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Sepiida
#tr['Sepioloidea sp.', 1] <- 'predator'

# Decapoda
#tr['Sergia robusta', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris liljeborgii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris phippsii', 1] <- 'scavenger | deposit | plankton | grazer'

# Decapoda
tr['Spirontocaris spinus', 1] <- 'scavenger | deposit | plankton | grazer'

# From Ptychogena lactea
tr['Staurostoma mertensii', 1] <- 'predator'

#
tr['Syscenus infelix', 1] <- 'parasite'

# From: https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Cerithioidea
tr['Tachyrhynchus erosus', 1] <- 'deposit | predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=141607#attributes
#tr['Teredo navalis', 1] <- 'xylophagous'

# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Thysanoessa%20longicaudata
#tr['Thysanoessa longicaudata', 1] <- 'scavenger | deposit | plankton | grazer'

# From other sea stars
tr['Tremaster mirabilis', 1] <- 'predator'

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=737284#attributes
tr['Tritia sp.', 1] <- 'predator | scavenger'

# From other sea stars
tr['Urasterias lincki', 1] <- 'predator'

# Amphipoda From Neohela monstrosa
tr['Wimvadocus torelli', 1] <- 'deposit'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=126714#notes
# https://www.globalbioticinteractions.org/?interactionType=eats&sourceTaxon=Xenodermichthys%20copei
tr['Xenodermichthys copei', 1] <- 'plankton | predator'

# http://www.marinespecies.org/aphia.php?p=taxdetails&id=156497#attributes
tr['Xylophaga atlantica', 1] <- 'xylophagous'

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=141988#attributes
#tr['Yoldia sp.', 1] <- 'deposit'

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
see_missingsp
#write.csv(see_missingsp,file="FeedingType_ManualEntry.csv")
feeding
# Export
save(feeding, file = './Data/SpeciesTraits/FeedingType.RData')
