# source('./Code/Traits/Environment.R')
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(sp)

# =-=-=-=-=-=-=-=-=-=- Environment from species -=-=-=-=-=-=-=-=-=-= #
library(rfishbase)
cl <- c("DemersPelag")


# Fishbase / sealifebase habitat classification
#   pelagic: occurring mainly in the water column between 0 and 200 m, not feeding on benthic organisms;
#   benthopelagic: living and/or feeding on or near the bottom, as well as in midwater, between 0 and 200 m;
#   demersal: living and/or feeding on or near the bottom, between 0 and 200 m;
#   reef-associated: living and/or feeding on or near reefs, between 0 and 200 m;
#   bathydemersal: living and/or feeding on or near the bottom, below 200 m.
#   bathypelagic: occurring mainly in open water below 200 m, not feeding on benthic organisms; and


# Fishbase
fb <- sb <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(sp$species, cl))
for(i in 1:nSp) {
  # Fishbase
  dat <- species(sp$species[i], server = 'fishbase', fields = cl)
  if (nrow(dat) > 1) {
    fb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    fb[i,] <- unlist(as.data.frame(dat))
  }

  # Sealifebase
  dat <- species(sp$species[i], server = 'sealifebase', fields = cl)
  if (nrow(dat) > 1) {
    sb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    sb[i,] <- unlist(as.data.frame(dat))
  }
}

# Merge datasets
env <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(sp$species, cl))
for(i in 1:nrow(env)) {
  if (any(!is.na(fb[i, ]))) {
    env[i, ] <- as.matrix(fb[i, ])
  } else if (any(!is.na(sb[i, ]))) {
    env[i, ] <- as.matrix(sb[i, ])
  } else {
    next
  }
}

# =-=-=-=-=-=-=-=-=-=- Environment from genus -=-=-=-=-=-=-=-=-=-= #
# Missing taxa
uid <- apply(env, 1, function(x) !any(!is.na(x)))
nm <- rownames(env)[uid]

# Select only genus
gn <- gsub('\\s(.*)', '', nm)

# Extract length for all species in genus
fbgn <- sbgn <- vector('list', length(gn))
for(i in 1:length(gn)) {
  # Fishbase
  spid <- species_list(Genus = gn[i], server = 'fishbase')
  fbgn[[i]] <- species(spid, server = 'fishbase', fields = cl)

  # Sealifebase
  spid <- species_list(Genus = gn[i], server = 'sealifebase')
  sbgn[[i]] <- species(spid, server = 'sealifebase', fields = cl)
}

# All environments from genera
library(tidyverse)
library(magrittr)
for(i in 1:length(gn)) {
  # Fishbase
  fbgn[[i]] <- fbgn[[i]] %>%
               filter(!is.na(DemersPelag)) %>%
               unique() %>%
               as.matrix() %>%
               sort() %>%
               paste(collapse = ' | ')

  # Sealifebase
  sbgn[[i]] <- sbgn[[i]] %>%
               filter(!is.na(DemersPelag)) %>%
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
envgn <- matrix(data = NA, nrow = length(nm), ncol = length(cl), dimnames = list(nm, cl))
for(i in 1:nrow(envgn)) {
  if (any(!is.na(fbgn[[i]]))) {
    envgn[i, ] <- as.matrix(fbgn[[i]])
  } else if (any(!is.na(sbgn[[i]]))) {
    envgn[i, ] <- as.matrix(sbgn[[i]])
  } else {
    next
  }
}

# Merge with species scale dataset
for(i in rownames(envgn)) env[i, ] <- envgn[i, ]


# Missing taxa
uid <- apply(env, 1, function(x) !any(!is.na(x)))
nm <- rownames(env)[uid]
tr <- matrix(data = '', nrow = length(nm), ncol = 1, dimnames = list(nm, colnames(env)))

# Entries
tr["Aphroditella hastata", 1] <- 'benthic'
tr["Arrhoges occidentalis", 1] <- 'benthic'
tr["Ascidiacea", 1] <- 'benthic'
tr["Aulacofusus brevicauda", 1] <- 'benthic'
tr["Balanidae", 1] <- 'benthic'
tr["Bryozoa", 1] <- 'benthic'
tr["Colga villosa", 1] <- 'benthic'
tr["Eusergestes arcticus", 1] <- 'pelagic'
tr["Gonostomatidae", 1] <- 'bathypelagic'
tr["Myctophidae", 1] <- 'bathypelagic'
tr["Naticidae", 1] <- 'benthic'
tr["Polynoidae", 1] <- 'benthic'
tr["Porifera", 1] <- 'benthic'
tr["Staurostoma mertensii", 1] <- 'benthopelagic'
tr["Tritia sp.", 1] <- 'benthic'
tr["Velutinidae", 1] <- 'benthic'
tr["Wimvadocus torelli", 1] <- 'benthic'

# Insert to environment DB
for(i in nm) env[i, ] <- tr[i, ]

# Resolve taxa environments
# "host" are fish parasites, will put them as "demersal" and "pelagic"
env[,1] <- gsub('host','benthic | pelagic', env[,1])

# "sessile" taxa are benthic species
env[,1] <- gsub('sessile','benthic', env[,1])

# 'others' are seabirds, so will categorize them as pelagic
env[,1] <- gsub('others','pelagic', env[,1])

# 'reef-associated' fish will be categorized as demersal for the St. Lawrence
env[,1] <- gsub('reef-associated','demersal', env[,1])

# 'pelagic-neretic' and 'pelagic-oceanic' will be categorized as 'pelagic'
env[,1] <- gsub('pelagic-neritic','pelagic', env[,1])
env[,1] <- gsub('pelagic-oceanic','pelagic', env[,1])

# Environment types
envType <- paste(env[,1], collapse = ' | ') %>%
           stringr::str_split(pattern = ' \\| ') %>%
           unlist() %>%
           unique() %>%
           sort()

environment <- matrix(data = 0, nrow = nSp, ncol = length(envType),
                      dimnames = list(sp$species, envType))

for(i in envType) environment[, i] <- stringr::str_detect(env[,1], i)

# Export
save(environment, file = './Data/SpeciesTraits/Environment.RData')
