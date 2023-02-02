# source('./Code/Traits/Environment.R')
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

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
fb <- sb <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
for(i in 1:nSp) {
  # Fishbase
  dat <- species(spList$species[i], server = 'fishbase', fields = cl)
  if (nrow(dat) > 1) {
    fb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    fb[i,] <- unlist(as.data.frame(dat))
  }
  # Sealifebase
  dat <- species(spList$species[i], server = 'sealifebase', fields = cl)
  if (nrow(dat) > 1) {
    sb[i,] <- unlist(as.data.frame(dat[1, ]))
  } else {
    sb[i,] <- unlist(as.data.frame(dat))
  }
}

# Merge datasets
env <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
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
tr["Actiniidae", 1] <- 'benthic'
# tr["Aphrodita hastata", 1] <- 'benthic'
tr["Arrhoges occidentalis", 1] <- 'benthic'
tr["Ascidiacea", 1] <- 'benthic'
#tr["Aulacofusus brevicauda", 1] <- 'benthic'
tr["Axius serratus", 1] <- 'benthic'
tr["Balanidae", 1] <- 'benthic'
tr["Bryozoa", 1] <- 'benthic'
tr["Caelorinchus caelorinchus", 1] <- 'benthopelagic'
#tr["Colga villosa", 1] <- 'benthic'
#tr["Eusergestes arcticus", 1] <- 'pelagic'
tr["Gonostomatidae", 1] <- 'bathypelagic'
# tr["Gymnelis viridis", 1] <- 'benthic'
tr["Heteropolypus", 1] <- 'benthic'
tr["Lophelia pertusa", 1] <- 'benthic'
tr["Macrorhamphosus scolopax", 1] <- 'benthopelagic'
tr["Macrostomias longibarbatus", 1] <- 'bathydemersal | demersal'
tr["Monstrilla", 1] <- 'pelagic'
tr["Myctophidae", 1] <- 'bathypelagic'
tr["Naticidae", 1] <- 'benthic'
tr["Ophiomusa lymani", 1] <- 'benthic'
tr["Polynoidae", 1] <- 'benthic'
tr["Porifera", 1] <- 'benthic'
tr["Radicipes gracilis", 1] <- 'benthic'
tr["Staurostoma mertensii", 1] <- 'benthopelagic'
#tr["Tritia sp.", 1] <- 'benthic'
tr["Velutinidae", 1] <- 'benthic'
tr["Vomer setapinnis", 1] <- 'benthopelagic'
tr["Wimvadocus torelli", 1] <- 'benthic'

# Insert to environment DB
for(i in nm) env[i, ] <- tr[i, ]

#Manual entries Birds and some mammals - Source of information for birds species is https://www.allaboutbirds.org/news/
tr2 = read.csv('./Data/ManualEntries/Environment_ManualEntry.csv', sep=",")

# Environment types
envType <- paste(env[,1], collapse = ' | ') %>%
           stringr::str_split(pattern = ' \\| ') %>%
           unlist() %>%
           unique() %>%
           sort()
environment <- matrix(data = 0, nrow = nSp, ncol = length(envType),
                      dimnames = list(spList$species, envType))

for(i in envType) environment[, i] <- stringr::str_detect(env[,1], i)

env_df <- as.data.frame(environment)
cnames=c(colnames(env_df))
env_df[cnames] <- sapply(env_df[cnames],as.character) #columns as character
# Resolve taxa environments
# "host" are fish parasites, will put them as "demersal" and "pelagic"
env_df2 <-  env_df %>% mutate(benthic = ifelse(grepl("1",host),"1",benthic))
env_df3 <-  env_df2 %>% mutate(pelagic = ifelse(grepl("1",host),"1",pelagic))
# "sessile" taxa are benthic species
env_df4 <-  env_df3 %>% mutate(benthic = ifelse(grepl("1",sessile),"1",benthic))
# 'others' are seabirds, so will categorize them as pelagic
env_df5 <- env_df4 %>% mutate(pelagic = ifelse(grepl("1",others),"1",pelagic))
# 'reef-associated' fish will be categorized as demersal for the St. Lawrence
env_df6 <- env_df5 %>% mutate(demersal = ifelse(grepl("1",`reef-associated`),"1",demersal))
# 'pelagic-neretic' and 'pelagic-oceanic' will be categorized as 'pelagic'
env_df7 <- env_df6 %>% mutate(pelagic = ifelse(grepl("1",`pelagic-neritic`),"1",pelagic))
env_dff <- env_df7 %>% mutate(pelagic = ifelse(grepl("1",`pelagic-oceanic`),"1",pelagic))

#select relevant columns
environment_final <- env_dff %>% select('bathydemersal','bathypelagic','benthic','benthopelagic','demersal','pelagic')
environment_final
#Verify if the dataset is complete
#table(environment_final$'NA')
#row_sub = apply(environment_final, 1, function(row) all(row !=1 ))
#see_missingsp=environment_final[row_sub,]
#write.csv(see_missingsp,file="Environment_ManualEntry.csv")

# environment_final <- as.matrix(environment_final)
environment <- environment_final

# Transform to numeric and matrix
env <- apply(environment, 2, as.numeric) |>
       as.matrix()
rownames(env) <- rownames(environment)
environment <- env

# Export
save(environment, file = './Data/SpeciesTraits/Environment.RData')
write.csv(environment, file = './Data/SpeciesTraits/Environment.csv')
