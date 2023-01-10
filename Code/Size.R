# source('./Code/Traits/Size.R')
# Load species
load('./Data/SpeciesList/SpeciesList.RData')
nSp <- nrow(spList)

# =-=-=-=-=-=-=-=-=-=- Size from species -=-=-=-=-=-=-=-=-=-= #
library(rfishbase)
cl <- c("Length","CommonLength","Weight")

# Fishbase
fb <- sb <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
for(i in 1:nSp) {
  # Fishbase
  dat <- species(spList$species[i], server = 'fishbase', fields = cl)
  if (nrow(dat) > 1) {
    fb[i,] <- colMeans(dat, na.rm = T)
  } else {
    fb[i,] <- unlist(as.data.frame(dat))
  }

  # Sealifebase
  dat <- species(spList$species[i], server = 'sealifebase', fields = cl)
  if (nrow(dat) > 1) {
    sb[i,] <- colMeans(dat, na.rm = T)
  } else {
    sb[i,] <- unlist(as.data.frame(dat))
  }
}

# Merge datasets
size <- matrix(nrow = nSp, ncol = length(cl), dimnames = list(spList$species, cl))
for(i in 1:nrow(size)) {
  if (any(!is.na(fb[i, ]))) {
    size[i, ] <- as.matrix(fb[i, ])
  } else if (any(!is.na(sb[i, ]))) {
    size[i, ] <- as.matrix(sb[i, ])
  } else {
    next
  }
}

# =-=-=-=-=-=-=-=-=-=- Size from genus -=-=-=-=-=-=-=-=-=-= #
# Missing taxa
uid <- rowSums(size, na.rm = TRUE) > 0
nm <- rownames(size)[!uid]

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

# Mean size in genus
for(i in 1:length(gn)) {
  fbgn[[i]] <- colMeans(fbgn[[i]], na.rm = T)
  sbgn[[i]] <- colMeans(sbgn[[i]], na.rm = T)
}

# Merge datasets
sizegn <- matrix(data = NA, nrow = length(nm), ncol = length(cl), dimnames = list(nm, cl))
for(i in 1:nrow(sizegn)) {
  if (any(!is.na(fbgn[[i]]))) {
    sizegn[i, ] <- as.matrix(fbgn[[i]])
  } else if (any(!is.na(sbgn[[i]]))) {
    sizegn[i, ] <- as.matrix(sbgn[[i]])
  } else {
    next
  }
}

# =-=-=-=-=-=-=-=-=-=- Size from species + genus -=-=-=-=-=-=-=-=-=-= #
# Only species with new size data
uid <- rowSums(sizegn, na.rm = T) > 0
sizegn <- sizegn[uid, ]

# Add data to size db
for(i in rownames(sizegn)) size[i, ] <- sizegn[i, ]

# Missing taxa
uid <- rowSums(size, na.rm = TRUE) > 0
nm <- rownames(size)[!uid]
nm <- append(nm, "Cardium sp.")

# Manual entries
tr <- matrix(NA, nrow = length(nm), ncol = 1, dimnames = list(nm, 'Length'))

# CaRNS St. Lawrence species check list :

#From image gbif - https://www.gbif.org/tools/zoom/simple.html?src=//api.gbif.org/v1/image/unsafe/https%3A%2F%2Finaturalist-open-data.s3.amazonaws.com%2Fphotos%2F226467101%2Foriginal.jpg
tr['Acanella arbuscula','Length'] <- 20

#https://www.invertebase.org/portal/taxa/index.php?taxon=29773
tr['Acanthogorgia armata','Length'] <- 25

# http://www.marinespecies.org/carms/aphia.php?p=checklist&action=search&gu_id=10178&tRank=220&inc_sub=1&status=pv
# Teleost picture, Actinauge cristata, http://www.marinespecies.org/aphia.php?p=image&pic=44491
tr['Actinauge sp.','Length'] <- 8

#From 0.2-20cm - https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=100653#attributes
tr['Actiniidae','Length'] <- 9.9

# From image: './Data/TaxaImages/Actinostola_callosa.jpg'
tr["Actinostola sp.", 'Length'] <- 10
tr["Actinostola", 'Length'] <- 10

# From images if the genus Agarum: https://onlinelibrary.wiley.com/doi/10.1002/tax.603015
tr["Agarum cribrosum", 'Length'] <- 50

#Alaria esculenta 150 cm https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=145716#attributes
tr['Alaria esculenta','Length'] <- 150

# From image: './Data/TaxaImages/Alcyonidium.jpg'
tr['Alcyonidium sp.','Length'] <- 30

#https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=125333#attributes
tr['Alcyonium digitatum','Length'] <- 20

# Ampelisca eschrichtii: 25mm; https://eol.org/pages/46521913
# Ampelisca macrocephala; 14mm; https://eol.org/pages/46521922
tr['Ampelisca sp.','Length'] <- 1.95
tr['Ampelisca','Length'] <- 1.95

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=125100&pic=49609
tr['Amphiura sp.','Length'] <- 5

# Anonyx sarsi: 30mm; https://eol.org/pages/46525653
tr['Anonyx sp.','Length'] <- 3
tr['Anonyx','Length'] <- 3

# Anthomastus grandiflorus 10cm From image: https://www.marinespecies.org/carms/aphia.php?p=image&tid=125335&pic=41873
tr['Anthomastus grandiflorus','Length'] <- 10

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=128504&pic=41358
tr['Anthoptilum grandiflorum','Length'] <- 46

# From image:https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=157181#images
tr['Aphrodita hastata','Length'] <- 7

# From image: https://eol.org/pages/464119
tr['Aphroditella hastata','Length'] <- 8

#From attributes: c(2.8,2.3,3.3,5,10) https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=158058#attributes
tr['Arbacia punctulata','Length'] <- 4.7

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=106182&pic=39250
tr['Arcoscalpellum michelottianum','Length'] <- 10

#From attributes: c(20,0.25,10,1,2,5,8):https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=129868#attributes
tr['Arenicola marina','Length'] <- 6.6

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=107550&pic=39070
tr['Argis dentata','Length'] <- 9

# From attributes: c(1.5,2): https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=102870#attributes
tr['Arrhis phyllonyx','Length'] <- 1.75

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=160164&pic=29974
tr['Arrhoges occidentalis','Length'] <- 4.5

# Ascidia obliqua: 8; http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=103713#attributes
# Ascidia prunu: 6; http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=103714#attributes
tr['Ascidiacea','Length'] <- 7

# From attributes: https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=145541#attributes
# Max = c(150,150)cm
tr['Ascophyllum nodosum','Length'] <- 150

# From attributes: http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=158351#attributes
# Max = c(100,150) mm
tr['Atlantopandalus propinqvus','Length'] <- 12.5

# From image: http://www.marinespecies.org/aphia.php?p=image&tid=254475&pic=48705
tr['Aulacofusus brevicauda','Length'] <- 4

# From image: https://www.gbif.org/species/2222014
tr['Axius serratus','Length'] <- 1

# Balanus balanus: c(50,20,30); http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=106213#attributes
# Balanus crenatus: c(25,35,15,20); http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=106215#attributes
tr['Balanidae','Length'] <- 2.5

# from image:https://www.mindat.org/paleoimg.php?id=542940
tr["Bathynectes longispina", 'Length'] <- 23

#Individuals can grow to 70 mm - https://eol.org/pages/46508875
tr['Bathynectes maravigna','Length'] <- 7

# Diameter to radius: https://eol.org/pages/49109568
tr['Bolocera','Length'] <- 12.5
tr['Bolocera sp.','Length'] <- 12.5

# Boreomysis arctica: 28mm; http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=119962#attributes
# Boreomysis tridens: c(26,30)mm; http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=119974#attributes
tr['Boreomysis sp.','Length'] <- 2.8

# Size for this taxa is not really relevant because they are colonial species
# The way size is used, however, it should not overly influence their
# vulnerability since size is not considered (currently) to evaluate the
# vulnerability of benthic taxa. We will therefore simply give them
# approximately the median size of taxa in the St. Lawrence
message('WARNING: Size for Bryozoa is the median of taxa size in this list, and thus uninformative. If size is ever considered to evaluate the vulnerability of benthic taxa to stressors, we should reevaluate this decision to properly evaluate the vulnerability of this taxa.')
tr['Bryozoa','Length'] <- 20

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=111230#attributes
tr['Caberea ellisii','Length'] <- 2.5

# From attributes c(48,30):  https://www.fishbase.se/summary/1726
tr['Caelorinchus caelorinchus','Length'] <- 39

# From image: 1.5cm - https://www.gbif.org/occurrence/350581839
tr['Calathura brachiata','Length'] <- 1.5

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=158383&pic=31981
tr['Calocaris templemani','Length'] <- 1.7

# From image, Cardium costatum: https://www.marinespecies.org/carms/aphia.php?p=image&tid=224543&pic=65649
tr['Cardium sp.','Length'] <- 8


# From the genus and from images: https://www.gbif.org/fr/species/8174/treatments
tr["Chrysogorgia agassizii", 'Length'] <- 11

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=139000&pic=39148
#tr['Ciliatocardium ciliatum','Length'] <- 5

# From image: http://www.marinespecies.org/carms/photogallery.php?album=2125&pic=30498
#tr['Colga villosa','Length'] <- 3

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=123915&pic=31932
tr['Ctenodiscus crispatus','Length'] <- 7

# From images:
# ~8cm; http://www.marinespecies.org/carms/aphia.php?p=image&tid=140102&pic=31970
# ~6cm; http://www.marinespecies.org/carms/aphia.php?p=image&tid=140102&pic=29957
#tr['Cyrtodaria siliqua','Length'] <- 7

#From image: https://www.marinespecies.org/carms/aphia.php?p=image&tid=158356&pic=148627
tr['Dichelopandalus leptocerus','Length'] <- 7

# From images:
# http://www.marinespecies.org/carms/aphia.php?p=image&tid=146941&pic=49623
# http://www.marinespecies.org/carms/aphia.php?p=image&tid=146941&pic=39305
# http://www.boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=599947
tr['Drifa glomerata','Length'] <- 8

# From image: http://www.marinespecies.org/aphia.php?p=image&pic=41879
tr['Duva florida','Length'] <- 11

# c(50,47,59) https://eol.org/pages/598165/data
tr['Echinarachnius parma','Length'] <- 5.2

# From images:
# http://www.marinespecies.org/carms/aphia.php?p=image&tid=102146&pic=39306
# http://www.marinespecies.org/carms/aphia.php?p=image&tid=102146&pic=31832
tr['Epimeria loricata','Length'] <- 2

# From image: './Data/TaxaImages/Eualus_fabricii.jpg'
tr['Eualus fabricii','Length'] <- 5

# https://eol.org/pages/318684
tr['Eualus gaimardii','Length'] <- 10

# From images:
# './Data/TaxaImages/Eualus_macilentus.jpg'
# './Data/TaxaImages/Eualus_macilentus2.jpg'
tr['Eualus macilentus','Length'] <- 7

# From image: http://www.marinespecies.org/aphia.php?p=image&tid=107125&pic=31829
#tr['Eusergestes arcticus','Length'] <- 7

# From image: './Data/TaxaImages/Eusirus_cuspidatus.jpg'
tr['Eusirus cuspidatus','Length'] <- 2.5

#From attributes - 210cm https://www.marinespecies.org/aphia.php?p=taxdetails&id=128506#attributes
tr['Funiculina quadrangularis','Length'] <- 210

# https://www.marinespecies.org/aphia.php?p=taxdetails&id=241272#images
tr["Gennadas", 'Length'] <-4

#Gnathophausia zoea from image ~7cm: https://www.marinespecies.org/carms/aphia.php?p=image&tid=119930&pic=110539
tr['Gnathophausia','Length'] <- 7

# Cyclothone microdon: 7.6; https://eol.org/pages/46563201
tr['Gonostomatidae','Length'] <- 7.6

#From attributes:https://www.marinespecies.org/aphia.php?p=taxdetails&id=127096#attributes
tr['Gymnelis viridis','Length'] <- 56

# From image: ~40cm; http://www.marinespecies.org/carms/aphia.php?p=image&tid=128509&pic=41899
tr['Halipteris finmarchica','Length'] <- 40

# From image description: http://www.marinespecies.org/aphia.php?p=image&tid=110364&pic=107452
tr['Hamingia arctica','Length'] <- 5

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=124223&pic=29988
tr['Heliometra glacialis','Length'] <- 6

# c(25,26,16) https://eol.org/pages/46555727/data
tr['Hemithiris psittacea','Length'] <-22.3

#Heteropolypus insolitus - 2cm- From image: https://www.marinespecies.org/aphia.php?p=taxdetails&id=345447#images
tr['Heteropolypus','Length'] <-2

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=100954&pic=44488
tr['Hormathia nodosa','Length'] <- 10

# From attributes c(21.5,12,15)https://www.marinespecies.org/aphia.php?p=taxdetails&id=130464#attributes
tr['Hyalinoecia tubicola','Length'] <- 16.2

# c(105,105,83,50) https://eol.org/pages/46508041/data
tr['Hyas araneus','Length'] <- 8.6

# https://eol.org/pages/46508042/data
tr['Hyas coarctatus','Length'] <- 6.1

# https://eol.org/pages/46521640
tr['Hyperia galba','Length'] <- 1.2

#Ichnopus spinicornis 1.7 cm From attributes: https://www.marinespecies.org/aphia.php?p=taxdetails&id=102589#attributes
tr['Ichnopus','Length'] <- 1.7

#Idotea pelagica 0.4 cm - https://eol.org/pages/46520254
#Idotea linearis  0.4 cm https://eol.org/pages/46520250
tr['Idotea','Length'] <- 0.4

#0.1cm From images: https://www.marinespecies.org/aphia.php?p=taxdetails&id=158285#images
tr['Keratoisis ornata','Length'] <- 0.1

#From attributes 150cm: https://www.marinespecies.org/aphia.php?p=taxdetails&id=145724#attributes
tr['Laminaria digitata','Length'] <- 150

#From the species Laminaria digitata (From attributes): https://www.marinespecies.org/aphia.php?p=taxdetails&id=145724#attributes
tr['Laminaria longicruris','Length'] <- 150

#From attributes - 5cm - https://www.marinespecies.org/aphia.php?p=taxdetails&id=144953#attributes
tr['Leathesia difformis','Length'] <- 5

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=593072&pic=65169
tr['Liponema multicorne','Length'] <- 9

# From attributes: 1.2cm - https://www.marinespecies.org/aphia.php?p=taxdetails&id=135161#attributes
tr['Lophelia pertusa','Length'] <- 1.2

#https://eol.org/pages/46567475
tr['Macrorhamphosus scolopax','Length'] <- 20

# https://eol.org/pages/46563367
tr["Macrostomias longibarbatus", 'Length'] <- 43

# https://eol.org/pages/46530499
tr['Maera loveni','Length'] <- 2.5

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=102837#attributes
tr['Melita dentata','Length'] <- 2.2

# From images - ~0.4 cm https://www.marinespecies.org/aphia.php?p=taxdetails&id=119780#images
tr['Monstrilla','Length'] <- 0.4

# Lampadena speculigera: 15.3; https://eol.org/pages/46564157
# Notoscopelus kroyeri: 14.3; https://eol.org/pages/46564223
# Benthosema glaciale: 10.3; https://eol.org/pages/46564028
# Myctophum punctatum: 11; https://eol.org/pages/46564195
tr['Myctophidae','Length'] <- 12.7

# Cryptonatica affinis: 2.4; https://eol.org/pages/590174
# Lunatia pallida: 4.7; https://eol.org/pages/592329
tr['Naticidae','Length'] <- 3.6

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=102108&pic=30641
tr['Neohela monstrosa','Length'] <- 6

# From image: http://www.marinespecies.org/aphia.php?p=image&tid=178261&pic=78861
tr['Novodinia americana','Length'] <- 15

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=102908&pic=44497
tr['Oediceros saginatus','Length'] <- 2

# From the family Oithonidae 0.2-2mm: https://www.marinespecies.org/aphia.php?p=taxdetails&id=106422#attributes
tr["Oithona spinirostris", 'Length'] <- 0.2

# From images - ~ 2.5 cm https://eol.org/pages/46505067/media
tr['Oncopagurus','Length'] <- 2.5

#From attributes 2-200mm: https://www.marinespecies.org/aphia.php?p=taxdetails&id=124973#attributes
tr['Ophiacantha abyssicola','Length'] <- 10

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=124978&pic=29928
tr['Ophiacantha bidentata','Length'] <- 10

#From images ~4cm: https://www.marinespecies.org/aphia.php?p=taxdetails&id=245559#images
tr['Ophiomusa lymani','Length'] <- 4

# c(15,20,80) https://eol.org/pages/598273/data
tr['Ophiopholis aculeata','Length'] <- 3.8

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=125147&pic=78860
tr['Ophioscolex glacialis','Length'] <- 8

#From images: ~3cm http://v3.boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=89932
tr['Oplophorus spinosus','Length'] <- 3

#From images: ~40cm https://collections.peabody.yale.edu/search/Record/YPM-IZ-004750.CN
tr['Paragorgia arborea','Length'] <- 40

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=102152&pic=30190
tr['Paramphithoe hystrix','Length'] <- 2

# From the family Hyperiidae and from image: https://eol.org/pages/46521658
tr["Parathemisto", 'Length'] <- 0.8

# https://eol.org/pages/46473768/data
tr['Parvicardium pinnulatum','Length'] <- 1.3

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=128515&pic=31988
tr['Pennatula aculeata','Length'] <- 10

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=128516&pic=41360
tr['Pennatula grandis','Length'] <- 20

# From image: https://www.marinespecies.org/aphia.php?p=image&tid=124655&pic=47572
tr['Pentamera calcigera','Length'] <- 10

# From image: Phronima sedentaria 2.5 cm https://collections.peabody.yale.edu/search/Record/YPM-IZ-075000
tr['Phronima','Length'] <- 2.5

# http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=1057563#attributes
tr['Polynoidae','Length'] <- 9

# https://eol.org/pages/46514709
#tr['Pontophilus norvegicus','Length'] <- 7.5

# From image description: http://www.marinespecies.org/aphia.php?p=image&tid=125170&pic=78863
tr['Poraniomorpha sp.','Length'] <- 8

# Size for this taxa is not really relevant because they are colonial species
# The way size is used, however, it should not overly influence their
# vulnerability since size is not considered (currently) to evaluate the
# vulnerability of benthic taxa. We will therefore simply give them
# approximately the median size of taxa in the St. Lawrence
message('WARNING: Size for Porifera is the median of taxa size in this list, and thus uninformative. If size is ever considered to evaluate the vulnerability of benthic taxa to stressors, we should reevaluate this decision to properly evaluate the vulnerability of this taxa.')
tr['Porifera','Length'] <- 20

# From images: https://eol.org/media/13254984
tr['Plutonaster agassizi','Length'] <- 2.5

#Pontaster tenuispinus up to 6.8 cm https://eol.org/pages/45327507
tr['Pontaster','Length'] <- 6.8

# From attributes: https://eol.org/pages/598513
tr['Porania pulvillus','Length'] <- 10

# From image: ~7cm https://www.marinespecies.org/aphia.php?p=image&tid=123321&pic=148081
#https://www.marinespecies.org/aphia.php?p=image&tid=123321&pic=148080
tr['Poraniomorpha','Length'] <- 7

# From attributes: https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=130954#attributes
tr['Potamilla neglecta','Length'] <- 4.5

# From image (in TaxaImages)
tr['Primnoa resedaeformis','Length'] <- 25

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=123908&pic=32027
tr['Psilaster andromeda','Length'] <- 10

# From the genus and from images: https://www.gbif.org/fr/species/8174/treatments
tr["Radicipes gracilis", 'Length'] <- 11

# From identification catalog, but I think it may be much bigger
tr['Reteporella grimaldii','Length'] <- 4

# From image: http://www.marinespecies.org/aphia.php?p=image&tid=102224&pic=49824
tr['Rhachotropis aculeata','Length'] <- 4

# From image: './Data/TaxaImages/Sabinea_sarsii.png'
tr['Sabinea sarsii','Length'] <- 8

# From image: './Data/TaxaImages/Sabinea_septemcarinata.png'
tr['Sabinea septemcarinata','Length'] <- 9

# From images: ~ 50cm- http://v3.boldsystems.org/index.php/Taxbrowser_Taxonpage?taxid=55715
tr['Saccorhiza dermatodea','Length'] <- 50

# From another species in the same genus, Sclerasterias contorta: https://www.marinespecies.org/aphia.php?p=taxdetails&id=178804#images
tr["Sclerasterias tanneri", 'Length'] <- 10

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=107568&pic=31898
tr['Sclerocrangon boreas','Length'] <- 6

# https://eol.org/pages/600560/data
tr['Securiflustra securifrons','Length'] <- 10

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=107136&pic=39122
tr['Sergia robusta','Length'] <- 7

# From attributes: https://eol.org/pages/46552473
tr['Sertularia polyzonias','Length'] <- 5

# From images:
# https://eol.org/pages/46468246
# http://www.marinespecies.org/aphia.php?p=image&tid=254617&pic=39151
#tr['Similipecten greenlandicus','Length'] <- 2

# From 2-200mm https://www.marinespecies.org/aphia.php?p=taxdetails&id=442342#attributes
tr["Spinolambrus pourtalesii", 'Length'] <- 10

# From attributes of the genus (7.4; 4; 6): https://www.marinespecies.org/aphia.php?p=taxdetails&id=106994#attributes
tr["Spirontocaris fabricii", 'Length'] <- 5.8

# c(60,40,74) https://eol.org/pages/343975/data
tr['Spirontocaris liljeborgii','Length'] <- 5.8

# c(60,40,74) https://eol.org/pages/46514360/data
tr['Spirontocaris phippsii','Length'] <- 5.8

# https://eol.org/pages/46514361/data
tr['Spirontocaris spinus','Length'] <- 6

# c(200,300) http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=594013#attributes
tr['Staurostoma mertensii','Length'] <- 250

# From images:
# http://www.marinespecies.org/photogallery.php?album=716&pic=38780
# http://www.marinespecies.org/photogallery.php?album=716&pic=29984
tr['Stegocephalus inflatus','Length'] <- 3.5

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=123808&pic=49748
tr['Stephanasterias albula','Length'] <- 3

# Stephanauge nexilis: http://www.marinespecies.org/carms/aphia.php?p=image&tid=158258&pic=31828
tr['Stephanauge sp.','Length'] <- 5

# https://www.researchgate.net/publication/225495067_Do_bipolar_distributions_exist_in_marine_sponges_Stylocordyla_chupachups_sp_nv_Porifera_Hadromerida_from_the_Weddell_Sea_Antarctic_previously_reported_as_S_borealis_Lovn_1868/figures?lo=1
tr['Stylocordyla borealis','Length'] <- 7

# 3
tr['Syscenus infelix','Length'] <- 3

# https://eol.org/pages/51258428
tr["Tealia felina", 'Length'] <- 10

# c(50,250) https://eol.org/pages/46552783/data
tr['Thuiaria thuja','Length'] <- 15

# From attributes:https://www.marinespecies.org/aphia.php?p=taxdetails&id=422527#attributes
tr['Thyonella pervicax','Length'] <- 8

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=124002&pic=30202
tr['Tremaster mirabilis','Length'] <- 15

# From image: http://www.marinespecies.org/aphia.php?p=image&tid=467490&pic=51169
tr['Tritia sp.','Length'] <- 2

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=123815&pic=30204
tr['Urasterias lincki','Length'] <- 25

# From 2-200mm: https://www.marinespecies.org/aphia.php?p=taxdetails&id=172121#attributes
tr["Vazella pourtalesi", 'Length'] <- 10

# Onchidoris bilamellata: 4; http://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=150457#attributes
tr['Velutinidae','Length'] <- 4

#From image: https://www.marinespecies.org/carms/aphia.php?p=taxdetails&id=546035#images
tr['Venericardia borealis','Length'] <- 1

# https://eol.org/pages/46578013
tr["Vomer setapinnis", 'Length'] <- 25

# From image: http://www.marinespecies.org/carms/aphia.php?p=image&tid=102793&pic=31974
tr['Wimvadocus torelli','Length'] <- 4

# From description: https://marine.ucsc.edu/target/target-species-zostera.html
tr['Zostera marina','Length'] <- 300

# Add to dataset
for(i in rownames(tr)) size[i, "Length"] <- tr[i, "Length"]

# Keep only length
size_df <- as.matrix(size[, 1])
colnames(size_df) <- 'Size'

#Verify if the dataset is complete
which(is.na(size_df), arr.ind=TRUE)

size <- size_df

# Export
save(size, file = './Data/SpeciesTraits/Size.RData')
write.csv(size, file = './Data/SpeciesTraits/Size.csv')
