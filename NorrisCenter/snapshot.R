# Norris Center Snapshot | Emma Yockman
# 8 Apr 2025
rm(list=ls())
lapply(c("tidyverse","ggpubr","ggthemes","patchwork","rmarkdown","scales","tidyverse","GGally"),require,character.only=T)
setwd("~/Documents/Coding/educational/NorrisCenter/Apr8norriscentersnapshot")

# DATA EXPLORATION ----
# what is this?
## data downloaded from CCH2, UCSC collection only, on April 8th, 2025.
# measurementOrFact.csv = seems to be phenology traits? we haven't really used this at NC; but seems interesting, check measurementType and measurementValue
# identifiers.csv = types of identifiers that have been associated with each specimen, aside from barcode or Symbiota ID (e.g. old stamping system, other herbarium's stamp, etc)
# identifications.csv = taxon IDs for each specimen, who IDed it, when, notes, references, etc.
# multimedia.csv = has been imaged or not, what kind of image, when
# occurrences.csv = all specimens, all info

# data sets I care about right now (i think they are all related tbh...)
ids=read_csv("identifications.csv") # maybe don't use this, i think some specimens are double counted bc ID updates and such?
ims=read_csv("multimedia.csv") # only has some... bc not all imaged?
dat=read_csv("occurrences.csv") # warning; I think this is all raw data of every occurence

# shared info across all datasets:
## coreid = symbiota's unique identifier (dat has "id" which i think is coreid)
length(intersect(colnames(ids), colnames(ims))) # only share coreid
length(intersect(colnames(ims), colnames(dat))) # nothing shared... (id is coreid?)
length(intersect(colnames(ids), colnames(dat))) # 13 column names in common... which ones?
intersect(colnames(ids), colnames(dat))

head(dat %>% select(id, genus, specificEpithet, taxonRank, infraspecificEpithet, recordID, modified))
head(ids %>% select(coreid, genus, specificEpithet, taxonRank, infraspecificEpithet, recordID, modified))
# recordID and modified are NOT the same, just have same column name in dat and ids
# id and coreid are the same i think


# TAXON IDENTIFICATIONS ====
head(ids)
# this is just species identification, changes to taxa, or id updates and sources
colnames(ids)
length(unique(ids$coreid)) # 15606 specimens in here?
length(unique(ids$recordID)) # 16193 unique records.... so some have same coreID....? Al says this is double counting barcodes/unlinked images/etc
length(unique(ids$scientificName)) # 3668 species
length(unique(ids$genus)) # 830 genera

ggplot(ids, aes(x=modified))+
  geom_density(col="darkgreen")+
  theme_few()+
  theme(legend.position = "none")+
  labs(x="When IDENTIFICATION last updated", y="Specimens")+
  scale_y_continuous(labels = comma)
  # all the IDs happened in 2023 it seems

nrow(ids) # 16193 specimens

g = ids %>% 
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            prop=N/16193) # what proportion of our entire collection is that?

nrow(g) # 830 genera
head(g)
g2 = g %>% filter(N > 100) %>% na.omit()

palette1 = c("Arctostaphylos" = "#621B00",
             "Lupinus" = "#14248A",
             "Quercus" = "#99C1B9",
             "Trifolium" = "#D4C2FC")

ggplot(g, aes(x="", y=prop, fill=genus)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ labs(x=NULL,y=NULL, fill=NULL)+
  theme_void()+theme(legend.position = "none")+
  #geom_text(aes(label = paste(N, genus)), position = position_stack(vjust=0.5), size=1)+
  scale_fill_manual(values=palette1)
# ones I care about in this data set - genus, modified, identifiedBy, dateIdentified?
sum(g$prop) # should be 1?
sum(g2$prop) # genera w > 100 specimens are 26% of collection
length(unique(g2$genus)) # theres 22 of them
unique(g2$genus)

palette2 = c("Acmispon" = "grey90",
             "Agrostis" = "grey80",
             "Arctostaphylos" = "#621B00",
             "Bromus" = "grey90",
             "Carex" = "grey80",
             "Castilleja" = "grey90",
             "Ceanothus" = "grey80",
             "Chorizanthe" = "grey90",
             "Eriogonum" = "grey80",
             "Gilia" = "grey90",
             "Juncus" = "grey80",
             "Lupinus" = "#14248A",
             "Mimulus" = "grey90",
             "Nemophila" = "grey80",
             "Oxalis" = "grey90",
             "Phacelia" = "grey80",
             "Piperia" = "grey90",
             "Plagiobothrys" = "grey80",
             "Plantago" = "grey90",
             "Quercus" = "#99C1B9",
             "Stachys" = "grey90",
             "Trifolium" = "#D4C2FC") # palette for genera with 100+ specimens

ggplot(g2, aes(x=genus, y=N, fill=genus))+
  geom_col()+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values= palette2)+
  labs(x="", y="")+ ylim(0,1000)

# IMAGING ----
# question: how much of our collection is imaged, and when did we do it?
# how much of a contribution has the past year been?
head(ims) # imaging data; useful columns = coreID, 
colnames(ims)
length(unique(ims$coreid)) # coreid is each specimen unique ID in symbiota; 13473 specimens
unique(ims$format) # format is what it is
unique(ims$type) # everything has same type
unique(ims$subtype) # everythign has same subtype
unique(ims$creator) # who put image there
unique(ims$MetadataDate) # date image got uploaded?
unique(ims$metadataLanguage) # everything in english

# THE COLLECTION ====
head(dat) #normal cch2 table display
a=colnames(dat)
# needed columns (after much exploration)
# id (short symbiota id) or occurrence ID (huge unique id) = for everything in cch2
# basisOfRecord = is it pressed or not
# catalogNumber = 13950 have been barcoded, this isn't all specimens in cch2
# otherCatalogNumbers = past stamping systems and stuff
# phylum = mostly Tracheophytes but some not (NA)
# class = within phylum
# order (50), family (249 with typos)
# scientificName = 3574, i'm sure there are typos
# taxonID = 3426, code for sci names? might account for typos in sci names?
# genus = 832
# specificEpithet = not measure of species - just second latin name
# verbatimTaxonRank = var. subsp. etc - used for typing and stuff/labels
# infraspecificEpithet = after var or subsp
# taxonRank = how good the ID is
# typeStatus = special ones
# recordedBy = collector (1320, likely typos and double counts)
# recordNumber = collector number (eg. RM 1101)
# eventDate = when collected (4179 days)
# year (141), month, day
# associatedOccurrences we don't use but might be useful field for cross referencing
# associatedTaxa don't need but interesting to me as ecologist
# establishmentMeans = tracking native status?
# lifeStage = we don't use but should
# continent = 4, + NA
# countryCode fixed country typo problem ;country = typos; United Stated, USA, "United Arab Emirates" - which should prob be US
# stateProvince = need to fix california typos
# county = SO MNAY ISSUES unique((dat%>%filter(stateProvince=="California"))$county)
# municipality = only keep for w/in SC? (there are 7k+ specimens from Santa Cruz); length(unique((dat%>%filter(county=="Santa Cruz"))$id))
# locality = so many unique ones for SC
# decimalLatitude = most of the ones in SC have these yay
# decimalLatitude = most of the ones in SC have these yay
# geodeticDatum = needed for lat long
# coordinateUncertaintyInMeters
# georeferenceSources
# minimumElevationInMeters
# maximumElevationInMeters
# disposition = only SC county pls
# recordEnteredBy = who put this in CCH2
# modified = NOT when it got entered; some of these weirdly all have same number

a[103]
head(unique(dat$recordEnteredBy),10)
length(unique(dat$recordEnteredBy))
head(unique((dat%>%filter(county=="Santa Cruz"))$recordEnteredBy))


dat %>% mutate(barc=ifelse(is.na(catalogNumber), "nobarc","barc")) %>%
  group_by(barc) %>% summarize(N=n()) # 13958 of records in here have barcodes, only 13950 are unique...; 2880 not barcoded, what's with those ones?

dat %>% mutate(barc=ifelse(is.na(catalogNumber), "nobarc","barc")) %>%
  filter(barc=="nobarc") %>% group_by(disposition) %>% summarize(N=n())






length(unique(dat$id)) # 16838 specimens in here = this is close to CCH2 = 17300
length(unique(dat$recordID)) # 16838 unique records
length(unique(dat$collID)) # all part of UCSC collection
length(unique(dat$recordEnteredBy)) # 135 users working on this; show how many are students we've engaged! = all has a list
length(unique(dat$disposition)) # 109 projects part of... ? this isn't clean; Al said that these are stuff he's used for batch uploads, when Sys bot classes wanted their data in. A
length(unique(dat$recordedBy)) # collectors; some typos and dupes/misc notes
length(unique(dat$countryCode)) # probably groups country better than typoed slot.
length(unique(dat$country)) # USA US United States United Stated - typos need to be consolidated
unique(dat$typeStatus) # types of types = need to consolidate

length(dat %>% filter(country=="United Arab Emirates")) # ummm..... Al said he'lll fix these maybe

# FIX CALIFORNIA records; if you want CA counties need to filter first
length(unique(dat$stateProvince)) # some typos = california Califoria California; some put a country in here, or locality or just. "State".

length(unique(dat$family)) #249 families, many typos...
length(unique(dat$scientificName)) # 3574 species (different from ids)
length(unique(dat$genus)) # 832 genera (2 more than ids)


unique(dat$basisOfRecord) # only want PreservedSpecimen
unique(dat$catalogNumber) #this is OUR barcode = NA does exist


ggplot(dat, aes(x=modified))+
  geom_density(col="darkgreen")+
  theme_few()+
  theme(legend.position = "none")+
  labs(x="When OCCURENCE last updated", y="density")




santacruz = dat %>% filter(basisOfRecord=="PreservedSpecimen") %>%
  filter(county=="Santa Cruz") %>%
  group_by(genus)
length(unique(santacruz$family)) # 598 genera from SC County; 7711 specimens
colnames(santacruz)

ggplot(santacruz, aes(x=family))+
  geom_bar()+
  scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=2),
        axis.ticks.x=element_blank(),
        legend.position = "none")

# how has collection changed over time
# year = year specimen collected
# modified = time we entered it in CCH2
scTime = santacruz %>% group_by(modified) %>% summarize(N=n()) # modified is actually the wrong metric...

ggplot(scTime, aes(x=modified, y=N))+
  geom_line()+
  theme_few()+
  scale_y_continuous(trans='log10') # all time

str(scTime)
santacruz$date = format(santacruz$modified, "%Y-%m-%d")
santacruz$month = format(santacruz$modified, "%Y-%m")
scTime = santacruz %>% group_by(month) %>% summarize(N=n())

ggplot(scTime, aes(x=month, y=N))+
  geom_point()+geom_line()+
  theme_few()+
  scale_y_continuous(trans='log10')+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=9),
        axis.ticks.x=element_blank())

min(santacruz$modified); max(santacruz$modified) # started doing data in 2016

# recordedBy = Collector
# typeStatus = is this a type specimen?