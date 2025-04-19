# Norris Center Snapshot | Emma Yockman
# 8 Apr 2025

rm(list=ls()) # clear R's brain
lapply(c("tidyverse","ggpubr","ggthemes","patchwork","rmarkdown","scales","tidyverse","GGally"),require,character.only=T) # load in packages in bulk
setwd("~/Documents/Coding/educational/NorrisCenter/Apr8norriscentersnapshot") # set working directory

# DATA EXPLORATION ----
# what is this?
## data downloaded from CCH2, UCSC collection only, on April 8th, 2025.
# measurementOrFact.csv = seems to be phenology traits? we haven't really used this at NC; but seems interesting, check measurementType and measurementValue
# identifiers.csv = types of identifiers that have been associated with each specimen, aside from barcode or Symbiota ID (e.g. old stamping system, other herbarium's stamp, etc)
# identifications.csv = taxon IDs for each specimen, who IDed it, when, notes, references, etc.
# multimedia.csv = has been imaged or not, what kind of image, when
# occurrences.csv = all specimens, all info

# data sets I care about right now (i think they are all related tbh...)
ims=read_csv("multimedia.csv") # only has some... bc not all imaged?
dat=read_csv("occurrences.csv") # warning; I think this is all raw data of every occurence

# shared info across all datasets:
## coreid = symbiota's unique identifier (dat has "id" which i think is coreid)
length(intersect(colnames(ims), colnames(dat))) # see if there are any matches in column names
# nothing shared... (id is coreid?)

head(dat %>% # show me first 10 rows of data if you only look at these columns
       select(id, genus, specificEpithet, taxonRank, 
              infraspecificEpithet, recordID, modified), 10) 

# clean dat ----
## what are the columns ----
head(dat, 10) #normal cch2 table display
a=colnames(dat) # make column names an object so i can explore them easier

### needed columns (after much exploration) ----
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

a[103] # look at 1 of the 103 column names; can enter any number 1 through 103 here
head(unique(dat$recordEnteredBy),10) # loot at that column's info
length(unique(dat$recordEnteredBy)) # check how many there are; types of things in the column
head(unique((dat%>%filter(county=="Santa Cruz"))$recordEnteredBy)) # look at column for just Santa Cruz

## summary ----
length(unique(dat$id)) # 16838 specimens in here? = this is close to CCH2 = 17300; this includes ones missing barcodes
length(unique(dat$recordID)) # 16838 unique records; Al says this is double counting barcodes/unlinked images/etc
length(unique(dat$scientificName)) # 3574 species (different from ids)
length(unique(dat$genus)) # 832 genera (2 more than ids)
length(unique(dat$family)) # 249 families double counting, many typos...
length(unique(dat$catalogNumber)) # 13950 barcodes?
is.na(dat$catalogNumber) # are there any NAs
max(dat$catalogNumber) # largest catalog number
length(unique(dat$collID)) # all 1 = all part of UCSC collection
length(unique(dat$recordEnteredBy)) # 135 users working on this; show how many are students we've engaged! = all has a list
length(unique(dat$disposition)) # 109 projects part of... ? this isn't clean; Al said that these are stuff he's used for batch uploads, when Sys bot classes wanted their data in. A
length(unique(dat$recordedBy)) # collectors; some typos and dupes/misc notes
length(unique(dat$countryCode)) # probably groups country better than typoed slot.
length(unique(dat$country)) # USA US United States United Stated - typos need to be consolidated
unique(dat$typeStatus) # types of types = need to consolidate variations in capitalization/etc
unique(dat$basisOfRecord) # only want PreservedSpecimen

### state issues ----
## if you want CA counties need to filter first
length(unique(dat$stateProvince)) # some typos = california Califoria California; some put a country in here, or locality or just. "State".
unique(dat$stateProvince) # issues = Fort Ord, United States, CA misspellings, "unknown"

# someone put in "State" = for a specimen that is in the US
dat %>% filter(stringr::str_detect(stateProvince,"United States|Fort Ord")) %>% select(id, catalogNumber, stateProvince, locality, county, recordedBy, recordEnteredBy)
# fix UCSC100000240, UCSC100014453, UCSC100014661 (emma fixed in cch2 also); others are fine

dat %>% filter(stringr::str_detect(stateProvince,"^(?i)c")) %>% # check anything that might be california
  select(id, catalogNumber, stateProvince, locality, county, recordedBy, recordEnteredBy) %>% # only look at relevant columns to see if it actually is an issue
  filter(!stringr::str_detect(stateProvince,"California|Connecticut|Colorado|Chihuahua"))# which ones are the issues (ignore correctly spelled CA, CT, CO, Chihuahua)
# fix UCSC100002783, UCSC100014670 (emma fixed in cch2 also)

### country issue ----
head(dat %>% filter(stringr::str_detect(country,"United Arab Emirates"))%>% select(id, catalogNumber, stateProvince, locality, county, recordedBy, recordEnteredBy)) # ummm..... Al said he'll fix these maybe
# fix UCSC100006158, UCSC100010927, UCSC100011068 = all should be United States; Emma fixed in CCH2

### fix ----
# BEFORE YOU LEAVE: double check these guys in here and CCH2
# cch2 syntax: UCSC100000240,UCSC100014453,UCSC100014661,UCSC100002783,UCSC100014670,UCSC100006158,UCSC100010927,UCSC100011068
dat %>% filter(stringr::str_detect(catalogNumber, 
"UCSC100000240|UCSC100014453|UCSC100014661|UCSC100002783|UCSC100014670|UCSC100006158|UCSC100010927|UCSC100011068")) %>%
  select(id, catalogNumber, country, stateProvince, county, locality, recordedBy, recordEnteredBy)

# how to fix? go to Row Column and overwrite.

# catalogNumber == UCSC100002783, stateProvince == California
dat["stateProvince"][dat["catalogNumber"] == "UCSC100002783"] # find the specific value for a given catalogNumber
dat["stateProvince"][dat["catalogNumber"] == "UCSC100002783"] <- "California"
dat["stateProvince"][dat["catalogNumber"] == "UCSC100002783"] # check it

# catalogNumber == UCSC100000240, stateProvince == California
dat["stateProvince"][dat["catalogNumber"] == "UCSC100000240"] # find the specific value for a given catalogNumber
dat["stateProvince"][dat["catalogNumber"] == "UCSC100000240"] <- "California"
dat["stateProvince"][dat["catalogNumber"] == "UCSC100000240"] # check it

# catalogNumber == UCSC100006158, country == United States 
dat["country"][dat["catalogNumber"] == "UCSC100006158"] # find the specific value for a given catalogNumber
dat["country"][dat["catalogNumber"] == "UCSC100006158"] <- "United States"
dat["country"][dat["catalogNumber"] == "UCSC100006158"] # check it

# catalogNumber == UCSC100010927, country == United States 
dat["country"][dat["catalogNumber"] == "UCSC100010927"] # find the specific value for a given catalogNumber
dat["country"][dat["catalogNumber"] == "UCSC100010927"] <- "United States"
dat["country"][dat["catalogNumber"] == "UCSC100010927"] # check it

# catalogNumber == UCSC100011068, country == United States 
dat["country"][dat["catalogNumber"] == "UCSC100011068"] # find the specific value for a given catalogNumber
dat["country"][dat["catalogNumber"] == "UCSC100011068"] <- "United States"
dat["country"][dat["catalogNumber"] == "UCSC100011068"] # check it

# catalogNumber == UCSC100014453, stateProvince == California
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014453"] # find the specific value for a given catalogNumber
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014453"] <- "California"
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014453"] # check it

# catalogNumber == UCSC100014661, stateProvince == California, county == Monterey
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014661"] # find the specific value for a given catalogNumber
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014661"] <- "California"
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014661"] # check it
dat["county"][dat["catalogNumber"] == "UCSC100014661"] # find the specific value for a given catalogNumber
dat["county"][dat["catalogNumber"] == "UCSC100014661"] <- "Monterey"
dat["county"][dat["catalogNumber"] == "UCSC100014661"] # check it

# catalogNumber == UCSC100014670, stateProvince == California
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014670"] # find the specific value for a given catalogNumber
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014670"] <- "California"
dat["stateProvince"][dat["catalogNumber"] == "UCSC100014670"] # check it


dat %>% filter(stringr::str_detect(catalogNumber, 
                                   "UCSC100000240|UCSC100014453|UCSC100014661|UCSC100002783|UCSC100014670|UCSC100006158|UCSC100010927|UCSC100011068")) %>%
  select(id, catalogNumber, country, stateProvince, county, locality, recordedBy, recordEnteredBy)
# IS EVERYTHING FIXED? if no, go back.

## merge dat + ims ----
# question: how much of our collection is imaged, and when did we do it?
# how much of a contribution has the past year been?
head(ims) # imaging data; useful columns = coreID, 
colnames(ims); nrow(ims)
length(unique(ims$coreid)) # coreid is each specimen unique ID in symbiota; 13473 specimens
nrow(ims) # 13924 rows... some doubled coreids?
unique(ims$format) # format is what it is
unique(ims$type) # everything has same type
unique(ims$subtype) # everythign has same subtype
unique(ims$creator) # who put image there
unique(ims$MetadataDate) # date image got uploaded?
unique(ims$metadataLanguage) # everything in english

a=unique(dat$id); a=as.character(a); length(a)
b=unique(ims$coreid); b=as.character(b); length(b)
sum(a %in% b) # 13473 out of 13924 -- NOT all ims ids exist in dat; but maybe should be ok to merge with some gaps;
nrow(ims %>% group_by(coreid) %>% summarize(N=n()) %>% filter(N>2)) # 361 coreids doubled; some tripled?
(length(unique(ims$coreid)) + 361)==nrow(ims) # there are still more rows than doubles..... am i doing my math right?

ims2 = ims %>% select(coreid, subtype, creator, MetadataDate, associatedSpecimenReference); head(ims2)
ims2 = ims2 %>% mutate(ref = sapply(strsplit(associatedSpecimenReference, 
                                      "https://cch2.org/portal/collections/individual/index.php?occid=",TRUE),
                       function(x) (x[2])))
ims2$associatedSpecimenReference[1]
ims2$ref[1]
head(ims2)
nrow(ims2)==sum(ims2$coreid %in% ims2$ref)

# Get ready to HASHMAPPPPP ----
# see > hashmap.R for instructions and brainstorming

#rename dat id to coreid so we can merge dat and ims
dat = dat %>%
  rename("coreid" = "id")
colnames(dat)[1] #check that it worked
dat["coreid"][dat["coreid"] == "229483"]
ims["coreid"][ims["coreid"] == "229483"]

# merge!!! 
cch2=full_join(x=dat,y=ims,by="coreid") # saying, merge dat to id using coreid column
head(cch2)
colnames(dat)
colnames(ims)
colnames(cch2)
nrow(dat)-nrow(ims)

# filtered ----
# barcoded only stuff
nrow(cch2) # 16838 occurences, but not all have barcodes

length(unique(dat$catalogNumber)) #this is OUR barcode = NA does exist; so only 13949 are unique...

dat2= dat %>% # make new df
  filter(!is.na(catalogNumber)) # filter out occurrences with no barcode entered

nrow(dat2) # count; 13958 of records in here have barcodes; 2880 not barcoded
length(unique(dat2$catalogNumber)) # 13948 cat numbers
length(unique(dat2$id)) # 13949 IDS --- cat number with two ids? - multiple duplicates; nrow = 13957

colnames(dat2)
dat2 = dat2 %>% select(id, catalogNumber, recordedBy, eventDate, year, month, day, 
                       family, scientificName, genus, specificEpithet, dateIdentified, identifiedBy, typeStatus, basisOfRecord, 
                       country, stateProvince, county, municipality, locality, decimalLatitude, decimalLongitude,
                       recordEnteredBy, modified); head(dat2, 10)
dat2= dat2 %>% filter(catalogNumber !="TEST")
head(dat2,10)
dat2 %>% filter(catalogNumber == str_detect(dat2$catalogNumber, " "))

dat2$year = replace(dat2$year, dat2$year == 1076, 1976)
str(dat2)
dat2$eventDate = replace(dat2$eventDate, dat2$eventDate == "1076-12-21", "1976-12-21")

## visualizations-----
ggplot(dat2, aes(x=modified))+ # histogram?
  geom_density(col="darkgreen")+
  theme_few()+
  theme(legend.position = "none")+
  labs(x="When IDENTIFICATION last updated (errors)", y="Density of Specimens")+
  scale_y_continuous(labels = comma)

nrow(dat) # 16838 specimens in total collection
f = dat2 %>%  group_by(family) %>%  # group by family; get amount of specimens per fam
        summarize(N=n(), # how many specimens of each genera do we have
                  prop=N/16838) # what proportion of our entire collection is that?
nrow(f) # 183 families barcoded
head(f)
f2 = f %>% filter(N > 100) %>% na.omit()

sum(f$prop) # should be 1? == 83% of collection is barcoded
sum(f2$prop) # barcoded families w > 100 specimens are 62% of collection
length(unique(f2$family)) # theres 24 fams with >100 occ. out of the 183 families
unique(f2$family)

g = dat2 %>%  group_by(genus) %>%  # group by family; get amount of specimens per fam
  summarize(N=n(), # how many specimens of each genera do we have
            prop=N/16838) # what proportion of our entire collection is that?
nrow(g) # 793 genera barcoded
head(g)
g2 = g %>% filter(N > 100) %>% na.omit()

sum(g$prop) # should be 1? == 0.8288989 of collection is barcoded
sum(g2$prop) # barcoded genera w > 100 specimens are 27% of collection
length(unique(g2$genus)) # theres 22 genera >100 occ.
unique(g2$genus)

### palettes 
palette1 = c("Arctostaphylos" = "#5B2333",
             "Lupinus" = "#14248A",
             "Quercus" = "#FBB13C",
             "Trifolium" = "#417B5A",
             "Lasthenia"="#F24333",
             "Bromus"="#BAA5FF")

palette3= c("Asteraceae"="#ea693b",
            "Fabaceae"="#7e9d06",
            "Poaceae"="#8f72ff") # for family breakdown

## plotting ----
### families ----
ggplot(f, aes(x="", y=prop, fill=family)) +# pie chart of families
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ labs(x=NULL,y=NULL, fill=NULL)+ # makes it circular?
  theme_void()+theme(legend.position = "none")+
  #geom_text(aes(label = paste(N, genus)), position = position_stack(vjust=0.5), size=1)+
  scale_fill_manual(values=palette3)

ggplot(f2, aes(x=family, y=N, fill=family))+ # bar chart of families
  geom_col()+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values= palette3)+
  labs(x="", y="") + ylim(0,2500)

### genera ----
ggplot(g, aes(x="", y=prop, fill=genus)) +# pie chart of families
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ labs(x=NULL,y=NULL, fill=NULL)+ # makes it circular?
  theme_void()+theme(legend.position = "none")+
  #geom_text(aes(label = paste(N, genus)), position = position_stack(vjust=0.5), size=1)+
  scale_fill_manual(values=palette1)

ggplot(g2, aes(x=genus, y=N, fill=genus))+ # bar chart of families
  geom_col()+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values= palette1)+
  labs(x="", y="")

#### asters only ----
f2 %>% filter(family=="Asteraceae")
asters = dat %>% filter(family=="Asteraceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/16838, # how much of whole collection is that
            propAsters=N/1693) # how much of just Asteraceae is that genera
nrow(asters) # 136 taxa, 1 NA group

ggplot(asters, aes(x=genus, y=N))+
  geom_col(fill="#ea693b")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=3),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="") + ylim(0,110)

#### legumes only ----
f2 %>% filter(family=="Fabaceae")
legumes = dat %>% filter(family=="Fabaceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/16838, # how much of whole collection is that
            propLegumes=N/2297) # how much of just Asteraceae is that genera
nrow(legumes) # 30 taxa, 1 NA group

ggplot(legumes, aes(x=genus, y=N))+
  geom_col(fill="#7e9d06")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="") + ylim(0,1500)

#### grasses only ----
f2 %>% filter(family=="Poaceae")
grass = dat %>% filter(family=="Poaceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/16838, # how much of whole collection is that
            propGrass=N/1236) # how much of just Poaceae is that genera
nrow(grass) #79 taxa, 1 NA group

ggplot(grass, aes(x=genus, y=N))+
  geom_col(fill="#8f72ff")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=6),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="") + ylim(0,150)

## other SC only ----

ggplot(dat, aes(x=modified))+
  geom_density(col="darkgreen")+
  theme_few()+
  theme(legend.position = "none")+
  labs(x="When OCCURENCE last updated", y="density")

# Look at just SC County Pressed Specimens
santacruz = dat %>% filter(basisOfRecord=="PreservedSpecimen") %>%
  filter(county=="Santa Cruz")
length(unique(santacruz$genus)) # 598 genera from SC County; 7711 specimens
length(unique(santacruz$family)) # 219 families from SC County; 7711 specimens
nrow(santacruz)
colnames(santacruz)

ggplot(santacruz, aes(x=family, fill=family))+
  geom_bar()+
  scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=2),
        axis.ticks.x=element_blank(),
        legend.position = "none")+scale_fill_manual(values=palette3)

### families ----
f3 = santacruz %>% group_by(family) %>%  # group by family; get amount of specimens per fam
  summarize(N=n(), # how many specimens of each family do we have
            prop=N/7711) # how much within our santa cruz collection is that

ggplot(f3, aes(x="", y=prop, fill=family)) +# pie chart of families
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ labs(x=NULL,y=NULL, fill=NULL)+ # makes it circular?
  theme_void()+theme(legend.position = "none")+
  #geom_text(aes(label = paste(N, genus)), position = position_stack(vjust=0.5), size=1)+
  scale_fill_manual(values=palette3)

f4 = f3 %>% filter(N > 100) %>% na.omit()
ggplot(f4, aes(x=family, y=N, fill=family))+ # bar chart of families
  geom_col()+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values= palette3)+
  labs(x="", y="")

### genera ----
g3 = santacruz %>% group_by(genus) %>%  # group by genus; get amount of specimens per fam
  summarize(N=n(), # how many specimens of each genera do we have
            prop=N/7711) # how much within our santa cruz collection is that

ggplot(g3, aes(x="", y=prop, fill=genus)) +# pie chart of families
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ labs(x=NULL,y=NULL, fill=NULL)+ # makes it circular?
  theme_void()+theme(legend.position = "none")+
  #geom_text(aes(label = paste(N, genus)), position = position_stack(vjust=0.5), size=1)+
  scale_fill_manual(values=palette1)

g4 = g3 %>% filter(N > 50) %>% na.omit()
ggplot(g4, aes(x=genus, y=N, fill=genus))+ # bar chart of families
  geom_col()+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  scale_fill_manual(values= palette1)+
  labs(x="", y="")

#### asters only ----
f4 %>% filter(family=="Asteraceae")
asters2 = santacruz %>% filter(family=="Asteraceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/7711, # how much of whole collection is that
            propAsters=N/837) # how much of just Asteraceae is that genera
nrow(asters2) # 90 taxa, 1 NA group

ggplot(asters2, aes(x=genus, y=N))+
  geom_col(fill="#ea693b")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=3),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="")

#### legumes only ----
f4 %>% filter(family=="Fabaceae")
legumes2 = santacruz %>% filter(family=="Fabaceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/7711, # how much of whole collection is that
            propLegumes=N/723) # how much of just Asteraceae is that genera
nrow(legumes2) # 21 taxa, 1 NA group

ggplot(legumes2, aes(x=genus, y=N))+
  geom_col(fill="#7e9d06")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="")

#### grasses only ----
f4 %>% filter(family=="Poaceae")
grass2 = santacruz %>% filter(family=="Poaceae") %>%
  group_by(genus) %>% 
  summarize(N=n(), # how many specimens of each genera do we have
            propWhole=N/7711, # how much of whole santa cruz coll is that
            propGrass=N/624) # how much of just Poaceae is that genera
nrow(grass2) #63 taxa, 1 NA group

ggplot(grass2, aes(x=genus, y=N))+
  geom_col(fill="#8f72ff")+
  #scale_y_continuous(trans='log10')+
  theme_few()+
  theme(axis.text.x=element_text(angle=90, hjust=1, size=6),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  #scale_fill_manual(values= palette2)+
  labs(x="", y="")


## how has collection changed over time ----
# year = year specimen collected
# modified = time we entered it in CCH2 (THIS ISNT TRUE, WRONG INFO)
scTime = santacruz %>% group_by(modified) %>% summarize(N=n()) # modified is actually the wrong metric...

ggplot(scTime, aes(x=modified, y=N))+
  geom_line()+
  theme_few()+
  scale_y_continuous(trans='log10') # all time

str(scTime)
santacruz$moddate = format(santacruz$modified, "%Y-%m-%d")
santacruz$modmonth = format(santacruz$modified, "%Y-%m")
scTime = santacruz %>% group_by(modmonth) %>% summarize(N=n())

ggplot(scTime, aes(x=modmonth, y=N))+
  geom_point()+
  theme_few()+
  scale_y_continuous(trans='log10')+
  theme(axis.text.x=element_text(angle=45, hjust=1, size=9),
        axis.ticks.x=element_blank())

min(santacruz$modified); max(santacruz$modified) # started doing data in 2016?
