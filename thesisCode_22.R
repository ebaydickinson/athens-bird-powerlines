#R Script for my 2021 Honors Thesis at UGA, titled:
#"Landscape and Local Determinants of Non-breeding Bird-Use in Powerlines"
#Under mentorship of Dr. Richard Hall

library(tidyverse)
library(magrittr)
library(plyr)
library(dplyr)
library(rlang)
library(ggplot2)
library(lubridate)
library(permute)
library(lattice)

install.packages("remotes")
remotes::install_github("gavinsimpson/ggvegan") 
library(vegan)

install.packages("ggpubr")
library(ggpubr)

library(ggvegan)

#Enter in Data-----

#sheet names
obs<-read_csv("obs_Feb282021.csv")
met<-read_csv("met_Feb282021.csv")

#putting in a file called "BirdRange" to determine seasons that bird species are in Athens 
seas<-read_csv("BirdRange.csv")

#first column in seas is Species, UPPERCASE IMPORTANT!!
seas %<>% dplyr::rename(Species = species)

#rename column checkno to be a visit. Change data type to be a character 
met %<>% mutate(visit=as.character(CheckNo))

#dim(obs)[2] tells me the number of columns in the table--from the 2nd column onwards
#gather together the abundance of the birds
longobs = obs %>% 
  gather(2:dim(obs)[2],key=visit,value=abund)
longobs$abund = as.numeric(longobs$abund)

#associate each visit with the name of the site visited 
metadata = met %>% select(visit,Site)

#attach site to metadata in longobs
longobs %<>% left_join(.,metadata)

#attacb season to longobs
longobs %<>% left_join(.,seas)

#For each site, you will get total number of birds seen in total
TotalBird = longobs %>% 
  drop_na(abund) %>% 
  group_by(Site) %>% 
  dplyr::summarize(totAbund=sum(abund))

#For each site, get the total number of species
TotalSpecies<-longobs %>% 
  drop_na(abund) %>% 
  select(Site,Species) %>% 
  distinct() %>% 
  group_by(Site) %>% 
  dplyr::summarize(totSR = n())

SiteVisit<-longobs %>% 
  drop_na(abund) %>% 
  group_by(Site,visit) %>% 
  dplyr::summarize(visAbund=sum(abund),Species_Richness_Per_Visit=n())

#Creates a file of species richness by Site and Season 
SeasonSpecies<-longobs %>% 
  drop_na(abund) %>% 
  select(Site,Species,season) %>% 
  distinct() %>% 
  group_by(Site,season) %>% 
  dplyr::summarise(SeasSR = n())

#Remove the NAs from the column season
SeasonSpecies %<>% drop_na(.,season)

#Create a file of bird abundance by site and season
# call this file SeasonAbund
SeasonAbund<-longobs %>% 
  drop_na(abund) %>% 
  select(Site,season,abund) %>% 
  distinct() %>% 
  group_by(Site,season) %>% 
  dplyr::summarize(SeasAbund = sum(abund))

#drop NAs
SeasonAbund %<>% drop_na(season)

#averaging the abundance per site visit
timesVisited<-met %>% 
  group_by(Site, Date) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  group_by(Site) %>% 
  dplyr::summarise(nVisits = n())

#Plots (ggplot2)----

#Boxplot of total abundance
ggplot(TotalBird,aes(Site,totAbund))+
  geom_bar(stat="identity")

#grouping powelines by urban vs natural and north vs. south 
SiteType<-tibble(Site=c("chase","riverbend","sandycreek","sbg"),
                  Type=c("urban","urban","natural","natural"),
                  Location=c("north","south","north","south"))

#join site type with totalBird 
TotalBird %<>% left_join(.,SiteType)

#same bar plot as ln. 90-91, but plot the different locations 
ggplot(TotalBird,aes(Site,totAbund))+
  geom_bar(stat="identity",aes(fill=Location))

#how to visualize number of species in each plot
ggplot(SiteVisit, aes(Site,Species_Richness_Per_Visit))+
  geom_boxplot()

#barplot of total species richness
ggplot(TotalSpecies,aes(Site,totSR))+
  geom_bar(stat="identity")

#joining site type with species richness
TotalSpecies %<>% left_join(.,SiteType)

#plotting different categories (natural vs. urban) 
ggplot(TotalSpecies,aes(Site,totSR))+
  geom_bar(stat="identity",aes(fill=Type))+
  labs(x = "Site", y = "Species Richness")+
  scale_fill_discrete(labels = c("managed", "unmanaged"),type = c("#F0E442", "#FF3399"))
ggsave("SR_site.png", width = 5, height = 4)

#stacked bar plot of species season
ggplot(SeasonSpecies, aes(fill = season, x=Site,y=SeasSR)) + 
  geom_bar(position="stack",stat = "identity")

# stacked bar of species season that plots proportion of the groups
ggplot (SeasonSpecies, aes(fill = season, x=Site,y= SeasSR)) + 
  geom_bar(position = "fill", stat = "identity")+
  labs(x = "Site", y = "% Species Richness") +
  scale_fill_discrete(labels = c("Migration", "Summer Resident","Winter Resident", "Year Resident"), type = c("#999999", "#E69F00", "#009E73", "#56B4E9"))
ggsave("SeasonSpecies.png",width = 5, height = 5) 

#stacked bar plot of bird abundance 
#stacked bar plot of species grouped by season
ggplot(SeasonAbund, aes(fill = season, x=Site,y=SeasAbund)) + 
  geom_bar(position="stack",stat = "identity")
ggplot (SeasonAbund, aes(fill = season, x=Site,y=SeasAbund)) + 
  geom_bar(position = "fill", stat = "identity")

#color blind friendly pallet
cbPalette<-c ("#999999", "#E69F00", "#009E73", "#56B4E9","#F0E442", "#0072B2", "#D55E00")

#eltonTraits (database for bird diets)----
#using the elton traits database to include bird diets into the study
eltonAdj<-read_csv("elton_traits_adjusted (1).csv")

#From eltonAdj,take out just the species names and diet and call it EltonDiet 
eltonDiet<-eltonAdj %>% select (Species, Diet) 

# Then from EltonDiet, include only the species that the Athens-Clarke County Species List and EltonTraits have in common
#Then I dropped the NAs at the bottom, I need to make sure that this list is correct. 
#172 observations returned, 215 in the original list, 43 not included
eltonClarke<-semi_join(eltonDiet,longobs, by=c("Species"))%>% drop_na(Species,Diet)
  
#eltonMissing, used season(seas) to extract
#a little easier then extracting the missing values using longobs
#shows the 43 missing obs.  
eltonMissing<-anti_join(seas,eltonDiet, by=c("Species"))

#From eltonMissing,looked up all the species names that are causing discrepancies 
#(ex. Grey ≠ Gray, Grey-cheeked Thrush vs. Gray-cheeked Thrush)
#fixed the discrepancies in excel, calling new csv "elton_traits_adjusted_EBD.csv"
eltonAdj2<-read_csv("elton_traits_adjusted_EBD.csv")

#From eltonAdj2, I wanted to take out just the species names and diet and call it EltonDiet 
eltonDiet<-eltonAdj2 %>% select (Species, Diet)

#read in SpeciesBinary
speciesBinary<-read_csv("speciesBinary.csv")

#join speciesBinary to longobs
longobs %<>% 
  left_join(speciesBinary,longobs, by=c("Species"))

#take longobs and remove the sp. entries using speciesBinary
longobsAdj<-longobs %>% 
  filter(speciesYesNo == 1 )

#Categorize each Diet Type with 4 categories, call it eltonClarkeDiet
eltonClarke %<>%
  mutate_at(vars(Diet), as.factor) %>% 
  mutate(Diet = fct_collapse(Diet, 
                             plant = c("PlantSeed", "FruiNect"),
                             omnivore = "Omnivore",
                             insect = "Invertebrate",
                             meat = "VertFishScav"))

#eltonMissing, I had to use seas, since it was a little easier then extracting the missing values using longobs
#shows the 43 missing items 
anti_join(seas,eltonDiet, by=c("Species"))

#Plotting EltonTraits----

#create data frame that has species name/diet type/abundance
#Create TotalAbund, summarize abundance per site
TotalAbund<-longobsAdj %>% 
  drop_na(abund) %>% 
  select(Site,Species) %>% 
  distinct() %>% 
  group_by(Site) %>% 
  dplyr::summarize(totAbund = n())

TotalAbund2<-longobsAdj %>% 
  drop_na(abund) %>% 
  group_by(Site,Species) 

#Creates a file of species richness by Site and Season 
SeasonSpecies<-longobs %>% drop_na(abund) %>% select(Site,Species,season) %>% distinct() %>% group_by(Site,season) %>% 
  dplyr::summarize(SeasSR = n()) %>% 
  drop_na(season)

#I want to plot a stacked bar graph of diet traits and species richness
#first, I need to add eltonClarke information by joining with longobsAdj
longobsAdj %<>% 
  left_join(eltonClarke,longobsAdj, by = c("Species"))

#second, I need to create a new df called eltonSpecies
eltonSpecies<-longobsAdj %>% 
  drop_na(abund) %>% 
  select(Site,Species,Diet) %>% 
  distinct() %>% 
  group_by(Site,Diet) %>% 
  dplyr::summarise(SeasSR = n()) %>% 
  drop_na(Diet)

#third, I need to plot these into a stacked barplot
# stacked bar of species season that plots proportion of the groups
ggplot (eltonSpecies, aes(fill = Diet, x=Site,y= SeasSR)) + 
  geom_bar(position = "fill", stat = "identity")+
  labs(x = "Site", y = "% Species Richness")
ggsave("eltonSpecies.png",width = 5, height = 5) 

#landcoverdata----

#surrounding GIS landcover from USGS National Landcover Database
#read in as % percent landcover
#join percent landcover to site type in longobs
percentCover<-read_csv("percent_cover.csv")
longobsAdj<-left_join(percentCover, longobsAdj, by = "Site")

# I rearranged the table so that is easier to but in a stacked barplot
percentCover<-pivot_longer(percentCover, cols = 2:5, names_to = "coverType", values_to = "perCover")

#stacked barplot for cover type
ggplot (percentCover, aes(fill = coverType, x=Site,y= perCover))+ 
  geom_bar(position = "fill", stat = "identity")+
  labs(x = "Site", y = "% landcover")
ggsave("percentCover.png",width = 5, height = 5) 

#row width and height----

#read in row_height_width, call rowDimension
rowDimension<-read_csv("row_width_length.csv")

#add to longobsAdj
longobsAdj<-dplyr::left_join(rowDimension,longobsAdj, by = c("Site"))

#Dates----

#group by year
#take met, and pull out the dates, CheckNO, and site names, and new winter binary
# 1 = (2019-2020 (up until summer 2020)), 2 = (2020-2021)
metaDate<-met %>% select(visit,Site,Date,winter)

#group by "winter" 
#associate the dates with longobsAdj
longobsAdj %<>% left_join(.,metaDate)
#create winterAbund by grouping by site and by winter
winterAbund<-longobsAdj %>% 
  drop_na(abund) %>% 
  group_by(Site, winter) %>% 
  dplyr::summarise(totAbund = sum(abund)) %>% 
  mutate_at(vars(Site, winter), as.factor)

#average out the unequal visits that I did for each site
#I want to know how many sites I visited per year:
yearVisit<-metaDate %>% 
  group_by(Site, Date, winter) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  group_by(Site, winter) %>% 
  dplyr::summarise(visits = n()) %>% 
  mutate(winter = as.factor(winter))

#join yearVisit to winterAbund
#want to associate abundances with visits per year
#call it winterVisit
#create a new column called aveAbund (averageAbund)
winterVisit<-left_join(winterAbund, yearVisit, by = c("winter","Site"), copy = FALSE) %>% 
  mutate(aveAbund = totAbund / visits)

#do the same thing with species richness, call it aveSR
#divide totAbund by winterVisit to get a new column
#plot by abundance
ggplot(winterVisit,aes(Site, aveAbund))+
  geom_bar(stat="identity", aes(fill= winter), 
           position = "dodge") +
  labs(x = "Site", y = "Average Abundance") + 
  scale_fill_discrete(type = c("#ef8a62", "#67a9cf"),
                      labels = c("2019-2020", "2020-2021")) +
  theme_bw()
ggsave("aveAbund.png", width = 5, height = 5)

#create winterSR by grouping by site, winter, and by Species
winterSR = longobsAdj %>% 
  drop_na(abund) %>% 
  dplyr::select(Site, winter, Species) %>% 
  distinct() %>% 
  group_by(Site, winter) %>% 
  dplyr::summarise(totSR = n()) %>% 
  mutate_at(vars(Site, winter), as.factor)

#plot by species richness
ggplot(winterSR,aes(Site,totSR))+
  geom_bar(stat="identity", aes(fill= winter), 
           position = "dodge") +
  scale_fill_discrete(type = c("#ef8a62", "#67a9cf"),
                      labels = c("2019-2020", "2020-2021")) +
  theme_bw() + 
  labs(x = "Site", y = "Species Richness" )
ggsave("winterSR.png", width = 5, height = 5)

#I want to plot the species richness and abundance by date 
#abundance data by date, winter, site, species
library(lubridate)
dateAbund = longobsAdj %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(month = lubridate::month(Date)) %>% 
  filter(month %in% c("10", "11", "12", "1", "2")) %>% 
  mutate(year = lubridate::year(Date)) %>% 
  drop_na(abund) %>% 
  select(Site, winter,abund, year, month) %>% 
  distinct() %>% 
  group_by(Site, winter, month) %>% 
  drop_na(abund) %>% 
  dplyr::summarize(abundance = sum(abund)) %>% 
  mutate_at(vars(month), as.factor)

#plot bird abundance by date
ggplot(data = dateAbund, aes(x = month, y = abundance, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("Oct", "Nov", "Dec", "Jan", "Feb"))

#species richness by date 
dateSR = longobsAdj %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(month = lubridate::month(Date)) %>% 
  filter(month %in% c("10", "11", "12", "1", "2")) %>% 
  mutate(year = lubridate::year(Date)) %>% 
  drop_na(abund) %>% 
  select(Site,Species, winter,year, month) %>% 
  distinct() %>% 
  group_by(Site, winter, month) %>% 
  dplyr::summarize(SR = n()) %>% 
  mutate_at(vars(month), as.factor)

#graph SR by month and date 
ggplot(data = dateSR, aes(x = month, y = SR, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c("Oct", "Nov", "Dec", "Jan", "Feb"))

#linear models----

#Before I make a linear model
#I need to organize all my data in a that I have columns 
#columns for site, date, total richness, total abundance, and any site variables
#In a new data frame, I need to link longobsAdj to a new data frame 
#I can get both SiteSR and SiteAbund for each 

#pull out siteSR 
linearSR = longobsAdj %>% 
  drop_na(abund) %>% 
  select(visit,Species) %>% 
  distinct() %>% 
  group_by(visit) %>% 
  dplyr::summarise(siteSR = n())

#pull out siteAbund
linearAbund = longobsAdj %>% 
  drop_na(abund) %>% 
  select(visit, abund) %>% 
  distinct() %>% 
  group_by(visit) %>% 
  dplyr::summarize(siteAbund = sum(abund))

linear = left_join(linearSR,linearAbund, by=c("visit"))
longobsAdj %<>% left_join(.,linear)

# make boxplots of site abundance and richness
#Contributor: Dr. JP Schmidt 
#install.packages("ggpubr")
#library(ggpubr)
#library(ggplot2)
SiteVisit$log10abund<-log10(SiteVisit$visAbund)
my_comparisons <- list( c("chase", "riverbend"), c("chase", "sandycreek"), c("chase", "sbg"),c("riverbend", "sandycreek"), c("riverbend", "sbg") , c("sandycreek", "sbg"))
ggboxplot(SiteVisit, x = "Site", y = "log10abund",
          color = "Site", palette = "jco",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 2.1)     # Add global p-value

SiteVisit$log10rich<-log10(SiteVisit$Species_Richness_Per_Visit)
ggboxplot(SiteVisit, x = "Site", y = "log10rich",
          color = "Site", palette = "jco",
          add = "jitter")+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1.5)     # Add global p-value

# linear regression template code
#lm1<-lm(richness~forest+ag+urban+date,data=mergeddata)
#summary(lm1)

#linear regression for species richness versus percentCover, width, winter
lmSRvROW<-lm(siteSR~Forested+Agricultural+Developed+OpenWater,data = longobsAdj)
summary(lmSRvROW)

#linear regression for species abundance versus percentCover 
lmAbundvROW<-lm(siteAbund~Forested+Agricultural+Developed+width,data = longobsAdj)
summary(lmAbundvROW)

#'scatter plot for everything
#ggscatter(mergedata, x = "forest", y = "richness", add = "reg.line") +
#stat_cor(label.x = 3, label.y = 34) +
#Wstat_regline_equation(label.x = 3, label.y = 32)

#scatterplot for Forested vs. siteSR
ggscatter(longobsAdj, x = "Forested", y = "siteSR", add = "reg.line") +
  stat_cor(label.x = 1, label.y = 34) +
  stat_regline_equation(label.x = 3, label.y = 32)

#scatterplot for Developed versus siteSR
ggscatter(longobsAdj, x = "Developed", y = "siteSR", add = "reg.line") +
  stat_cor(label.x = 3, label.y = 34) +
  stat_regline_equation(label.x = 3, label.y = 32)+
  ggpar(xlim = c(0,1))

ggpar(ggscatter(longobsAdj, x = "Developed", y = "siteSR", add = "reg.line") +
        stat_cor(label.x = 0.5, label.y = 34) +
        stat_regline_equation(label.x = 0.5, label.y = 30), xlim = c(0,1))
ggsave("scatterplotDeveloped.png", width = 5, height = 5 )


#scatterplot for width versus siteSR
ggscatter(longobsAdj, x = "width", y = "siteSR", add = "reg.line") +
  stat_cor(label.x = 3, label.y = 34) +
  stat_regline_equation(label.x = 3, label.y = 32)

#linear models for species richness with diet
#use eltonSpecies in lm
lmSRDiet<-lm(SeasSR~ Diet+Site,data = eltonSpecies)
summary(lmSRDiet)

#linear models for season abund
#linear models for season species richness
lmSeasAbund<-lm(SeasAbund~season+Site,data = SeasonAbund)
summary(lmSeasAbund)

#pca models for each season (year, Winter, Migratory, summer)----

#Birds classified as yearly
seasonYear = filter(longobsAdj, "season" == "Y")
#data frame = pcaYear  
pcaYear = pivot_wider(seasonYear,
                      names_from = "Species", 
                      values_from = "abund")

#Birds classified as Winter
seasonWinter = filter(longobsAdj, season == "W")
#data frame = pcaWinter  
pcaWinter = pivot_wider(seasonWinter,
                        names_from = "Species", 
                        values_from = "abund")

#Birds classified as Migratory
seasonMigrant = filter(longobsAdj, season == "M")
#data frame = pcaMigrant 
pcaMigrant = pivot_wider(seasonMigrant,
                         names_from = "Species", 
                         values_from = "abund")

#Birds classified as Summer
seasonSummer = filter(longobsAdj, season == "S")
#data frame = pcaSummer 
pcaSummer = pivot_wider(seasonSummer,
                        names_from = "Species", 
                        values_from = "abund")

#library(vegan)
## S3 method for class 'cca'
#library(ggplot2)
#library(vegan)

install.packages("remotes")
remotes::install_github("gavinsimpson/ggvegan") 
library(vegan)

#cca plot for summer birds
summerbirds<-pcaSummer[,16:57]
summerbirds[is.na(summerbirds)] <- 0
summerbirds<-cbind(pcaSummer[,1:7],summerbirds)
summerbirds<-summerbirds[as.logical(rowSums(summerbirds[,9:43] != 0)), ]


cca.summer <- cca(summerbirds[,9:43]~width+Developed+Forested+Agricultural,data=summerbirds)
plot(cca.summer,display=c("bp","sp"))


#figure margins are too large
plot(cca.summer,display=c("bp","sp"))


#autoplot
autoplot(
  cca.summer,
  axes = c(1, 2,3),
  geom = c("point", "text"),
  layers = c("species", "biplot"),
  legend.position = "none",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylab,
  xlab
)

