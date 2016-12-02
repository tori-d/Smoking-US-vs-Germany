library(countrycode)
library(ggplot2)

#### First Dataset ####

# source: http://apps.who.int/gho/data/node.sdg.3-a-viz?lang=en
# https://en.wikipedia.org/wiki/List_of_countries_by_cigarette_consumption_per_capita

## Set working directory
setwd("/Users/toridykes1/Blog/Cigarettes")

## Read CSV

# Europe Data
smokingdf1 <- read.csv("global_situation_data.csv", sep = "\t", 
                       header = T, fileEncoding = "UCS-2LE", stringsAsFactors = F)
# Americas data
smokingdf2 <- read.csv("AMglobal_situation_data.csv", sep = "\t", 
                       header = T, fileEncoding = "UCS-2LE", stringsAsFactors = F)

## Rename columns

names(smokingdf1) <- c("RegionalValue","GlobalValue","Value","Country","IndicatorShortCode","Region")

names(smokingdf2) <- c("RegionalValue","GlobalValue","Value","Country","IndicatorShortCode","Region")

## Subset to just U.S. data
smokingdf2 <- subset(smokingdf2, smokingdf2$Country == "United States of America")

## Merge US with Europe data

smokingdf <- merge(smokingdf1, smokingdf2, 
                   by = c("RegionalValue","GlobalValue","Value","Country","IndicatorShortCode","Region"), 
                   all = T)

## Order by Value and then reindex row numbers

smokingdf3 <- smokingdf[order(smokingdf$Value),]

rownames(smokingdf3) <- 1:nrow(smokingdf3)

## Drop undesired countries

dropnames <- c("Uzbekistan","Azerbaijan","Republic of Moldova","Kazakhstan","Turkey",
               "Kyrgyzstan","Belarus","Armenia","Albania","Georgia","Ukraine","Israel",
               "Bosnia and Herzegovina","Serbia","Slovenia","Luxembourg","Lithuania","Andorra")

smokingdf3 <- smokingdf3[! smokingdf3$Country %in% dropnames, ]

## Create column with 2-letter country names

smokingdf3$CountryShort <- countrycode(smokingdf3$Country, "country.name", "iso2c")

## Build ggplot

area.color1 <- c("one","one","two","one","one","one","one","one","one","one",
                 "one","one","one","one","one","one","one","one","one","one","two",
                 "one","one","one","one","one","one","one") # for coloring of graph

smokingplot <- ggplot(data=smokingdf3, aes(x=reorder(CountryShort, Value),
                                           y=Value, fill = area.color1)) # set axes and coloring

smokingplot <- smokingplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Value), vjust=1.6, color="white", size=3.5) # set bar position & labeling of values

smokingplot <- smokingplot + labs(x="Country", y="Prevalence (%)") +
  ggtitle("Age-standardized prevalence of tobacco smoking among persons 15 years and older (%)") 
  # title axes and graph

smokingplot <- smokingplot + theme(legend.position = "none", 
                                   axis.text.x = element_text(vjust = 10, face = "bold"), 
                                   plot.title = element_text(face = "bold", hjust = .5, size = 15),
                                   axis.title.y = element_text(face = "bold"),
                                   axis.title.x = element_text(face = "bold")) # font adjustments to units

smokingplot <- smokingplot + scale_fill_manual(values=c("grey50","red")) +
  scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) # ensure correct fill color
  # and remove space between unit labels and bars
  

smokingplot

#### Second Dataset ####

# Source here: http://www.tobaccoatlas.org/topic/cigarette-use-globally/
# Under resources

## Load csv
smokingpercap <- read.csv("Consumption-Map.csv", sep = ",", header = T, stringsAsFactors = F)

## Create a vector of all the country names in the first dataset

shortkeep <- smokingdf3$CountryShort

shortkeep

## Create a column with two-letter country names

smokingpercap$CountryShort <- countrycode(smokingpercap$Country, "country.name", "iso2c")

## Keep only the entries that match the countries in the vector

smokingpercap2 <- smokingpercap[smokingpercap$CountryShort %in% shortkeep, ]

## Reorder list by consumption and reindex rows

smokingpercap2 <- smokingpercap2[order(smokingpercap2$TA5.Data),]

rownames(smokingpercap2) <- 1:nrow(smokingpercap2)

## GGplot

area.color2 <- c("one","one","one","one","one","one","one","two",
                 "one","one","one","one","one","one","one","one","two",
                 "one","one","one","one","one","one","one","one","one","one","one") # for coloring of graph

smokingpercapplot <- ggplot(data=smokingpercap2, aes(x=reorder(CountryShort, TA5.Data),
                                                     y=TA5.Data, fill = area.color2)) # set axes and bar fill

smokingpercapplot <- smokingpercapplot + geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=TA5.Data), hjust=1.3, color="white", size=3.5) # set bars and value labels

smokingpercapplot <- smokingpercapplot + labs(x="Country", y="Number of Cigarettes Consumed") + 
  ggtitle("Number of cigarettes consumed per person per year (age 15 and older)") # label axes and graph

smokingpercapplot <- smokingpercapplot + 
  theme(legend.position = "none", axis.text.x = element_text(vjust = 10, face = "bold"), 
        plot.title = element_text(face = "bold", hjust = .5, size = 15),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) # modify unit text

smokingpercapplot <- smokingpercapplot + scale_fill_manual(values=c("grey50","red")) +
  scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) 
  # fill color and adjust spacing

smokingpercapplot <- smokingpercapplot + coord_flip() # rotate graph

smokingpercapplot
