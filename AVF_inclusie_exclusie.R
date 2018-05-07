## LIBS
library(tidyverse)
library(psych)
library(readxl)
library(readr)

# 
# ANALYSE INCLUSIE 2009-2016
# 

## INLADEN
excel_sheets('AVF_dataset_1.xlsx')
Artikelen <- read_excel('AVF_dataset_1.xlsx', sheet="Volledige search vanaf 2005", col_names = T, col_types = NULL, skip=0)
inclusieFinal <- read_excel('AVF_dataset_1.xlsx', sheet="Inclusie", col_names = T, col_types = NULL, skip=0)
Artikelen <- Artikelen %>% filter(Year > 2008)
inclusieFinal <- inclusieFinal %>% filter(Year > 2008) %>% filter(!duplicated(.["Title"]))

dubbeleArtikelen <- Artikelen %>% filter(duplicated(.["Title"])) # ontdubbelen
exclusieFase1 <- filter(Artikelen, Inclusie != "j")
inclusieFase1 <- filter(Artikelen, Inclusie == "j")
exclusieFase2 <- anti_join(inclusieFase1, inclusieFinal, by = "Author") # hier wsch 98 ipv 97 obv casus snoeren embrechts

## Export results to excel
library(XLConnect)
results_book <- loadWorkbook("AVF_DB.xlsx")
createSheet(results_book, name = "Artikelen2009up")
createSheet(results_book, name = "dubbeleArtikelen_2009")
createSheet(results_book, name = "exclusieFase1_2009")
createSheet(results_book, name = "inclusieFase1_2009")
writeWorksheet(results_book, Artikelen, sheet="Artikelen2009up")
writeWorksheet(results_book, dubbeleArtikelen, sheet="dubbeleArtikelen_2009")
writeWorksheet(results_book, exclusieFase1, sheet="exclusieFase1_2009")
writeWorksheet(results_book, inclusieFase1, sheet="inclusieFase1_2009")
saveWorkbook(results_book, file = "AVF_DB.xlsx")

# 
# ANALYSE INCLUSIE 2005-2008
#

## INLADEN
excel_sheets('AVF_dataset_1.xlsx')
Artikelen <- read_excel('AVF_dataset_1.xlsx', sheet="Volledige search vanaf 2005", col_names = T, col_types = NULL, skip=0)
Artikelen <- read_excel('AVF_dataset_1.xlsx', sheet="eerste exclusie compleet", col_names = T, col_types = NULL, skip=0)
inclusieFinal <- read_excel('AVF_dataset_1.xlsx', sheet="Inclusie", col_names = T, col_types = NULL, skip=0)
exclusieTest <- read_excel('AVF_dataset_1.xlsx', sheet="Exclusie", col_names = T, col_types = NULL, skip=0)
Artikelen <- Artikelen %>% filter(Year > 2004 & Year < 2009)
inclusieFinal <- inclusieFinal %>% filter(Year > 2004 & Year < 2009) %>% filter(!duplicated(.["Title"]))

dubbeleArtikelen <- Artikelen %>% filter(duplicated(.["Title"])) # ontdubbelen
exclusieTest <- exclusieTest %>% filter(Year > 2004 & Year < 2009) %>% filter(!duplicated(.["Title"])) # ontdubbelen
exclusieFase1 <- filter(Artikelen, Inclusie != "j")
inclusieFase1 <- filter(Artikelen, Inclusie == "j")
inclusieFinal2 <- anti_join(inclusieFase1, exclusieTest, by = "Title")
exclusieFase2 <- anti_join(inclusieFase1, inclusieFinal, by = "Title")

test2 <- anti_join(inclusieFase1, inclusieFinal)
test <- anti_join(exclusieTest, Artikelen)

full <- bind_rows(test2, inclusieFinal)
eind <- anti_join(full, exclusieTest, by = 'Title')

test3 <- anti_join(inclusieFinal, inclusieFinal2, by = 'Title')
test4 <- anti_join(inclusieFinal, Artikelen, by = 'Title')

## Export results to excel
library(XLConnect)
results_book <- loadWorkbook("AVF_DB.xlsx")
createSheet(results_book, name = "Artikelen2005up")
createSheet(results_book, name = "dubbeleArtikelen_2005")
createSheet(results_book, name = "exclusieFase1_2005")
createSheet(results_book, name = "inclusieFase1_2005")
createSheet(results_book, name = "exclusieFase2_2005")
writeWorksheet(results_book, Artikelen, sheet="Artikelen2005up")
writeWorksheet(results_book, dubbeleArtikelen, sheet="dubbeleArtikelen_2005")
writeWorksheet(results_book, exclusieFase1, sheet="exclusieFase1_2005")
writeWorksheet(results_book, inclusieFase1, sheet="inclusieFase1_2005")
writeWorksheet(results_book, exclusieFase2, sheet="exclusieFase2_2005")
saveWorkbook(results_book, file = "AVF_DB.xlsx")

## IMPORT XML
# Activate the `XML` library
library(XML)

# Parse the XML file
xmlfile <- xmlTreeParse('AVF in HH gebied.xml')

# Result is usually similar to this: [1] "XMLDocument" "XMLAbstractDocument"
class(xmlfile)

#Tip: you can use the xmlRoot() function to access the top node:
topxml <- xmlRoot(xmlfile)

#You will see that the data is presented kind of weirdly when you try printing out the xmlfile vector. That is because the XML file is still a real XML document in R at this point. To put the data in a data frame, you first need to extract the XML values. You can use the xmlSApply() function to do this:
topxml <- xmlSApply(topxml, function(x) xmlSApply(x, xmlValue))
xml_df <- data.frame(t(topxml), row.names=NULL)