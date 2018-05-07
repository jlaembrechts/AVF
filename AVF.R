## LIBS
library(tidyverse)
library(psych)
library(readxl)
library(knitr)
library(scales)
library(ggplot2)
library(ggsignif)
library(PerformanceAnalytics)
library(stargazer)

## External SCRIPTS
source("Utils.R", local = T)

## INIT
inladen()
data_clean()
subsets_maken()

# ANALYZE
## Demographics
shapiro_value <- shapiro.test(AVF$Age)$p.value # < 0.05 => data not normally distrbuted
wilcox_value <- wilcox.test(primair_AVF$Age, secundair_AVF$Age, correct = F)$p.value ## is de Mann Whithney U test

results_demographics <- tibble(groep = "No of patients", totaal = round(nrow(AVF),0), totaal_perc = NA, primair = round(nrow(primair_AVF),0), primair_perc = percent(primair/totaal), secundair = round(nrow(secundair_AVF),0), secundair_perc = percent(secundair/totaal), p = NA)
results_demographics <- add_row(results_demographics, groep = "Mean age of presentation", totaal = round(describe(AVF$Age)$mean,1), totaal_perc = round(describe(AVF$Age)$sd,1), primair = round(describe(primair_AVF$Age)$mean,1), primair_perc = round(describe(primair_AVF$Age)$sd,1), secundair = round(describe(secundair_AVF$Age)$mean,1), secundair_perc = round(describe(secundair_AVF$Age)$sd,1), p = round(wilcox_value,3))
AVF %>% filter(Gender == "m") %>% createRowDemographics(naam = "Males")
results_demographics <- add_row(results_demographics, groep = "Follow-up", totaal = round(mean(AVF$FollowUp, na.rm = T),1), totaal_perc = NA, primair = round(mean(primair_AVF$FollowUp, na.rm = T),1), primair_perc = NA, secundair = round(mean(secundair_AVF$FollowUp, na.rm = T),1), secundair_perc = NA)

makeVarsSymptoms <- function(groep, naam){
  selectie_df <- filter(AVF, grepl(groep, Symptoms))
  createRow(selectie_df = selectie_df,  naam)
}

## Clinical presentation
results_clinicalPresentation <- tibble(groep = "Time since first symptoms", totaal = round(describe(AVF$TimeSinceFirstSymptoms)$mean,1), totaal_perc = round(describe(AVF$TimeSinceFirstSymptoms)$sd,1), primair = round(describe(primair_AVF$TimeSinceFirstSymptoms)$mean,1), primair_perc = round(describe(primair_AVF$TimeSinceFirstSymptoms)$sd,1), secundair = round(describe(secundair_AVF$TimeSinceFirstSymptoms)$mean,1), secundair_perc = round(describe(secundair_AVF$Age)$sd,1), p = round(wilcox.test(primair_AVF$TimeSinceFirstSymptoms, secundair_AVF$TimeSinceFirstSymptoms, correct = F)$p.value, 3))
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = "Time since trauma", totaal = round(describe(AVF$TimeSinceTrauma)$mean,1), totaal_perc = round(describe(AVF$TimeSinceTrauma)$sd,1), primair = round(describe(primair_AVF$TimeSinceTrauma)$mean,1), primair_perc = round(describe(primair_AVF$TimeSinceTrauma)$sd,1), secundair = round(describe(secundair_AVF$TimeSinceTrauma)$mean,1), secundair_perc = round(describe(secundair_AVF$TimeSinceTrauma)$sd,1), p = round(wilcox.test(primair_AVF$TimeSinceTrauma, secundair_AVF$TimeSinceTrauma, correct = F)$p.value, 3))
row_vector <- createRow(adverseEvents, "Growth")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
## hier nog comorbiditeiten
## primair
paste(unlist(primair_AVF$Comorbidity), collapse =" ") %>%
  strsplit(" ") %>%
  as.data.frame() -> primair_comorbidity
  colnames(primair_comorbidity) <- c("Comorbidity")
  primair_comorbidity$Comorbidity <- gsub(",","", primair_comorbidity$Comorbidity)

primair_comorbidity %>%
  na.omit() %>%
  filter(Comorbidity != "NA") %>%
  count(Comorbidity) %>%
  filter(n>1) %>%
  filter(Comorbidity !=0) %>%
  mutate(percentage = n*100 / nrow(AVF)) -> primair_comorbidity
primair_comorbidity_plot <- ggplot(primair_comorbidity, aes(x = Comorbidity, y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 5)

## secundair
paste(unlist(secundair_AVF$Comorbidity), collapse =" ") %>%
  strsplit(" ") %>%
  as.data.frame() -> secundair_comorbidity
  colnames(secundair_comorbidity) <- c("Comorbidity")
  secundair_comorbidity$Comorbidity <- gsub(",","", secundair_comorbidity$Comorbidity)
  
secundair_comorbidity %>%
  na.omit() %>%
  filter(Comorbidity != "NA") %>%
  count(Comorbidity) %>%
  filter(n>1) %>%
  filter(Comorbidity !=0) %>%
  mutate(percentage = n*100 / nrow(AVF)) -> secundair_comorbidity 
secundair_comorbidity_plot <- ggplot(secundair_comorbidity, aes(x = Comorbidity, y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 5)

## Plot genesis voor secundair
paste(unlist(secundair_AVF$Genesis), collapse =" ") %>%
  strsplit(" ") %>%
  as.data.frame() -> secundair_genesis
  colnames(secundair_genesis) <- c("Genesis")
  secundair_genesis$Genesis <- gsub(",","", secundair_genesis$Genesis)
  
secundair_genesis %>%
  na.omit() %>%
  filter(Genesis != "NA") %>%
  count(Genesis) %>%
  filter(n>1) %>%
  filter(Genesis !=0) %>%
  mutate(percentage = n*100 / nrow(AVF)) -> secundair_genesis
secundair_genesis_plot <- ggplot(secundair_genesis, aes(x = Genesis, y = percentage)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

row_vector <-makeVarsSymptoms('ALC|ATAX|DYSA|INTB|INES|INST|SAH|SEIZ|TIA|VOM', "Intracranial symptoms")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('BLEED|BLEEDT|ECCY|GUMB|HEMO', "Extracranial bloodloss")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('INCO|MYE|NEDE|QUA|RADI|SPCC|ULE|UAI|UAMW', "Neurologic deficit")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('BLV|DECV|DIP|VFD', "Visual impairment")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('CHEM|EPIP|EXOPH|HIP|ORBS|PROP', "Ocular symptoms")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('ANG|CM|HEART|HEARTR|PALP|TACH', "Cardial symptoms")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('TIN|PTIN', "Subjective sound symptoms")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('TR|BR|MUR', "Objective sound")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsSymptoms('HEAD', "Headache")
results_clinicalPresentation <- add_row(results_clinicalPresentation, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])

klachten <- list()
klachten[1] <- c('ALC|ATAX|DYSA|INTB|INES|INST|SAH|SEIZ|TIA|VOM', "Intracranial symptoms")
klachten[2] <- c('BLEED|BLEEDT|ECCY|GUMB|HEMO', "Extracranial bloodloss")
klachten[3] <- c('INCO|MYE|NEDE|QUA|RADI|SPCC|ULE|UAI|UAMW', "Neurologic deficit")
klachten[4] <- c('BLV|DECV|DIP|VFD', "Visual impairment")
klachten[5] <- c('CHEM|EPIP|EXOPH|HIP|ORBS|PROP', "Ocular symptoms")
klachten[6] <- c('ANG|CM|HEART|HEARTR|PALP|TACH', "Cardial symptoms")
klachten[7] <- c('TIN|PTIN', "Subjective sound symptoms")
klachten[8] <- c('TR|BR|MUR', "Objective sound")
klachten[9] <- c('HEAD', "Headache")

klachten[1][2]

### Analyse vaten
#### Efferent
results_efferent <- tibble(groep = character(), totaal = numeric(), totaal_perc = numeric(), primair = numeric(), primair_perc = numeric(), secundair = numeric(), secundair_perc = numeric(), p = numeric())

makeVarsEfferent <- function(groep, naam){
  selectie_df <- filter(AVF, grepl(groep, Eff))
  createRow(selectie_df = selectie_df, naam)
}
row_vector <-makeVarsEfferent('BCV|SV', "Brachiocephalic / Subclavian")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('IJV', "Internal Jugular")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('STHV', "Superior thyroid")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('FV|SOV', "Facial aff")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('VV|SVP|AV|FRV|FCV|PVP|OCV|CEVP|PMP|EV|EVP|IVS', "Vertebral aff")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('EJV|PAV', "External jugular aff")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsEfferent('RMV|STV|SCV', "Retromandibular aff")
results_efferent <- add_row(results_efferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])

#### Afferent
results_afferent <- tibble(groep = character(), totaal = numeric(), totaal_perc = numeric(), primair = numeric(), primair_perc = numeric(), secundair = numeric(), secundair_perc = numeric(), p = numeric())

OKCVC_CCA <- filter(secundair_AVF, grepl("CCA", Aff)) %>% filter(grepl("OKCVC", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("CCA", Aff)))
OKCVC_Carotiden <- filter(secundair_AVF, grepl("CCA|ICA|ECA", Aff)) %>% filter(grepl("OKCVC", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("CCA|ICA|ECA", Aff)))
OKCVC_jugular <- filter(secundair_AVF, grepl("IJV", Eff)) %>% filter(grepl("OKCVC", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("IJV", Eff)))
OKCVC_brachiocephalica <- filter(secundair_AVF, grepl("BCV|SV", Eff)) %>% filter(grepl("OKCVC", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("BCV|SV", Eff)))
OKCVC_brachiocephalica_art <- filter(secundair_AVF, grepl("BCA|SA", Aff)) %>% filter(grepl("OKCVC", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("BCA|SA", Aff)))

OKPLR_CCA <- filter(secundair_AVF, grepl("CCA", Aff)) %>% filter(grepl("OKPLR", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("CCA", Aff)))
OKPLR_Carotiden <- filter(secundair_AVF, grepl("CCA|ICA|ECA", Aff)) %>% filter(grepl("OKPLR", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("CCA|ICA|ECA", Aff)))
OKPLR_jugular <- filter(secundair_AVF, grepl("IJV", Eff)) %>% filter(grepl("OKPLR", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("IJV", Eff)))
OKPLR_brachiocephalica <- filter(secundair_AVF, grepl("BCV|SV", Eff)) %>% filter(grepl("OKPLR", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("BCV|SV", Eff)))
OKPLR_brachiocephalica_art <- filter(secundair_AVF, grepl("BCA|SA", Aff)) %>% filter(grepl("OKPLR", Genesis)) %>% nrow()/nrow(filter(secundair_AVF, grepl("BCA|SA", Aff)))

makeVarsAfferent <- function(groep, naam){
  selectie_df <- filter(AVF, grepl(groep, Aff))
  createRow(selectie_df = selectie_df, naam)
}
row_vector <-makeVarsAfferent('BCA|SA', "Brachiocephalic / Subclavian")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('VA|RA', "Vertebral eff")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('CCA|ICA|ECA', "Carotiden alle drie")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('CCA', "Carotis communis")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('ICA', "Carotis interna")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('ECA', "Carotis externa")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('STHA', "Thyroidea sup")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('FA|ANGA', "Facial")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('LA|SUBLA|SLA', "Lingual and eff")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])
row_vector <-makeVarsAfferent('OPA|STRA|DNA|EA|SOA', "Ophthalmic and eff")
results_afferent <- add_row(results_afferent, groep = row_vector[1], totaal = row_vector[2], totaal_perc = row_vector[3], primair = row_vector[4], primair_perc = row_vector[5], secundair = row_vector[6], secundair_perc = row_vector[7], p = row_vector[8])

## Show % of CCA en IJV en BCA/SA involved in catheter placement
check_cathether_involvement()

## Treatment
# treatment row is modality, symptoms after treatment, adverse events, recurrence
results_treatment <- tibble(Modality = character(), total =  numeric(), residualSymptomsn = numeric(), ResidualSymptoms = numeric(), AdverseEventsn = numeric(), AdverseEvents = numeric(), Recurrencen = numeric(), Recurrence = numeric())
results_treatment_graph <- tibble(Modality = character(), total =  numeric(), residualSymptomsn = numeric(), ResidualSymptoms = numeric(), AdverseEventsn = numeric(), AdverseEvents = numeric(), Recurrencen = numeric(), Recurrence = numeric())

symptomsAfterTreatment <- AVF %>% filter(!is.na(SymptomsAfterTreatment)) %>% filter(SymptomsAfterTreatment != 0)
adverseEvents <- AVF %>% filter(!is.na(AdverseEvents)) %>% filter(AdverseEvents != 0)
adverseEventsDeath <- filter(AVF, grepl("DEATH|death", AdverseEvents))
recurrence <- filter(AVF, Recurrence == 1)

treatment_groups <- c('^SS$|^SC$|^SL$', '^SUR$|^SBR$|^SR$|^SE$', '^E[A-Z1-5]{0,4}$', '^EG$|ENC$|EO$', '^EL$|^EPVA$|^EE$', '^EC$|^EDB$|^EP$|^EAVP\\d?', '^E[A-Z1-5]{0,4}\\s(E[A-Z1-5]{0,4}\\s?){1,4}$', '(^S.*\\sE.*$)|(^E.*\\sS.*$)|SLEM|SEEM', '^D[A-Z]{1,2}')

createRowTreatment('^SS$|^SC$|^SL$', 'Surgical closure')
createRowTreatment('^SUR$|^SBR$|^SR$|^SE$', 'Surgical excision')
createRowTreatment('^E[A-Z1-5]{0,4}$', 'Embolization')
createRowTreatment('^EG$|ENC$|EO$', 'Liquid agent')
# createRowTreatment('^', 'Sclerosing agent')
createRowTreatment('^EL$|^EPVA$|^EE$', 'Particulate agent')
createRowTreatment('^EC$|^EDB$|^EP$|^EAVP\\d?', 'Mechanical agent')
createRowTreatment('^E[A-Z1-5]{0,4}\\s(E[A-Z1-5]{0,4}\\s?){1,4}$', 'Multiple embolisation')
createRowTreatment('(^S.*\\sE.*$)|(^E.*\\sS.*$)|SLEM|SEEM', 'Surgical treatment and embolisation')
createRowTreatment('^D[A-Z]{1,2}', 'Direct puncture')

test <- filter(AVF, grepl('^EL$|^EPVA$|^EE$', Treatment)) #one or multiple

residualsymptoms_graph <- ggplot(results_treatment_graph, aes(x = Modality, y = ResidualSymptoms)) + theme(axis.title.x = element_blank()) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 0.7) + geom_signif(comparisons = list(c("Surgical closure", "Surgical excision"), c("Surgical closure", "Mechanical agent"), c("Liquid agent", "Mechanical agent"), c("Liquid agent", "Embolization")), map_signif_level = T,  annotations="*") + geom_signif(comparisons = list(c("Surgical closure", "Embolization")), map_signif_level = T,  annotations="*", y_position = 0.4) + geom_signif(comparisons = list(c("Surgical excision", "Liquid agent")), map_signif_level = T,  annotations="*", y_position = 0.45) + geom_signif(comparisons = list(c("Surgical excision", "Multiple embolisation"), c("Embolization", "Multiple embolisation")), map_signif_level = T,  annotations="*", y_position = 0.5) + geom_signif(comparisons = list(c("Surgical excision", "Direct puncture")), map_signif_level = T,  annotations="*", y_position = 0.55) + geom_signif(comparisons = list(c("Mechanical agent", "Multiple embolisation")), map_signif_level = T,  annotations="*", y_position = 0.6) + labs(x = "Modality", y = "Residual Symptoms", title="% Residual symptoms per modality")
adverseevents_graph <- ggplot(results_treatment_graph, aes(x = Modality, y = AdverseEvents)) + theme(axis.title.x = element_blank()) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 0.5) + geom_signif(comparisons = list(c("Surgical treatment and embolisation", "Surgical excision")), map_signif_level = T,  annotations="*") + geom_signif(comparisons = list(c("Surgical treatment and embolisation", "Embolization")), map_signif_level = T,  annotations="*", y_position = 0.4) + geom_signif(comparisons = list(c("Surgical treatment and embolisation", "Liquid agent")), map_signif_level = T,  annotations="*", y_position = 0.45) + geom_signif(comparisons = list(c("Surgical treatment and embolisation", "Mechanical agent")), map_signif_level = T,  annotations="*", y_position = 0.5)
recurrence_graph <- ggplot(results_treatment_graph, aes(x = Modality, y = Recurrence)) + theme(axis.title.x = element_blank()) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 0.5) + geom_signif(comparisons = list(c("Surgical excision", "Direct puncture")), map_signif_level = T,  annotations="*", y_position = 0.4)

## Check significance between residual symptoms / recurrence / adverse events
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[1])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[2])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[3])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[4])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[5])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[6])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[7])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[8])
lapply(treatment_groups, checkSignificance, modality1=treatment_groups[9])


test <- filter(AVF, grepl('^E[A-Z1-5]{0,4}$', Treatment))

# save data in variables.RData
save.image("~/Documents/Studie/Onderzoek/AVF/variables.RData")

# createRowTreatment('^E[A-Z1-5]{0,4}$', 'Endovascular single treatment')

# createRowTreatment('^R|\\sR', 'Repair')

# createRowTreatment('^EB|\\sEB', 'Elastic bandage')

# createRowTreatment('^DL|\\sDL', 'DP lipiodol')
# createRowTreatment('^DNC|\\sDNC', 'DP NBC')
# createRowTreatment('^DO|\\sDO', 'DP Onyx')

# createRowTreatment('^EPVA|\\sEPVA', 'Endo Amplatzer vascular plug')
# createRowTreatment('^EC|\\sEC', 'Endovascular coils')
# createRowTreatment('^EDB|\\sEDB', 'Endovascular detachable balloon occlusion')
# createRowTreatment('^EE|\\sEE', 'Endovascular ethiodal tantalum powder')
# createRowTreatment('^EG|\\sEG', 'Endovascular glue')
# createRowTreatment('^EL|\\sEL', 'Endovascular lipiodol')
# createRowTreatment('^ENC|\\sENC', 'Endovascular NBC')
# createRowTreatment('^EO|\\sEO', 'Endovascular Onyx')
# createRowTreatment('^EB|\\sEB', 'Elastic bandage')
# createRowTreatment('^EP|\\sEP', 'Endovascular plug')
# createRowTreatment('^EPVA|\\sEPVA', 'Endovascular PVA')
# createRowTreatment('^SUR|\\sSUR', 'Surgical')
# createRowTreatment('^SBR|\\sSBR', 'Surgical en bloc resection')
# createRowTreatment('^SE|\\sSE', 'Surgical excision')
# createRowTreatment('^SL|\\sSL', 'Surgical ligation')
# createRowTreatment('^SR|\\sSR', 'Surgical resection')
# createRowTreatment('^SS|\\sSS', 'Surgical suture')
# createRowTreatment('^SC|\\sSC', 'Surgical clip')
#createRowTreatment('^SEEM|\\sSEEM', 'Surgical excision after embolisation')
#createRowTreatment('^SLEM|\\sSLEM', 'Surgical ligation after embolisation')
#createRowTreatment('^R|\\sR', 'Repair')

## Export results to excel
# library(XLConnect)
# results_book <- loadWorkbook("AVF_results.xlsx")
# createSheet(results_book, name = "UpdateKet")
# writeWorksheet(results_book, AVF_update, sheet="UpdateKet")
# saveWorkbook(results_book, file = "AVF_results.xlsx")

##============================================================================================================