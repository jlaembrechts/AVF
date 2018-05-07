# R UTILS file
# BACKUP Helper functie 
# Vergelijkt primaire en secundaire groep op basis van subgroep
# subgroep = variabele "groep" bijv endovasculaire groep
# crëeert rij voor resultatenvel
createRowDemographics <- function(selectie_df, naam){
  
  primair <- filter(selectie_df, grepl("^S|^C", Genesis))
  secundair <- filter(selectie_df, grepl("^OK|^T|^sec|^R", Genesis))
  
  totaal <- nrow(selectie_df)
  primair_aanwezig <- nrow(primair)
  secundair_aanwezig <- nrow(secundair)
  primair_afwezig <- nrow(primair_AVF) - primair_aanwezig
  secundair_afwezig <- nrow(secundair_AVF) - secundair_aanwezig
  
  fisherMatrix <-  matrix(c(primair_aanwezig, secundair_aanwezig, primair_afwezig, secundair_afwezig),
                          nrow = 2,
                          dimnames =
                            list(c("primair", "Secundair"),
                                 c("aanwezig", "afwezig")))
  

  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  
  results_demographics <<- add_row(results_demographics, groep = naam, totaal = round(totaal, 0), totaal_perc = percent(totaal/nrow(AVF)), primair = round(primair_aanwezig,0), primair_perc = percent(primair_aanwezig/(primair_aanwezig+primair_afwezig)), secundair = round(secundair_aanwezig, 0), secundair_perc = percent(secundair_aanwezig/(secundair_aanwezig+secundair_afwezig)), p = round(p, 3))
}
createRowClinicalPresentation <- function(selectie_df, naam){
  
  primair <- filter(selectie_df, grepl("^S|^C", Genesis))
  secundair <- filter(selectie_df, grepl("^OK|^T|^sec|^R", Genesis))
  
  totaal <- nrow(selectie_df)
  primair_aanwezig <- nrow(primair)
  secundair_aanwezig <- nrow(secundair)
  primair_afwezig <- nrow(primair_AVF) - primair_aanwezig
  secundair_afwezig <- nrow(secundair_AVF) - secundair_aanwezig
  
  fisherMatrix <- matrix(c(primair_aanwezig, secundair_aanwezig, primair_afwezig, secundair_afwezig),
                         nrow = 2,
                         dimnames =
                           list(c("primair", "Secundair"),
                                c("aanwezig", "afwezig")))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  
  results_clinicalPresentation <<- add_row(results_clinicalPresentation, groep = naam, totaal = round(totaal,0), totaal_perc = percent(totaal/nrow(AVF)), primair = round(primair_aanwezig,0), primair_perc = percent(primair_aanwezig/(primair_aanwezig+primair_afwezig)), secundair = round(secundair_aanwezig,0), secundair_perc = percent(secundair_aanwezig/(secundair_aanwezig+secundair_afwezig)), p = round(p, 3))
}

check_cathether_involvement <- function(){
filter(secundair_AVF, grepl("CCA", Aff)) %>% nrow(filter(grepl("OKCVC", Genesis)))/nrow(filter(secundair_AVF, grepl("CCA", Aff))) %>% print()
#  secundair_AVF_carotiden <- filter(secundair_AVF, grepl("CCA|ICA|ECA", Aff))
#  secundair_AVF_jugular <- filter(secundair_AVF, grepl("BCV|SV", Eff))
#  secundair_AVF_brachiocephalica <- filter(secundair_AVF, grepl("IJV", Eff))

}

createRowAnatomicalDistribution <- function(selectie_df, naam){
  
  primair <- filter(selectie_df, grepl("^S|^C", Genesis))
  secundair <- filter(selectie_df, grepl("^OK|^T|^sec|^R", Genesis))
  
  totaal <- nrow(selectie_df)
  primair_aanwezig <- nrow(primair)
  secundair_aanwezig <- nrow(secundair)
  primair_afwezig <- nrow(primair_AVF) - primair_aanwezig
  secundair_afwezig <- nrow(secundair_AVF) - secundair_aanwezig
  
  fisherMatrix <-  matrix(c(primair_aanwezig, secundair_aanwezig, primair_afwezig, secundair_afwezig),
                          nrow = 2,
                          dimnames =
                            list(c("primair", "Secundair"),
                                 c("aanwezig", "afwezig")))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  
  results_anatomicalDistribution <<- add_row(results_anatomicalDistribution, groep = naam, totaal = round(totaal,0), totaal_perc = percent(totaal/nrow(AVF)), primair = round(primair_aanwezig,0), primair_perc = percent(primair_aanwezig/(primair_aanwezig+primair_afwezig)), secundair = round(secundair_aanwezig,0), secundair_perc = percent(secundair_aanwezig/(secundair_aanwezig+secundair_afwezig)), p = round(p, 3))
}
createRow <- function(selectie_df, naam){
  
  primair <- filter(selectie_df, grepl("^S|^C", Genesis))
  secundair <- filter(selectie_df, grepl("^OK|^T|^sec|^R", Genesis))
  
  totaal <- nrow(selectie_df)
  primair_aanwezig <- nrow(primair)
  secundair_aanwezig <- nrow(secundair)
  primair_afwezig <- nrow(primair_AVF) - primair_aanwezig
  secundair_afwezig <- nrow(secundair_AVF) - secundair_aanwezig
  
  fisherMatrix <-  matrix(c(primair_aanwezig, secundair_aanwezig, primair_afwezig, secundair_afwezig),
                          nrow = 2,
                          dimnames =
                            list(c("primair", "Secundair"),
                                 c("aanwezig", "afwezig")))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  row_vector <- c(naam, round(totaal, 0), percent(totaal/nrow(AVF)), round(primair_aanwezig,0), percent(primair_aanwezig/(primair_aanwezig+primair_afwezig)), round(secundair_aanwezig, 0), percent(secundair_aanwezig/(secundair_aanwezig+secundair_afwezig)), round(p, 3))
  return(row_vector)  
}

createRowTreatment <- function(modality, naam){
  
  total <- filter(AVF, grepl(modality, Treatment))
  residualsymptoms <- symptomsAfterTreatment %>% filter(grepl(modality, Treatment))
  adverseevents <- adverseEvents %>% filter(grepl(modality, Treatment))
  recurrence <- recurrence %>% filter(grepl(modality, Treatment))
  
  residualsymptoms_perc <- percent(nrow(symptomsAfterTreatment %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment))))
  adverseevents_perc <- percent(nrow(adverseEvents %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment))))
  recurrence_perc <- percent(nrow(recurrence %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment))))

  residualsymptoms_graph <- nrow(symptomsAfterTreatment %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment)))
  adverseevents_graph <- nrow(adverseEvents %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment)))
  recurrence_graph <- nrow(recurrence %>% filter(grepl(modality, Treatment)))/nrow(filter(AVF, grepl(modality, Treatment)))
  
  results_treatment <<- add_row(results_treatment, Modality = naam, total = nrow(total), residualSymptomsn = nrow(residualsymptoms), ResidualSymptoms = residualsymptoms_perc, AdverseEventsn = nrow(adverseevents), AdverseEvents = adverseevents_perc, Recurrencen = nrow(recurrence), Recurrence = recurrence_perc)
  results_treatment_graph <<- add_row(results_treatment_graph, Modality = naam, total = nrow(total), residualSymptomsn = nrow(residualsymptoms), ResidualSymptoms = residualsymptoms_graph, AdverseEventsn = nrow(adverseevents), AdverseEvents = adverseevents_graph, Recurrencen = nrow(recurrence), Recurrence = recurrence_graph)
}

## siginificantie in treatment
checkSignificance <- function(modality1, modality2){
  
  total1 <- filter(AVF, grepl(modality1, Treatment)) %>% nrow()
  residualsymptoms1 <- symptomsAfterTreatment %>% filter(grepl(modality1, Treatment)) %>% nrow()
  adverseevents1 <- adverseEvents %>% filter(grepl(modality1, Treatment)) %>% nrow()
  recurrence1 <- recurrence %>% filter(grepl(modality1, Treatment)) %>% nrow()
  
  total2 <- filter(AVF, grepl(modality2, Treatment)) %>% nrow()
  residualsymptoms2 <- symptomsAfterTreatment %>% filter(grepl(modality2, Treatment)) %>% nrow()
  adverseevents2 <- adverseEvents %>% filter(grepl(modality2, Treatment)) %>% nrow()
  recurrence2 <- recurrence %>% filter(grepl(modality2, Treatment)) %>% nrow()
  
  fisherMatrix <- matrix(c(residualsymptoms1, residualsymptoms2, (total1-residualsymptoms1), (total2-residualsymptoms2)),
                         nrow = 2,
                         dimnames =
                           list(c("Modality1", "Modality2"),
                                c("Aanwezig", "Afwezig")))
  print(paste0(modality1, ' vs ', modality2))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  if(p<0.05){print(paste0('residual symptoms ', p))}
  
  fisherMatrix <- matrix(c(adverseevents1, adverseevents2, (total1-adverseevents1), (total2-adverseevents2)),
                         nrow = 2,
                         dimnames =
                           list(c("Modality1", "Modality2"),
                                c("Aanwezig", "Afwezig")))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  if(p<0.05){print(paste0('Adverse Events ', p))}
  
  fisherMatrix <- matrix(c(recurrence1, recurrence2, (total1-recurrence1), (total2-recurrence2)),
                         nrow = 2,
                         dimnames =
                           list(c("Modality1", "Modality2"),
                                c("Aanwezig", "Afwezig")))
  
  p <- fisher.test(fisherMatrix, alternative = "two.sided", conf.level = 0.95)$p.value
  if(p<0.05){print(paste0('Recurrence ', p))}
}