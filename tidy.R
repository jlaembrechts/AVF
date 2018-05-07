## INLADEN

inladen <- function(){
  # setwd("/Users/jur/Documents/Studie/Onderzoek/AVF")
  excel_sheets('AVF_dataset_1.xlsx')
  AVF <- read_excel('AVF_dataset_1.xlsx', sheet="Inclusie", col_names = T, col_types = NULL, skip=0)
  AVF <<- as.tibble(AVF)
  
  ## update met nieuwe Ket
  AVF_update <- read_excel('AVF_dataset_1.xlsx', sheet="UpdateKet", col_names = T, col_types = NULL, skip=0)
  AVF_update <- as.tibble(AVF_update)
  AVF_vergelijk <- read_excel('AVF_dataset_1.xlsx', sheet="vergelijk", col_names = T, col_types = NULL, skip=0)
  AVF_vergelijk <- as.tibble(AVF_vergelijk)
  AVF_update <- distinct(AVF_update) # verwijder duplicates
  AVF_update <<- anti_join(AVF_update, AVF_vergelijk) # welke zijn al bekeken
}

data_clean <- function(){
  ## DATA CLEANING
  to_numeric <- c("Year", "Age", "TimeSinceTrauma", "TimeSinceFirstSymptoms", "NumberOfFistulas", "FollowUp")
  to_character <- c("CountryHospital", "SyndromeComorbidity", "Comorbidity", "Symptoms", "Aff", "Eff", "Gender", "Genesis")
  AVF[, to_numeric] <<- lapply(AVF[, to_numeric], as.numeric)
  AVF[, to_character] <<- lapply(AVF[, to_character], as.character)
}

subsets_maken <- function(){
  ## SUBSETS MAKEN
  endovascular_grep <<- "DL|DNC|DO|EMB|ENV|EAVP\\d?|EC|ECS|EDB|EE|EG|EL|ENC|EO|EP|EPVA"
  surgical_grep <<- "SUR|SBR|SE|SL|SR|SS|SC"
  refused_grep <<- "TREF"
  none_grep <<- "TNO"
  multiple_grep <<- "\\w{2,}"
  subclavian_brachiocephalic_grep <<- "SV$|BCV$"
  afferent <<- c("ANGA", "ACA", "AA", "APA", "BCA", "CCA", "CCT", "DCA", "DTA", "DNA", "EA", "ECA", "FA", "IOV", "ITT", "ICA", "LA", "MA", "MMA", "OA", "OPA", "PA", "PAA", "RA", "SPA", "SA", "SLA", "STA", "SUBLA", "STHA", "SOA", "STRA", "TT", "TA", "VA")
  efferent <<- c("AV", "BCV", "CEVP", "EV", "EVP", "EJV", "FV", "FRV", "FCV", "IJV", "IVS", "JV", "OCV", "OV", "PVP", "PMP", "PAV", "PP", "RMV", "SCV", "SS", "SV", "SVP", "STV", "SOV", "STHV", "VV")
  
  symptoms_all <<- c("ALL", "ANG", "BR", "CM", "CHEM", "DISF", "DIZZ", "DYSF", "DYSP", "EPIP", "EXOPH", "HEAD", "HL", "HEART", "HEARTR", "HIP", "HOAR", "JC", "LTOO", "LLE", "LLMW", "MUR", "SNO", "ORBS", "PAIN", "PALP", "PERC", "PROP", "PULM", "PTIN", "SAH", "SCNO", "SWELL", "SYNC", "TACH", "TR", "TRI", "TIN", "VAR", "VERT")
  symptoms_table <<- c("ALC|ATAX|DYSA|INTB|INES|INST|SAH|SEIZ|TIA|VOM", "BLEED|BLEEDT|ECCY|GUMB|HEMO", "INCO|MYE|NEDE|QUA|RADI|SPCC|ULE|UAI|UAMW", "BLV|DECV|DIP|VFD", "CHEM|EPIP|EXOPH|HIP|ORBS|PROP", "ANG|CM|HEART|HEARTR|PALP|TACH", "TIN|PTIN", "TR|BR|MUR")
  symptoms_names <<- c("Intracranial", "Extracranial bloodloss", "Neurologic deficit", "Visual impairment", "Ocular symptoms", "Cardial symptoms", "Subjective sound", "Objective sound")
  
  treatments <<- c("DL", "DNC", "DO", "EMB", "ENV", "EAVP", "EC", "ECS", "EDB", "EE", "EG", "EL", "ENC", "EO", "EP", "EPVA", "TNO", "TREF", "SUR", "SBR", "SE", "SL", "SR", "SS", "SC", "EB", "SEEM", "SLEM", "R") 
  genesis <<- c("C", "OK", "OKA", "OKCEA", "OKCVC", "OKCK", "OKCS", "OKCNB", "OKCABG", "OKCRT", "OKDP", "OKENDO", "OKES", "OKFF", "OKHT", "OKHC", "OKH", "OKCT", "OKICD", "OKFO", "OKLB", "OKPLR", "OKRM", "OKR", "OKSS", "OKSF", "OKS", "OKTT", "OKTB", "R", "S", "SM", "TB", "TCF", "TNM", "TP", "TVF", "T", "THE")
  comorbidities <<- c("ALHE", "ASD", "BH", "CMP", "CEA", "CH", "CT", "CM", "CHB", "CPOD", "CHD", "DM", "GR", "HEART", "HEART R/L", "HEARTOK", "HD", "HEPC", "HH", "VHL", "HC", "HT", "INTA", "IJVC", "LET", "LDD", "LC", "LF", "LT", "MA", "MOYA", "MG", "MCI", "NB", "NF1", "OV", "PMI", "PA", "PC", "PR", "RF", "RA", "RHT", "THN", "VM", "VSD")
  primair_AVF <<- filter(AVF, grepl("^S|^C", Genesis)) # primaire oorzaak AVF
  secundair_AVF <<- filter(AVF, grepl("^OK|^T|^sec", Genesis)) # secundaire oorzaak AVF
  multiple_AVF <<- filter(AVF, grepl(multiple_grep, Treatment)) # multipele behandelingen
  
  #subsets nog gemixed (met subsets waarbij meerdere behandelingen zijn gedaan)
  endovascular_AVF <<- filter(AVF, grepl(endovascular_grep, Treatment))
  surgical_AVF <<- filter(AVF, grepl(surgical_grep, Treatment))
  refused_AVF <<- filter(AVF, grepl(refused_grep, Treatment))
  multiple_AVF <<- filter(AVF, grepl(multiple_grep, Treatment))
  none <<- filter(AVF, grepl(none_grep, Treatment))
  both <<- endovascular_AVF[(endovascular_AVF$Treatment %in% surgical_AVF$Treatment),]
  subclavian_brachiocephalic <<- filter(AVF, grepl(subclavian_brachiocephalic_grep, Eff)) 
  
  #subsets
  pure_endovascular <<- endovascular_AVF[!(endovascular_AVF$Treatment %in% surgical_AVF$Treatment),]
  pure_surgical <<- surgical_AVF[!(surgical_AVF$Treatment %in% endovascular_AVF$Treatment),]
  secundair_refused <<- secundair_AVF[(secundair_AVF$Treatment %in% refused_AVF$Treatment),]
}