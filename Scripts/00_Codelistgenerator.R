# Codelistgenerator -------------------------------------------------------
library(CodelistGenerator)
getVocabVersion(cdm)
getVocabularies(cdm = cdm)
getDomains(cdm)

asthma_codes <- getCandidateCodes(cdm=cdm,
                                  keywords= "asthma",
                                  standardConcept = "Standard",
                                  searchNonStandard = FALSE,
                                  domains = "Condition")
asthma_codes %>% glimpse()
write.csv(asthma_codes, "//EPOFS/RWCancerEpi/ONGOING PROJECTS/Asthma - Time trends-20240621T085916Z-001/Asthma - Time trends/Timetrends_asthma/Updated_Analysis_17072024/Results/Outcome cohorts/asthma_dx_codes.csv", row.names = FALSE)

