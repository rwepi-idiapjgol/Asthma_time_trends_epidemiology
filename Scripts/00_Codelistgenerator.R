# Codelistgenerator -------------------------------------------------------
library(CodelistGenerator)
getVocabVersion(cdm) #v5.0 27-FEB-25
getVocabularies(cdm = cdm)
getDomains(cdm)

asthma_codes <- getCandidateCodes(cdm=cdm,
                                  keywords= "asthma",
                                  standardConcept = "Standard",
                                  searchNonStandard = FALSE,
                                  domains = "Condition")
asthma_codes |> glimpse()
write.csv(asthma_codes, "E:/Asthma_time_trends/outcome_cohorts/asthma_dx_codes.csv", row.names = FALSE)

