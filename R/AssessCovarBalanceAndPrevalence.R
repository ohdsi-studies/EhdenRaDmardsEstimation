# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of EhdenRaDmardsEstimation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @export
assessCovarBalanaceAndPrevalence <- function(shinyDataFolder,
                                             studyFolder) {

  databasefiles <- list.files(shinyDataFolder, pattern = "database_")
  databases <- sub(".*database_*(.*?) *.rds", "\\1", databasefiles)
  databases <- databases[databases != "Meta-analysis"]
  
  getComparisonSummaries <- function(database) {
    comparisonSummary <- readRDS(file.path(shinyDataFolder, paste0("comparison_summary_", database, ".rds")))
    comparisonSummary <- comparisonSummary[c("target_id", "comparator_id", "database_id")]
    comparisonSummary <- comparisonSummary[comparisonSummary$comparator_id == 219, ] # keep only methotrexate as comparator
    return(comparisonSummary)
  }
  comparisonSummaries <- lapply(databases, getComparisonSummaries)
  comparisonSummaries <- do.call(rbind, comparisonSummaries)
  
  getCovariateSummaries <- function(database) {
    covariateSummary <- readRDS(file.path(shinyDataFolder, paste0("covariate_", database, ".rds")))
    return(covariateSummary)
  }
  covariateSummaries <- lapply(databases, getCovariateSummaries)
  covariateSummaries <- do.call(rbind, covariateSummaries)
  covariateSummaries <- covariateSummaries[c("covariate_id", "covariate_name")]
  covariateSummaries <- covariateSummaries[!duplicated(covariateSummaries), ]
  
  exposures <- readRDS(file.path(shinyDataFolder, paste0("exposure_of_interest_", databases[1],".rds")))[, 1:2]
  
  getCovarBalSummaries <- function(database, comparisonSummaries) { # database <- databases[1]
    comparisonSummary <- comparisonSummaries[comparisonSummaries$database_id == database, ]
    fullBal <- data.frame()
    for (i in 1:nrow(comparisonSummary)) { # i = 1
      targetId <- comparisonSummary$target_id[i]
      comparatorId <- comparisonSummary$comparator_id[i]
      database <- comparisonSummary$database_id[i]
      bal <- readRDS(file.path(shinyDataFolder, paste0("covariate_balance_t", targetId, "_c", comparatorId, "_", database, ".rds")))
      bal$covariate_analysis_id <- as.numeric(substr(bal$covariate_id, nchar(bal$covariate_id)-2, nchar(bal$covariate_id)))
      bal <- bal[!bal$covariate_analysis_id %in% c(706, 901, 902, 904, 920), ] # not measurement value, charlson, dcsi, chads2vasc, visits
      bal <- bal[, c("database_id", "target_id", "comparator_id", "analysis_id", "covariate_id", "target_mean_after", "comparator_mean_after", "std_diff_after")]
      fullBal <- rbind(fullBal, bal)
    }
    return(fullBal)
  }
  covarBalSummaries <- lapply(databases, getCovarBalSummaries, comparisonSummaries) # databases[c(2,8)]
  covarBalSummaries <- do.call(rbind, covarBalSummaries)
  covarBalSummaries$blind <- ifelse(abs(covarBalSummaries$std_diff_after) > 0.1 & 
                                    abs(pmin(covarBalSummaries$target_mean_after, 1-covarBalSummaries$target_mean_after) - pmin(covarBalSummaries$comparator_mean_after, 1-covarBalSummaries$comparator_mean_after)) > 0.05, 1, 0)
  blindCovariates <- covarBalSummaries[covarBalSummaries$blind == 1, ]
  blindCovariates <- merge(covariateSummaries, blindCovariates)
  blindCovariates <- merge(exposures, blindCovariates, by.x = "exposure_id", by.y = "comparator_id")
  names(blindCovariates)[names(blindCovariates) == "exposure_name"] <- "comparator_name"
  names(blindCovariates)[names(blindCovariates) == "exposure_id"] <- "comparator_id"
  blindCovariates <- merge(exposures, blindCovariates, by.x = "exposure_id", by.y = "target_id")
  names(blindCovariates)[names(blindCovariates) == "exposure_name"] <- "target_name"
  names(blindCovariates)[names(blindCovariates) == "exposure_id"] <- "target_id"
  write.csv(blindCovariates, file.path(studyFolder, "blindCovarBalSummaries.csv"), row.names = FALSE)
  toBlind <- unique(blindCovariates[, c("database_id", "analysis_id", "target_id", "target_name", "comparator_id", "comparator_name")])
  saveRDS(toBlind, file.path(shinyDataFolder, "to_blind.rds"))
  write.csv(toBlind, file.path(studyFolder, "toBlind.csv"), row.names = FALSE)
}
