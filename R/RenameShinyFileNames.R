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
renameShinyFileNames <- function(dataFolder) {
  
  files <- list.files(dataFolder, pattern = ".rds")
  splitTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
  
  for (splitTable in splitTables) { 
    splitTableFilesFrom <- file.path(dataFolder, grep(splitTable, files, value = TRUE))
    splitTablesFilesTo <- gsub("Amb_EMR", "AmbEMR", splitTableFilesFrom)
    splitTablesFilesTo <- gsub("BELGIUM", "DABelgium", splitTablesFilesTo)
    splitTablesFilesTo <- gsub("GERMANY", "DAGermany", splitTablesFilesTo)
    splitTablesFilesTo <- gsub("IPCI-HI-LARIOUS-RA", "ICPI", splitTablesFilesTo)
    splitTablesFilesTo <- gsub("LPDFRANCE", "LPDFrance", splitTablesFilesTo)
    splitTablesFilesTo <- gsub("Optum", "ClinFormatics", splitTablesFilesTo)
    splitTablesFilesTo <- gsub("PanTher", "OptumEHR", splitTablesFilesTo)
    file.rename(splitTableFilesFrom, splitTablesFilesTo)
  }
  
  toBlind <- readRDS(file.path(dataFolder, "to_blind.rds"))
  toBlind$database_id[toBlind$database_id == "Amb_EMR"] <- "AmbEMR"
  toBlind$database_id[toBlind$database_id == "BELGIUM"] <- "DABelgium"
  toBlind$database_id[toBlind$database_id == "GERMANY"] <- "DAGermany"
  toBlind$database_id[toBlind$database_id == "IPCI-HI-LARIOUS-RA"] <- "ICPI"
  toBlind$database_id[toBlind$database_id == "LPDFRANCE"] <- "LPDFrance"
  toBlind$database_id[toBlind$database_id == "Optum"] <- "ClinFormatics"
  toBlind$database_id[toBlind$database_id == "PanTher"] <- "OptumEHR"
  saveRDS(toBlind, file.path(dataFolder, "to_blind.rds"))
}