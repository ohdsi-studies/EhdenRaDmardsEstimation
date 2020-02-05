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
premergeShinyDataFiles <- function(dataFolder,
                                   newDataFolder) {

  if (!file.exists(newDataFolder)) {
    dir.create(newDataFolder)
  }
  
  # Copied from global.R ---------------------------------------------------------------------------
  splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
  
  files <- list.files(dataFolder, pattern = ".rds")
  
  # Find part to remove from all file names (usually databaseId):
  databaseFileName <- files[grepl("^database", files)]
  removeParts <- paste0(gsub("database", "", databaseFileName), "$")
  
  # Remove data already in global environment:
  for (removePart in removeParts) {
    tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)])) 
    camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
    camelCaseNames <- unique(camelCaseNames)
    camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
    suppressWarnings(
      rm(list = camelCaseNames)
    )
  }
  
  # Load data from data folder. R data objects will get names derived from the filename:
  loadFile <- function(file, removePart) {
    tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file)) 
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    if (!(tableName %in% splittableTables)) {
      newData <- readRDS(file.path(dataFolder, file))
      if (camelCaseName == "cohortMethodResult") {
        if (removePart != "_Meta-analysis.rds$") {
          newData$sources <- rep("", nrow(newData))
        }
      }
      colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
      if (exists(camelCaseName, envir = .GlobalEnv)) {
        existingData <- get(camelCaseName, envir = .GlobalEnv)
        newData <- rbind(existingData, newData)
        newData <- unique(newData)
      }
      assign(camelCaseName, newData, envir = .GlobalEnv)
    }
    invisible(NULL)
  }
  
  for (removePart in removeParts) {
    dummy <- lapply(files[grepl(removePart, files)], loadFile, removePart)
  }
  
  # Write merged data objects to new folder -------------------------------------------------------------
  removePart <- removeParts[1]
  tableNames <- gsub("_t([0-9]|NA)+_c([0-9]|NA)+$", "", gsub(removePart, "", files[grepl(removePart, files)])) 
  tableNames <- unique(tableNames)
  tableNames <- tableNames[!(tableNames %in% splittableTables)]
  
  saveTable <- function(tableName) {
    fileName <- file.path(newDataFolder, sprintf("%s_All.rds", tableName))
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    data <- get(camelCaseName, envir = .GlobalEnv)
    if (tableName == "covariate") {
      data$covariateName <- as.factor(data$covariateName)
    }
    colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
    saveRDS(data, fileName)
  }
  lapply(tableNames, saveTable)
  
  # Copy splittable tables ------------------------------------------------------------------------------
  toCopy <- files[grepl(paste(splittableTables, collapse = "|"), files)]
  file.copy(file.path(dataFolder, toCopy), file.path(newDataFolder, toCopy))
  
  # Copy to_blind file -------------------------------------------------------------------------------
  file.copy(file.path(dataFolder, "to_blind.rds"), file.path(newDataFolder, "to_blind.rds"))
}