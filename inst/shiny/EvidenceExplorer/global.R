source("DataPulls.R")
source("PlotsAndTables.R")

#shinySettings <- list(dataFolder = "E:/jweave17/StudyResults/EhdenRaDmardsEstimation/ResultsBELGIUM/ShinyData", blind = TRUE)
shinySettings <- list(dataFolder = "E:/jweave17/StudyResults/EhdenRaDmardsEstimation/ShinyDataAll", blind = FALSE)

dataFolder <- shinySettings$dataFolder
blind <- shinySettings$blind
connection <- NULL
positiveControlOutcome <- NULL

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
  lapply(files[grepl(removePart, files)], loadFile, removePart)
}

tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]

outcomeOfInterest$definition <- NULL
outcomeOfInterest <- outcomeOfInterest[!duplicated(outcomeOfInterest), ]

cohortMethodAnalysis$definition <- NULL
cohortMethodAnalysis <- cohortMethodAnalysis[!duplicated(cohortMethodAnalysis), ]


keeps <- (cohortMethodResult$outcomeId %in% c(187, 193, 197, 203, 253) & cohortMethodResult$analysisId %in% c(7:10)) |  # infections, leukopenia, pancytopenia
  (cohortMethodResult$outcomeId %in% c(182:185) & cohortMethodResult$analysisId %in% c(1, 2, 4, 5)) | # cardiovacular
  (cohortMethodResult$outcomeId %in% c(212, 223, 216, 218, 201) & cohortMethodResult$analysisId %in% c(3, 6)) | # oncology
  cohortMethodResult$outcomeId %in% negativeControlOutcome$outcomeId # controls
cohortMethodResult <- cohortMethodResult[keeps, ]
