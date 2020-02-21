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
createPlotsAndTables <- function(resultsFolder,
                                 reportFolder,
                                 createCountsTable = FALSE,
                                 createCharsTables = FALSE,
                                 createEventsTables = FALSE,
                                 createForestPlots = FALSE,
                                 createDiagnosticsPlots = FALSE) {
  
  if (!file.exists(reportFolder)) {
    dir.create(reportFolder)
  }
  
  source("extras/getResults/global.R")
  source("extras/getResults/dataClean.R") # add clean databaseName to database ref object; fix here (extras/getResults) and in shiny (EvideneExplorer)
  source("extras/getResults/PlotsAndTables.R")
  databaseIds <- c("Amb_EMR", "CCAE", "GERMANY", "MDCR", "Optum", "PanTher", "SIDIAP", "THIN", "Meta-analysis")
  
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 219] <- "MTX"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 224] <- "HCQ"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 225] <- "SSZ"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 226] <- "LEF"
  
  if (createCountsTable) {
    keeps <- ((attrition$outcomeId %in% c(187, 193, 197, 203, 253) & attrition$analysisId %in% c(7:10)) | # infections, leukopenia, pancytopenia
                (attrition$outcomeId %in% c(182:185) & attrition$analysisId %in% c(1, 2, 4, 5)) | # cardiovacular
                (attrition$outcomeId %in% c(212, 223, 216, 218, 201) & attrition$analysisId %in% c(3, 6)) & # oncology
                attrition$comparatorId == 219) # keep only methotrexate as comparator
    patientCounts <- attrition[keeps, ]
    patientCounts <- patientCounts[patientCounts$description == "Original cohorts" & patientCounts$databaseId %in% databaseIds,
                               c("databaseId", "exposureId", "subjects")]
    patientCounts <- aggregate(patientCounts, by = patientCounts[c("databaseId", "exposureId")], FUN = max)[3:5] # dunno why extra colummns being added
    exposureTotals <- aggregate(patientCounts["subjects"], by = patientCounts["exposureId"], FUN = sum)
    names(exposureTotals)[2] <- "exposureSubjects"
    databaseTotals <- aggregate(patientCounts["subjects"], by = patientCounts["databaseId"], FUN = sum)
    names(databaseTotals)[2] <- "databaseSubjects"
    patientCounts <- merge(patientCounts, exposureTotals)
    patientCounts$exposurePercent <- round(patientCounts$subjects / patientCounts$exposureSubjects * 100, 2)
    patientCounts <- merge(exposureOfInterest, patientCounts)
    patientCounts <- patientCounts[-c(1,3)]
    patientCounts <- merge(patientCounts, databaseTotals)
    patientCounts$databasePercent <- round(patientCounts$subjects / patientCounts$databaseSubjects * 100, 2)
    write.csv(patientCounts, file.path(reportFolder, "patientCounts.csv"), row.names = FALSE)  
  }
  
  if (createCharsTables) {
    charsFolder <- file.path(reportFolder, "chars")
    if (!file.exists(charsFolder)) {
      dir.create(charsFolder, recursive = TRUE)
    }
    
    headings <- list()
    titles <- list()
    table1s <- list()
    comparisonSummary <- comparisonSummary[comparisonSummary$databaseId %in% databaseIds, ] # before matching C sizes differ slightly because getDbCohortMethodData restrictToCommonPeriod=TRUE; no wide table by database :(
    
    for (i in 1:nrow(comparisonSummary)) {  # i = 1
      databaseName <- database$databaseName[database$databaseId == comparisonSummary$databaseId[i]] # see fix above
      databaseId <- comparisonSummary$databaseId[i]
      targetName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparisonSummary$targetId[i]]
      comparatorName <- exposureOfInterest$exposureName[exposureOfInterest$exposureId == comparisonSummary$comparatorId[i]]
      targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$targetId[i]]
      comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$comparatorId[i]]
      heading <- paste("Chars -", targetLabel, comparatorLabel, databaseName)
      title <- sprintf("Table %d: Patient characteristics for new users of %s and %s in the %s database. %s=%s. %s=%s.",
                       i, tolower(targetName), tolower(comparatorName), databaseName, targetLabel, targetName, comparatorLabel, comparatorName)
      
      balance <- getCovariateBalance(resultsFolder = resultsFolder,
                                     targetId = comparisonSummary$targetId[i],
                                     comparatorId = comparisonSummary$comparatorId[i],
                                     databaseId = databaseId,
                                     analysisId = 7, # No prior outcome ( 30d), On-treatment +14d, 5 PS strata
                                     outcomeId = 193) # pancytopenia, rarest outcome, largest sample
      table1 <- prepareTable1(balance,
                                beforeLabel = "Before PS stratification",
                                afterLabel = "After PS stratification",
                                targetLabel = targetLabel,
                                comparatorLabel = comparatorLabel,
                                percentDigits = 1,
                                stdDiffDigits = 2,
                                output = "latex",
                                pathToCsv = "inst/shiny/EvidenceExplorer/Table1Specs.csv")
      facs <- sapply(table1, is.factor)
      table1[facs] <- lapply(table1[facs], as.character)
      rm(facs)
      
      names(table1)[names(table1) == ""] <- c(letters[1:5]) # officer requires non-nul, unique colnames
      table1 <- flextable::qflextable(table1)
      table1 <- flextable::fontsize(table1, part = "all", size = 7) 
      table1 <- flextable::align(table1, j = 1, align = "left", part = "all")
      table1 <- flextable::merge_h_range(table1, j1 = 2 , j2 = 4, part = "header")
      table1 <- flextable::merge_h_range(table1, j1 = 5 , j2 = 7, part = "header")
      table1 <- flextable::set_header_labels(table1, a = "")
      table1 <- flextable::autofit(table1, add_w = 0, add_h = 0)
      
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      table1s[[length(table1s) + 1]] <- table1
      # fileName <- file.path(charsFolder, sprintf("Chars %s vs %s %s", targetLabel, comparatorLabel, databaseId) 
      # write.csv(table1, paste0(fileName, ".csv"), row.names = FALSE)
      # saveRDS(table1, paste0(fileName, ".rds"))
    }
    
    # fix this hardcoding
    doc <- officer::read_docx() %>%
      
      officer::body_add_par(headings[[1]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[1]]) %>%
      flextable::body_add_flextable(table1s[[1]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[2]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[2]]) %>%
      flextable::body_add_flextable(table1s[[2]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[3]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[3]]) %>%
      flextable::body_add_flextable(table1s[[3]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[4]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[4]]) %>%
      flextable::body_add_flextable(table1s[[4]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[5]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[5]]) %>%
      flextable::body_add_flextable(table1s[[5]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[6]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[6]]) %>%
      flextable::body_add_flextable(table1s[[6]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[7]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[7]]) %>%
      flextable::body_add_flextable(table1s[[7]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[8]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[8]]) %>%
      flextable::body_add_flextable(table1s[[8]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[9]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[9]]) %>%
      flextable::body_add_flextable(table1s[[9]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[10]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[10]]) %>%
      flextable::body_add_flextable(table1s[[10]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[11]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[11]]) %>%
      flextable::body_add_flextable(table1s[[11]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[12]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[12]]) %>%
      flextable::body_add_flextable(table1s[[12]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[13]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[13]]) %>%
      flextable::body_add_flextable(table1s[[13]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[14]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[14]]) %>%
      flextable::body_add_flextable(table1s[[14]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[15]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[15]]) %>%
      flextable::body_add_flextable(table1s[[15]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[16]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[16]]) %>%
      flextable::body_add_flextable(table1s[[16]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[17]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[17]]) %>%
      flextable::body_add_flextable(table1s[[17]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[18]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[18]]) %>%
      flextable::body_add_flextable(table1s[[18]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[19]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[19]]) %>%
      flextable::body_add_flextable(table1s[[19]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[20]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[20]]) %>%
      flextable::body_add_flextable(table1s[[20]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[21]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[21]]) %>%
      flextable::body_add_flextable(table1s[[21]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[22]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[22]]) %>%
      flextable::body_add_flextable(table1s[[22]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[23]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[23]]) %>%
      flextable::body_add_flextable(table1s[[23]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[24]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[24]]) %>%
      flextable::body_add_flextable(table1s[[24]]) %>%
      
      print(target = file.path(charsFolder, "chars.docx"))
  }
  
  if (createEventsTables) {
    eventsFolder <- file.path(reportFolder, "events")
    if (!file.exists(eventsFolder)) {
      dir.create(eventsFolder, recursive = TRUE)
    }
    
    headings <- list()
    titles <- list()
    eventTables <- list()
    tableNumber <- 0
    
    # TCs
    targetIds <- c(224, 225, 226)
    targetNames <- c("HCQ", "SSZ", "LEF")
    comparatorIds <- c(219, 219, 219)
    comparatorNames <- c("MTX", "MTX", "MTX")
    
    # leuko/pancyto outcomes/analyses
    outcomeNames <- c("Leukopenia", "Pancytopenia")
    outcomeIds <- c(187, 193)
    analysisIds <- c(7, 8, 9, 10)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) { # analysisId = 7
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      tableNumber <- tableNumber + 1
      heading <- sprintf("Events - Leukopenia, pancytopenia - %s", analysisName)
      title <- sprintf("Table %d: Patient counts, time-at-risk, outcome events, incidence rates, and minimum detectable relative risk of leukopenia and pancytopenia for csDMARD new users in the %s analysis.",
                       tableNumber, analysisName)
      tables <- createTables(analysisRef, databaseIds, exposureOfInterest)
      eventTable <- do.call(rbind, tables)
      header1 <- c("Outcome", "T vs. C", "Source", rep(c("T", "C"), 4), "MDRR")
      eventTable <- rbind(header1, eventTable)
      eventTable <- flextable::qflextable(eventTable)
      eventTable <- flextable::fontsize(eventTable, part = "all", size = 7) 
      eventTable <- flextable::set_header_labels(eventTable, 
                                                 outcomeName = "",
                                                 comparison = "",
                                                 databaseId = "",
                                                 targetSubjects = "Patients",
                                                 comparatorSubjects = "",
                                                 targetYears = "Person-years",
                                                 comparatorYears = "",
                                                 targetOutcomes = "Events", 
                                                 comparatorOutcomes = "",
                                                 targetIr = "Incidence rate",
                                                 comparatorIr = "",
                                                 mdrr = "")
      eventTable <- flextable::align(eventTable, j = 1:3, align = "left", part = "all")
      eventTable <- flextable::bold(eventTable, i = 1, bold = TRUE, part = "body")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 4 , j2 = 5, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 6 , j2 = 7, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 8 , j2 = 9, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 10 , j2 = 11, part = "header")
      eventTable <- flextable::merge_v(eventTable, j = 1:2, part = "body")
      std_border <- officer::fp_border()
      eventTable <- flextable::hline(eventTable, i = 1, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 10, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 19, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 28, j = 1:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 37, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 46, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::autofit(eventTable, add_w = 0, add_h = 0)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      eventTables[[length(eventTables) + 1]] <- eventTable
    }
    
    # infection outcomes/analyses
    outcomeNames <- c("Opportunistic infection", "Serious infection", "All infections")
    outcomeIds <- c(197, 203, 253)
    analysisIds <- c(7, 8, 9, 10)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) { # analysisId = 7
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      tableNumber <- tableNumber + 1
      heading <- sprintf("Events - Infections - %s", analysisName)
      title <- sprintf("Table %d: Patient counts, time-at-risk, outcome events, incidence rates, and minimum detectable relative risk of infection for csDMARD new users in the %s analysis.",
                       tableNumber, analysisName)
      tables <- createTables(analysisRef, databaseIds, exposureOfInterest)
      
      eventTable <- do.call(rbind, tables)
      header1 <- c("Outcome", "T vs. C", "Source", rep(c("T", "C"), 4), "MDRR")
      eventTable <- rbind(header1, eventTable)
      eventTable <- flextable::qflextable(eventTable)
      eventTable <- flextable::fontsize(eventTable, part = "all", size = 7) 
      eventTable <- flextable::set_header_labels(eventTable, 
                                                 outcomeName = "",
                                                 comparison = "",
                                                 databaseId = "",
                                                 targetSubjects = "Patients",
                                                 comparatorSubjects = "",
                                                 targetYears = "Person-years",
                                                 comparatorYears = "",
                                                 targetOutcomes = "Events", 
                                                 comparatorOutcomes = "",
                                                 targetIr = "Incidence rate",
                                                 comparatorIr = "",
                                                 mdrr = "")
      eventTable <- flextable::align(eventTable, j = 1:3, align = "left", part = "all")
      eventTable <- flextable::bold(eventTable, i = 1, bold = TRUE, part = "body")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 4 , j2 = 5, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 6 , j2 = 7, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 8 , j2 = 9, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 10 , j2 = 11, part = "header")
      eventTable <- flextable::merge_v(eventTable, j = 1:2, part = "body")
      std_border <- officer::fp_border()
      eventTable <- flextable::hline(eventTable, i = 1, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 10, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 19, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 28, j = 1:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 37, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 46, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 55, j = 1:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 63, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 71, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::autofit(eventTable, add_w = 0, add_h = 0)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      eventTables[[length(eventTables) + 1]] <- eventTable
    }
    
    # CV outcomes/analyses
    outcomeNames <- c("MI", "Stroke") # any visit
    outcomeIds <- c(185, 183)
    analysisIds <- c(1, 2, 4, 5)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    ref <- ref[order(ref$analysisId, rev(ref$outcomeId), ref$targetId), ]
    for (analysisId in unique(ref$analysisId)) { # analysisId = 1
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      tableNumber <- tableNumber + 1
      heading <- sprintf("Events - Cardiovascular - %s", analysisName)
      title <- sprintf("Table %d: Patient counts, time-at-risk, outcome events, incidence rates, and minimum detectable relative risk of caridovascular outcomes for csDMARD new users in the %s analysis.",
                       tableNumber, analysisName)
      tables <- createTables(analysisRef, databaseIds, exposureOfInterest)
      
      eventTable <- do.call(rbind, tables)
      header1 <- c("Outcome", "T vs. C", "Source", rep(c("T", "C"), 4), "MDRR")
      eventTable <- rbind(header1, eventTable)
      eventTable <- flextable::qflextable(eventTable)
      eventTable <- flextable::fontsize(eventTable, part = "all", size = 7) 
      eventTable <- flextable::set_header_labels(eventTable, 
                                                 outcomeName = "",
                                                 comparison = "",
                                                 databaseId = "",
                                                 targetSubjects = "Patients",
                                                 comparatorSubjects = "",
                                                 targetYears = "Person-years",
                                                 comparatorYears = "",
                                                 targetOutcomes = "Events", 
                                                 comparatorOutcomes = "",
                                                 targetIr = "Incidence rate",
                                                 comparatorIr = "",
                                                 mdrr = "")
      eventTable <- flextable::align(eventTable, j = 1:3, align = "left", part = "all")
      eventTable <- flextable::bold(eventTable, i = 1, bold = TRUE, part = "body")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 4 , j2 = 5, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 6 , j2 = 7, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 8 , j2 = 9, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 10 , j2 = 11, part = "header")
      eventTable <- flextable::merge_v(eventTable, j = 1:2, part = "body")
      std_border <- officer::fp_border()
      eventTable <- flextable::hline(eventTable, i = 1, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 10, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 19, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 28, j = 1:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 37, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 46, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::autofit(eventTable, add_w = 0, add_h = 0)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      eventTables[[length(eventTables) + 1]] <- eventTable
    }
    
    # cancer outcome/analyses
    outcomeNames <- c("All cancers")
    outcomeIds <- c(223)
    analysisIds <- c(3, 6)
    analysisNames <- c("PS stratified, delayed intent-to-treat", "PS matched, delayed intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) { # analysisId = 3
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      tableNumber <- tableNumber + 1
      heading <- sprintf("Events - Cancer - %s", analysisName)
      title <- sprintf("Table %d: Patient counts, time-at-risk, outcome events, incidence rates, and minimum detectable relative risk of cancer for csDMARD new users in the %s analysis.",
                       tableNumber, analysisName)
      tables <- createTables(analysisRef, databaseIds, exposureOfInterest)
      eventTable <- do.call(rbind, tables)
      header1 <- c("Outcome", "T vs. C", "Source", rep(c("T", "C"), 4), "MDRR")
      eventTable <- rbind(header1, eventTable)
      
      eventTable <- flextable::qflextable(eventTable)
      eventTable <- flextable::fontsize(eventTable, part = "all", size = 7) 
      eventTable <- flextable::set_header_labels(eventTable, 
                                                 outcomeName = "",
                                                 comparison = "",
                                                 databaseId = "",
                                                 targetSubjects = "Patients",
                                                 comparatorSubjects = "",
                                                 targetYears = "Person-years",
                                                 comparatorYears = "",
                                                 targetOutcomes = "Events", 
                                                 comparatorOutcomes = "",
                                                 targetIr = "Incidence rate",
                                                 comparatorIr = "",
                                                 mdrr = "")
      eventTable <- flextable::align(eventTable, j = 1:3, align = "left", part = "all")
      eventTable <- flextable::bold(eventTable, i = 1, bold = TRUE, part = "body")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 4 , j2 = 5, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 6 , j2 = 7, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 8 , j2 = 9, part = "header")
      eventTable <- flextable::merge_h_range(eventTable, j1 = 10 , j2 = 11, part = "header")
      eventTable <- flextable::merge_v(eventTable, j = 1:2, part = "body")
      std_border <- officer::fp_border()
      eventTable <- flextable::hline(eventTable, i = 1, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 10, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::hline(eventTable, i = 19, j = 2:12, border = std_border, part = "body")
      eventTable <- flextable::autofit(eventTable, add_w = 0, add_h = 0)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      eventTables[[length(eventTables) + 1]] <- eventTable
    }
    
    doc <- officer::read_docx() %>%
      
      officer::body_add_par(headings[[1]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[1]]) %>%
      flextable::body_add_flextable(eventTables[[1]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[2]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[2]]) %>%
      flextable::body_add_flextable(eventTables[[2]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[3]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[3]]) %>%
      flextable::body_add_flextable(eventTables[[3]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[4]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[4]]) %>%
      flextable::body_add_flextable(eventTables[[4]]) %>%
      officer::body_add_break() %>%
      
      
      officer::body_add_par(headings[[5]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[5]]) %>%
      flextable::body_add_flextable(eventTables[[5]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[6]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[6]]) %>%
      flextable::body_add_flextable(eventTables[[6]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[7]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[7]]) %>%
      flextable::body_add_flextable(eventTables[[7]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[8]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[8]]) %>%
      flextable::body_add_flextable(eventTables[[8]]) %>%
      officer::body_add_break() %>%
      
      
      officer::body_add_par(headings[[9]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[9]]) %>%
      flextable::body_add_flextable(eventTables[[9]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[10]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[10]]) %>%
      flextable::body_add_flextable(eventTables[[10]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[11]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[11]]) %>%
      flextable::body_add_flextable(eventTables[[11]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[12]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[12]]) %>%
      flextable::body_add_flextable(eventTables[[12]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[13]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[13]]) %>%
      flextable::body_add_flextable(eventTables[[13]]) %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[14]], style = "heading 2") %>% 
      officer::body_add_fpar(titles[[14]]) %>%
      flextable::body_add_flextable(eventTables[[14]]) %>%
      
      print(target = file.path(eventsFolder, "eventTable.docx"))
    
  }
  
  if (createForestPlots) {
    hrsFolder <- file.path(reportFolder, "HRs")
    if (!file.exists(hrsFolder)) {
      dir.create(hrsFolder, recursive = TRUE)
    }
    
    headings <- list()
    titles <- list()
    figureNumber <- 0
    fileNames <- list()
    
    # TCs
    targetIds <- c(224, 225, 226)
    targetNames <- c("HCQ", "SSZ", "LEF")
    comparatorIds <- c(219, 219, 219)
    comparatorNames <- c("MTX", "MTX", "MTX")
    
    # leuko/pancyto outcomes/analyses
    outcomeNames <- c("Leukopenia", "Pancytopenia")
    outcomeIds <- c(187, 193)
    analysisIds <- c(7, 8, 9, 10)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) { # analysisId <- 1
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      figureNumber <- figureNumber + 1
      heading <- sprintf("HR plots - Leukopenia, Pancytopenia - %s", analysisName)
      title <- sprintf("Figure %d: comparative risk of leukopenia and pancytopenia among new users of hydroxychloroquine, sulfasalazine, and leflunomide relative to methotrexate in the %s analysis.",
                       figureNumber, analysisName)
      plots <- createPlots(analysisRef, databaseIds)
      row0 <- grid::textGrob("")
      row1 <- grid::textGrob("HCQ", rot = 90, gp = grid::gpar(fontsize = 18))
      row2 <- grid::textGrob("SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
      row3 <- grid::textGrob("LEF", rot = 90, gp = grid::gpar(fontsize = 18))
      col1 <- grid::textGrob("Leukopenia", gp = grid::gpar(fontsize = 18))
      col2 <- grid::textGrob("Pancytopenia", gp = grid::gpar(fontsize = 18))
      plotGrid <- gridExtra::grid.arrange(row0, col1, col2,
                                          row1, plots[[1]], plots[[4]],
                                          row2, plots[[2]], plots[[5]],
                                          row3, plots[[3]], plots[[6]],
                                          nrow = 4,
                                          heights = c(1, 4, 4, 4),
                                          widths = c(0.25, 3, 3))
      fileName <- file.path(hrsFolder, paste0("HRs LeukoPancyto ", analysisRef$analysisName[1], ".png"))
      ggplot2::ggsave(fileName, plotGrid, width = 14, height = 7, dpi = 400)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      fileNames[[length(fileNames) + 1]] <- fileName
    }
    
    # infection outcomes/analyses
    outcomeNames <- c("Opportunistic infection", "Serious infection", "All infections")
    outcomeIds <- c(197, 203, 253)
    analysisIds <- c(7, 8, 9, 10)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) { # analysisId <- 7
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      figureNumber <- figureNumber + 1
      heading <- sprintf("HR plots - Infections - %s", analysisName)
      title <- sprintf("Figure %d: comparative risk of infection outcomes among new users of hydroxychloroquine, sulfasalazine, and leflunomide relative to methotrexate in the %s analysis.",
                       figureNumber, analysisName)
      plots <- createPlots(analysisRef, databaseIds)
      row0 <- grid::textGrob("")
      row1 <- grid::textGrob("HCQ", rot = 90, gp = grid::gpar(fontsize = 18))
      row2 <- grid::textGrob("SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
      row3 <- grid::textGrob("LEF", rot = 90, gp = grid::gpar(fontsize = 18))
      col1 <- grid::textGrob("Opp. infection", gp = grid::gpar(fontsize = 18))
      col2 <- grid::textGrob("Ser. infection", gp = grid::gpar(fontsize = 18))
      col3 <- grid::textGrob("Opp., Ser., and other infections", gp = grid::gpar(fontsize = 18))
      plotGrid <- gridExtra::grid.arrange(row0, col1, col2, col3,
                                          row1, plots[[1]], plots[[4]], plots[[7]],
                                          row2, plots[[2]], plots[[5]], plots[[8]],
                                          row3, plots[[3]], plots[[6]], plots[[9]],
                                          nrow = 4,
                                          heights = c(1, 4, 4, 4),
                                          widths = c(0.25, 3, 3, 3))
      fileName <- file.path(hrsFolder, paste0("HRs Infection ", analysisRef$analysisName[1], ".png"))
      ggplot2::ggsave(fileName, plotGrid, width = 21, height = 7, dpi = 400)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      fileNames[[length(fileNames) + 1]] <- fileName
    }
    
    # CV outcomes/analyses
    outcomeNames <- c("MI", "Stroke") # any visit
    outcomeIds <- c(185, 183)
    analysisIds <- c(1, 2, 4, 5)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat", "PS matched, on-treatment", "PS matched, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    ref <- ref[order(ref$analysisId, rev(ref$outcomeId), ref$targetId), ]
    for (analysisId in unique(ref$analysisId)) { # analysisId = 1
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      figureNumber <- figureNumber + 1
      heading <- sprintf("HR plots - Cardiovascular - %s", analysisName)
      title <- sprintf("Figure %d: comparative risk of cardiovascular outcomes among new users of hydroxychloroquine, sulfasalazine, and leflunomide relative to methotrexate in the %s analysis.",
                       figureNumber, analysisName)
      
      plots <- createPlots(analysisRef, databaseIds)
      row0 <- grid::textGrob("")
      row1 <- grid::textGrob("HCQ", rot = 90, gp = grid::gpar(fontsize = 18))
      row2 <- grid::textGrob("SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
      row3 <- grid::textGrob("LEF", rot = 90, gp = grid::gpar(fontsize = 18))
      col1 <- grid::textGrob("MI", gp = grid::gpar(fontsize = 18))
      col2 <- grid::textGrob("Stroke", gp = grid::gpar(fontsize = 18))
      plotGrid <- gridExtra::grid.arrange(row0, col1, col2,
                                          row1, plots[[1]], plots[[4]],
                                          row2, plots[[2]], plots[[5]],
                                          row3, plots[[3]], plots[[6]],
                                          nrow = 4,
                                          heights = c(1, 4, 4, 4),
                                          widths = c(0.25, 3, 3))
      fileName <- file.path(hrsFolder, paste0("HRs CV ", analysisRef$analysisName[1], ".png"))
      ggplot2::ggsave(fileName, plotGrid, width = 14, height = 7, dpi = 400)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      fileNames[[length(fileNames) + 1]] <- fileName
    }
    
    # cancer outcome/analyses
    outcomeNames <- c("All cancers")
    outcomeIds <- c(223)
    analysisIds <- c(3, 6)
    analysisNames <- c("PS stratified, delayed intent-to-treat", "PS matched, delayed intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    for (analysisId in unique(ref$analysisId)) {
      analysisRef <- ref[ref$analysisId == analysisId, ]
      analysisName <- analysisRef$analysisName[1]
      figureNumber <- figureNumber + 1
      heading <- sprintf("HR plots - Cancer - %s", analysisName)
      title <- sprintf("Figure %d: comparative risk of any cancer among new users of hydroxychloroquine, sulfasalazine, and leflunomide relative to methotrexate in the %s analysis.",
                       figureNumber, analysisName)
      plots <- createPlots(analysisRef, databaseIds)
      row0 <- grid::textGrob("")
      row1 <- grid::textGrob("HCQ", rot = 90, gp = grid::gpar(fontsize = 18))
      row2 <- grid::textGrob("SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
      row3 <- grid::textGrob("LEF", rot = 90, gp = grid::gpar(fontsize = 18))
      col1 <- grid::textGrob("Any cancer", gp = grid::gpar(fontsize = 18))
      plotGrid <- gridExtra::grid.arrange(row0, col1,
                                          row1, plots[[1]],
                                          row2, plots[[2]],
                                          row3, plots[[3]],
                                          nrow = 4,
                                          heights = c(1, 4, 4, 4),
                                          widths = c(0.25, 3))
      fileName <- file.path(hrsFolder, paste0("HRs Cancers ", analysisRef$analysisName[1], ".png"))
      ggplot2::ggsave(fileName, plotGrid, width = 7, height = 7, dpi = 400)
      titleFormat <- officer::fp_text(font.size = 10, bold = FALSE, font.family = "Calibri")
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      headings[[length(headings) + 1]] <- heading
      titles[[length(titles) + 1]] <- title
      fileNames[[length(fileNames) + 1]] <- fileName
    }
    
    doc <- officer::read_docx() %>%
      
      officer::body_add_par(headings[[1]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[1]]) %>%
      officer::body_add_img(fileNames[[1]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[2]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[2]]) %>%
      officer::body_add_img(fileNames[[2]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[3]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[3]]) %>%
      officer::body_add_img(fileNames[[3]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[4]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[4]]) %>%
      officer::body_add_img(fileNames[[4]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      # landscape start
      officer::body_end_section_continuous() %>% 
      
      officer::body_add_par(headings[[5]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[5]]) %>%
      officer::body_add_img(fileNames[[5]], width = 9, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[6]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[6]]) %>%
      officer::body_add_img(fileNames[[6]], width = 9, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[7]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[7]]) %>%
      officer::body_add_img(fileNames[[7]], width = 9, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[8]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[8]]) %>%
      officer::body_add_img(fileNames[[8]], width = 9, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_end_section_landscape() %>%
      #landscape end
      
      officer::body_add_par(headings[[9]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[9]]) %>%
      officer::body_add_img(fileNames[[9]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[10]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[10]]) %>%
      officer::body_add_img(fileNames[[10]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[11]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[11]]) %>%
      officer::body_add_img(fileNames[[11]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[12]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[12]]) %>%
      officer::body_add_img(fileNames[[12]], width = 6, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[13]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[13]]) %>%
      officer::body_add_img(fileNames[[13]], width = 3, height = 3, style = "centered") %>%
      officer::body_add_break() %>%
      
      officer::body_add_par(headings[[14]], style = "heading 2") %>%
      officer::body_add_fpar(titles[[14]]) %>%
      officer::body_add_img(fileNames[[14]], width = 3, height = 3, style = "centered") %>%

      print(target = file.path(hrsFolder, "hrPlots.docx"))
  }
  
  if (createDiagnosticsPlots) {
    
  }
  
}