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
createManuscriptTables <- function(reportFolder,
                                   analysis, # "primary", "matchedOnTreatment", "strataItt", "matchedItt"
                                   createCountsTable = FALSE,
                                   createNntTable = FALSE,
                                   createCharsTables = FALSE,
                                   createEventsTables = FALSE,
                                   createForestPlots = FALSE,
                                   createDiagnosticPlots = FALSE) { 
  
  # reportFolder <- "G:/StudyResults/EhdenRaDmardsEstimation2/report2"
  
  # counts table ---------------------------------------------------------------
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
  
  # NNT ------------------------------------------------------------------------
  if (createNntTable) {
    nnt <- getNnt(targetIds = c(225, 226),
                  targetLabels = c("SSZ", "LEF"),
                  comparatorIds = c(219, 219),
                  comparatorLabels = c("MTX", "MTX"),
                  databaseIds = c("CCAE", "CCAE"),
                  analysisIds = c(9, 9),
                  outcomeIds = c(253, 203) ,
                  outcomeNames = c("All infections", "Serious infections"))
    write.csv(nnt, file.path(reportFolder, "numberNeededToTreat.csv"), row.names = FALSE)
  }
  
  # analysis <- "primary"
  if (analysis == "primary") { # stratafied, on-treatment
    analysisIds <- c(7, 7, 1, 3)
    analysisName <- "primary analysis"
  }
  if (analysis == "matchedOnTreatment") {
    analysisIds <- c(9, 9, 4)
    analysisName <- "PS matched on-treatment"
  }
  if (analysis == "strataItt") {
    analysisIds <- c(8, 8, 2)
    analysisName <- "PS stratified intent-to-treat"
  }
  if (analysis == "matchedItt") {
    analysisIds <- c(10, 10, 5, 6)
    analysisName <- "PS matched intent-to-treat"
  }
  
  library(magrittr)
  
  if (!file.exists(reportFolder)) {
    dir.create(reportFolder)
  }
  
  source("extras/getResults/global.R")
  source("extras/getResults/dataClean.R")
  source("extras/getResults/PlotsAndTables.R")
  databaseIds <- c("AmbEMR", "CCAE", "DAGermany", "MDCR", "Optum", "PanTher", "SIDIAP", "THIN", "Meta-analysis")
  
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 219] <- "MTX"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 224] <- "HCQ"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 225] <- "SSZ"
  exposureOfInterest$shortName[exposureOfInterest$exposureId == 226] <- "LEF"
  
  # chars table ----------------------------------------------------------------
  if (createCharsTables) { 
    charsFolder <- file.path(reportFolder, "chars")
    if (!file.exists(charsFolder)) {
      dir.create(charsFolder, recursive = TRUE)
    }
    
    headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
    heading1 <- "Baseline characteristics after PS stratification"
    heading1 <- officer::fpar(officer::ftext(heading1, prop = headingFormat))
    
    titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
    comparisonSummary <- comparisonSummary[comparisonSummary$databaseId %in% databaseIds, ] 
    
    charTitles <- list()
    dbTable1s <- list()
    for (databaseId in unique(comparisonSummary$databaseId)) { # databaseId = "CCAE"
      
      table1s <- list()
      
      databaseSummary <- comparisonSummary[comparisonSummary$databaseId == databaseId, ]
      for (i in 1:nrow(databaseSummary)) { # i = 1
        targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == databaseSummary$targetId[i]]
        comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == databaseSummary$comparatorId[i]]
      
        balance <- getCovariateBalance(resultsFolder = dataFolder,
                                       targetId = databaseSummary$targetId[i],
                                       comparatorId = databaseSummary$comparatorId[i],
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
                                pathToCsv = "inst/shiny/EhdenRaDmardsEstimation/Table1Specs.csv")
        facs <- sapply(table1, is.factor)
        table1[facs] <- lapply(table1[facs], as.character)
        rm(facs)
        table1 <- table1[, c(1,5,6,7)]
        names(table1) <- c("characteristic", "t", "c", "stddiff")
        table1s[[length(table1s) + 1]] <- table1
      }
      
      dbTable1 <- merge(table1s[[1]], table1s[[2]], by = "characteristic", all = TRUE)
      dbTable1 <- merge(dbTable1, table1s[[3]], by = "characteristic", all = TRUE)
      charOrder <- table1s[[1]]$characteristic # 
      dbTable1$order <- match(dbTable1$characteristic, charOrder)
      dbTable1 <- dbTable1[order(dbTable1$order), ]
      dbTable1$order <- NULL
      
      dbTable1 <- flextable::qflextable(dbTable1)
      dbTable1 <- flextable::delete_part(dbTable1, part = "header")
      dbTable1 <- flextable::fontsize(dbTable1, part = "all", size = 7) 
      dbTable1 <- flextable::align(dbTable1, j = 1, align = "left", part = "all")
      dbTable1 <- flextable::autofit(dbTable1, add_w = 0, add_h = 0)
      
      title <- databaseId
      title <- officer::fpar(officer::ftext(title, prop = titleFormat))
      charTitles[[length(charTitles) + 1]] <- title      
      dbTable1s[[length(dbTable1s) + 1]] <- dbTable1
    }
    
    doc <- officer::read_docx() %>%
      officer::body_add_fpar(heading1, style = "heading 1") %>%  # pancytopenia table1s
      officer::body_add_fpar(charTitles[[1]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[1]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[2]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[2]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[3]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[3]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[4]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[4]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[5]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[5]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[6]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[6]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[7]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[7]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(charTitles[[8]], style = "heading 2") %>%
      flextable::body_add_flextable(dbTable1s[[8]]) %>%
    print(target = file.path(charsFolder, "charsAfter.docx"))
  }
  
  # events table ---------------------------------------------------------------
  if (createEventsTables) {
    eventsFolder <- file.path(reportFolder, "events")
    if (!file.exists(eventsFolder)) {
      dir.create(eventsFolder, recursive = TRUE)
    }
    
    headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
    heading2 <- "Event counts and incidence rates after PS stratification"
    heading2 <- officer::fpar(officer::ftext(heading2, prop = headingFormat))

    # TCs
    targetIds <- c(224, 225, 226)
    targetNames <- c("HCQ", "SSZ", "LEF")
    comparatorIds <- c(219, 219, 219)
    comparatorNames <- c("MTX", "MTX", "MTX")
    
    # leuko/pancyto outcomes/analyses
    outcomeNames <- c("Leukopenia", "Pancytopenia")
    outcomeIds <- c(187, 193)
    analysisNames <- analysisName
    ref1 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[1],
                      analysisNames)
    
    # infection outcomes/analyses
    outcomeNames <- c("Opportunistic infection", "Serious infection", "All infections")
    outcomeIds <- c(197, 203, 253)
    analysisNames <- analysisName
    ref2 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[2],
                      analysisNames)
    
    # CV outcomes/analyses
    outcomeNames <- c("MI", "Stroke") # any visit
    outcomeIds <- c(185, 183)
    analysisNames <- analysisName
    ref3 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[3],
                      analysisNames)
    ref3 <- ref3[order(ref3$analysisId, rev(ref3$outcomeId), ref3$targetId), ]
    
    # cancer outcome/analyses
    if (analysis %in% c("primary", "matchedItt")) {
      outcomeNames <- c("All cancers", "Leukemia", "Lymphoma", "Colorectal cancer", "Lung cancer")
      outcomeIds <- c(223, 212, 216, 218, 201)
      analysisNames <- analysisName
      ref4 <- createRef(targetIds,
                        targetNames,
                        comparatorIds,
                        comparatorNames,
                        outcomeNames,
                        outcomeIds,
                        analysisIds[4],
                        analysisNames)
      ref <- rbind(ref1, ref2, ref3, ref4)
    } else {
      ref <- rbind(ref1, ref2, ref3)
    }

    eventsTitles <- list()
    eventTables <- list()
    titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
    
    for (targetId in unique(ref$targetId)) { # targetId = 224
      targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == targetId]
      eventsTitle <- sprintf("%s vs. MTX, %s", targetLabel, analysisName)
      eventsTitle <- officer::fpar(officer::ftext(eventsTitle, prop = titleFormat))
      eventsTitles[[length(eventsTitles) + 1]] <- eventsTitle
      exposureRef <- ref[ref$targetId == targetId, ]
      eventTable <- createEventTable(exposureRef, databaseIds, exposureOfInterest)
      eventTable <- eventTable[, c("outcomeName", "databaseId", 
                                   "targetSubjects", "comparatorSubjects", "targetYears", "comparatorYears",
                                   "targetOutcomes", "comparatorOutcomes", "targetIr", "comparatorIr")]
      header1 <- c("Outcome", "Source", rep(c("T", "C"), 4))
      eventTable <- rbind(header1, eventTable)
      eventTable <- flextable::qflextable(eventTable)
      eventTable <- flextable::fontsize(eventTable, part = "all", size = 8) 
      eventTable <- flextable::set_header_labels(eventTable, 
                                                 outcomeName = "",
                                                 databaseId = "",
                                                 targetSubjects = "Patients",
                                                 comparatorSubjects = "",
                                                 targetYears = "Person-years",
                                                 comparatorYears = "",
                                                 targetOutcomes = "Events", 
                                                 comparatorOutcomes = "",
                                                 targetIr = "Incidence rate",
                                                 comparatorIr = "")
      eventTable <- flextable::align(eventTable, j = 1:2, align = "left", part = "all")
      eventTable <- flextable::bold(eventTable, i = 1, bold = TRUE, part = "body")
      eventTable <- flextable::merge_v(eventTable, j = 1, part = "body")
      std_border <- officer::fp_border()
      eventTable <- flextable::hline(eventTable, i = 1, border = std_border, part = "body")
      eventTable <- flextable::autofit(eventTable, add_w = 0, add_h = 0)
      eventTables[[length(eventTables) + 1]] <- eventTable
    }
    
    doc <- officer::read_docx() %>%
      officer::body_add_fpar(heading2, style = "heading 1") %>%
      officer::body_add_fpar(eventsTitles[[1]], style = "heading 2") %>%
      flextable::body_add_flextable(eventTables[[1]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(eventsTitles[[2]], style = "heading 2") %>%
      flextable::body_add_flextable(eventTables[[2]]) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(eventsTitles[[3]], style = "heading 2") %>%
      flextable::body_add_flextable(eventTables[[3]]) %>%
      print(target = file.path(eventsFolder, sprintf("eventsTable %s.docx", analysisName)))
  }
  
  # hr plots -------------------------------------------------------------------
  
  if (createForestPlots) {
    hrsFolder <- file.path(reportFolder, "HRs")
    if (!file.exists(hrsFolder)) {
      dir.create(hrsFolder, recursive = TRUE)
    }
    
    headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
    heading3 <- "Calibrated hazard ratio forest plots"
    heading3 <- officer::fpar(officer::ftext(heading3, prop = headingFormat))
    
    # TCs
    targetIds <- c(224, 225, 226)
    targetNames <- c("HCQ", "SSZ", "LEF")
    comparatorIds <- c(219, 219, 219)
    comparatorNames <- c("MTX", "MTX", "MTX")
    
    # leuko/pancyto outcomes/analyses
    outcomeNames <- c("Leukopenia", "Pancytopenia")
    outcomeIds <- c(187, 193)
    analysisNames <- analysisName
    ref1 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[1],
                      analysisNames)
    
    # infection outcomes/analyses
    outcomeNames <- c("Opportunistic infection", "Serious infection", "All infections")
    outcomeIds <- c(197, 203, 253)
    analysisNames <- analysisName
    ref2 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[2],
                      analysisNames)
    
    # CV outcomes/analyses
    outcomeNames <- c("MI", "Stroke") # any visit
    outcomeIds <- c(185, 183)
    analysisNames <- analysisName
    ref3 <- createRef(targetIds,
                      targetNames,
                      comparatorIds,
                      comparatorNames,
                      outcomeNames,
                      outcomeIds,
                      analysisIds[3],
                      analysisNames)
    ref3 <- ref3[order(ref3$analysisId, rev(ref3$outcomeId), ref3$targetId), ]
    
    # cancer outcome/analyses
    if (analysis %in% c("primary", "matchedItt")) {
      outcomeNames <- c("All cancers", "Leukemia", "Lymphoma", "Colorectal cancer", "Lung cancer")
      outcomeIds <- c(223, 212, 216, 218, 201)
      analysisNames <- analysisName
      ref4 <- createRef(targetIds,
                        targetNames,
                        comparatorIds,
                        comparatorNames,
                        outcomeNames,
                        outcomeIds,
                        analysisIds[4],
                        analysisNames)
      ref <- rbind(ref1, ref2, ref3, ref4)
    } else {
      ref <- rbind(ref1, ref2, ref3)
    }
    
    hrTitles <- list()
    hrFileNames <- list()
    titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
    
    for (targetId in unique(ref$targetId)) { # targetId = 226
      targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == targetId]
      hrTitle <- sprintf("%s vs. MTX, %s", targetLabel, analysisName)
      hrTitle <- officer::fpar(officer::ftext(hrTitle, prop = titleFormat))
      hrTitles[[length(hrTitles) + 1]] <- hrTitle
      exposureRef <- ref[ref$targetId == targetId, ]
      hrPlot <- createPlotsLong(exposureRef, databaseIds)
      hrFileName <- file.path(hrsFolder, sprintf("HRs %s vs MTX %s.png", targetLabel, analysisName))
      ggplot2::ggsave(hrFileName, hrPlot, width = 12, height = 12, dpi = 400)
      hrFileNames[[length(hrFileNames) + 1]] <- hrFileName
    }
    
    doc <- officer::read_docx() %>%
      officer::body_add_fpar(heading3, style = "heading 1") %>%
      officer::body_add_fpar(hrTitles[[1]], style = "heading 2") %>%
      officer::body_add_img(hrFileNames[[1]], width = 6, height = 7) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(hrTitles[[2]], style = "heading 2") %>%
      officer::body_add_img(hrFileNames[[2]], width = 6, height = 7) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(hrTitles[[3]], style = "heading 2") %>%
      officer::body_add_img(hrFileNames[[3]], width = 6, height = 7) %>%
      print(target = file.path(hrsFolder, sprintf("hrPlots %s.docx", analysisName)))
  }
  # diagnostic plots -----------------------------------------------------------
  if (createDiagnosticPlots) {
    diagnosticsFolder <- file.path(reportFolder, "diagnostics")
    if (!file.exists(diagnosticsFolder)) {
      dir.create(diagnosticsFolder, recursive = TRUE)
    }
    
    headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
    titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
    diagTitles <- list()
    diagFileNames <- list()
    
    # TCs
    targetIds <- c(224, 225, 226)
    targetNames <- c("HCQ", "SSZ", "LEF")
    comparatorIds <- c(219, 219, 219)
    comparatorNames <- c("MTX", "MTX", "MTX")
    
    # leuko/pancyto outcomes/analyses
    heading10 <- "Evidence evaluation diagnostics, no pancytopenia in last 30 days"
    heading10 <- officer::fpar(officer::ftext(heading10, prop = headingFormat))
    outcomeNames <- c("Leukopenia", "Pancytopenia")
    outcomeIds <- c(187, 193)
    analysisIds <- c(7, 8)
    analysisNames <- c("PS stratified, on-treatment", "PS stratified, intent-to-treat")
    ref <- createRef(targetIds,
                     targetNames,
                     comparatorIds,
                     comparatorNames,
                     outcomeNames,
                     outcomeIds,
                     analysisIds,
                     analysisNames)
    ref <- merge(ref, databaseIds[-9])
    names(ref)[names(ref) == "y"] <- "databaseId"
    ref <- ref[-c(1,2)]
    ref <- unique(ref)
    
    # only create diagnostics for pancytopenia (no event last 30d)
    ref <- ref[ref$outcomeId == 193, ]
    analyses <- data.frame(analysisId = analysisIds,
                           analysisName = analysisNames)
    
    for (databaseId in unique(ref$databaseId)) { # databaseId = "Amb_EMR"
      for (outcomeId in unique(ref$outcomeId)) { # outcomeId = 187
        outcomeName <- unique(ref$outcomeName[ref$outcomeId == outcomeId])
        fileName <- file.path(diagnosticsFolder, paste0("Diagnostics ", outcomeName, " in ", databaseId, ".png"))
        subRef <- ref[ref$databaseId == databaseId & ref$outcomeId == outcomeId, ]
        diagPlots <- list()
        for (j in 1:nrow(subRef)) { # j = 1
          targetId <- subRef$targetId[j]
          comparatorId <- subRef$comparatorId[j]
          defaultAnalysisId <- 7
          targetName <- subRef$targetName[j]
          comparatorName <- subRef$comparatorName[j]
          title <- sprintf("%s", databaseId)
          if (!file.exists(fileName)) {
            ps <- getPs(connection, targetId, comparatorId, defaultAnalysisId, databaseId)
            psPlot <- plotPs(ps, targetName, comparatorName)
            diagPlots[[length(diagPlots) + 1]] <- psPlot
            bal <- getCovariateBalance(connection, dataFolder, targetId, comparatorId, databaseId, defaultAnalysisId, outcomeId)
            balPlot <- plotCovariateBalanceScatterPlot(balance = bal)
            diagPlots[[length(diagPlots) + 1]] <- balPlot
            for (k in 1:nrow(analyses)) { # k = 1
              analysisId <- analyses$analysisId[k]
              analysisName <- analyses$analysisName[k]
              controlResults <- getControlResults(connection, targetId, comparatorId, analysisId, databaseId)
              nullPlot <-  plotLargeScatter(d = controlResults, xLabel = "Hazard ratio")
              diagPlots[[length(diagPlots) + 1]] <- nullPlot
            }
          }
        }
        if (!file.exists(fileName)) {
          row0 <- grid::textGrob("")
          row1 <- grid::textGrob("HCQ", rot = 90, gp = grid::gpar(fontsize = 18))
          row2 <- grid::textGrob("SSZ", rot = 90, gp = grid::gpar(fontsize = 18))
          row3 <- grid::textGrob("LEF", rot = 90, gp = grid::gpar(fontsize = 18))
          col1 <- grid::textGrob("PS distribution", gp = grid::gpar(fontsize = 18))
          col2 <- grid::textGrob("Covariate balance", gp = grid::gpar(fontsize = 18))
          col3 <- grid::textGrob("On-treatment", gp = grid::gpar(fontsize = 18))
          col4 <- grid::textGrob("Intent-to-treat", gp = grid::gpar(fontsize = 18))
          plotGrid <- gridExtra::grid.arrange(row0, col1, col2, col3, col4,
                                              row1, diagPlots[[1]], diagPlots[[2]], diagPlots[[3]], diagPlots[[4]],
                                              row2, diagPlots[[5]], diagPlots[[6]], diagPlots[[7]], diagPlots[[8]],
                                              row3, diagPlots[[9]], diagPlots[[10]], diagPlots[[11]], diagPlots[[12]],
                                              nrow = 4,
                                              heights = c(1, 4, 4, 4),
                                              widths = c(0.25, 3, 3, 3, 3))
          ggplot2::ggsave(fileName, plotGrid, width = 14, height = 7, dpi = 400)
        }
        titleFormat <- officer::fp_text(font.size = 12, bold = FALSE, font.family = "Calibri")
        title <- officer::fpar(officer::ftext(title, prop = titleFormat))
        diagTitles[[length(diagTitles) + 1]] <- title
        diagFileNames[[length(diagFileNames) + 1]] <- fileName
      }
    }
    doc <- officer::read_docx() %>%
      # landscape start
      officer::body_end_section_continuous() %>%
      officer::body_add_fpar(heading10, style = "heading 1") %>%  # pancytopenia diagnostics, no event last 30d
      officer::body_add_fpar(diagTitles[[1]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[1]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[2]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[2]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[3]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[3]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[4]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[4]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[5]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[5]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[6]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[6]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[7]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[7]], width = 10, height = 5) %>%
      officer::body_add_break() %>%
      officer::body_add_fpar(diagTitles[[8]], style = "heading 2") %>%
      officer::body_add_img(diagFileNames[[8]], width = 10, height = 5) %>%
      officer::body_end_section_landscape() %>%
      #landscape end
      print(target = file.path(diagnosticsFolder, "diagnosticsPlots.docx"))
  }
}