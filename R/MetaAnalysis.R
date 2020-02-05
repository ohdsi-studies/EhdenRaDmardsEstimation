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
doMetaAnalysis <- function(studyFolder,
                           outputFolders,
                           maOutputFolder,
                           maxCores) {
  
  OhdsiRTools::logInfo("Performing meta-analysis")
  shinyDataFolder <- file.path(maOutputFolder, "shinyData")
  if (!file.exists(shinyDataFolder))
    dir.create(shinyDataFolder, recursive = TRUE)
  
  loadResults <- function(outputFolder) {
    files <- list.files(file.path(outputFolder, "export"), pattern = "cohort_method_result.csv", full.names = TRUE)
    OhdsiRTools::logInfo("Loading ", files[1], " for meta-analysis")
    return(read.csv(files[1], stringsAsFactors = FALSE))
  }
  allResults <- lapply(outputFolders, loadResults)
  allResults <- do.call(rbind, allResults)
  
  allControls <- lapply(outputFolders, getAllControls)
  allControls <- do.call(rbind, allControls)
  allControls <- allControls[, c("targetId", "comparatorId", "outcomeId", "targetEffectSize")]
  allControls <- allControls[!duplicated(allControls), ]
  
  # drop unused combinations
  keeps <- ((allResults$outcome_id %in% c(187, 193, 197, 203, 253) & allResults$analysis_id %in% c(7:10)) |  # infections, leukopenia, pancytopenia
    (allResults$outcome_id %in% c(182:185) & allResults$analysis_id %in% c(1, 2, 4, 5)) | # cardiovacular
    (allResults$outcome_id %in% c(212, 223, 216, 218, 201, 206) & allResults$analysis_id %in% c(3, 6)) | # oncology
    allResults$outcome_id %in%  unique(allControls$outcomeId)) & # control outcomes
    allResults$comparator_id == 219 # keep only methotrexate as comparator
  allResults <- allResults[keeps, ]
  
  # blind estimates that don't pass covariate balance rule
  toBlind <- read.csv(file.path(studyFolder, "toBlind.csv"))
  toBlind <- toBlind[, -c(4, 6)]
  toBlind$to_blind <- 1
  
  allResults <- merge(allResults, toBlind, all.x = TRUE)
  allResults$to_blind[is.na(allResults$to_blind)] <- 0
  balBlinds <- allResults$to_blind == 1
  allResults$to_blind <- NULL
  
  # blind databases with bad on-treatment tar data
  dbBlinds <- allResults$database_id %in% c("BELGIUM", "GERMANY", "THIN", "PanTher", "IPCI-HI-LARIOUS-RA") & allResults$analysis_id %in% c(1,4,7,9)
  
  allResults$rr[balBlinds | dbBlinds] <- NA
  allResults$ci_95_lb[balBlinds | dbBlinds] <- NA
  allResults$ci_95_ub[balBlinds | dbBlinds] <- NA
  allResults$log_rr[balBlinds | dbBlinds] <- NA
  allResults$se_log_rr[balBlinds | dbBlinds] <- NA
  allResults$p[balBlinds | dbBlinds] <- NA
  allResults$calibrated_rr[balBlinds | dbBlinds] <- NA
  allResults$calibrated_ci_95_lb[balBlinds | dbBlinds] <- NA
  allResults$calibrated_ci_95_ub[balBlinds | dbBlinds] <- NA
  allResults$calibrated_log_rr[balBlinds | dbBlinds] <- NA
  allResults$calibrated_se_log_rr[balBlinds | dbBlinds] <- NA
  allResults$calibrated_p[balBlinds | dbBlinds] <- NA
  
  ncIds <- allControls$outcomeId[allControls$targetEffectSize == 1]
  allResults$type[allResults$outcome_id %in% ncIds] <- "Negative control"
  pcIds <- allControls$outcomeId[allControls$targetEffectSize != 1]
  allResults$type[allResults$outcome_id %in% pcIds] <- "Positive control"
  allResults$type[is.na(allResults$type)] <- "Outcome of interest"
  
  groups <- split(allResults, paste(allResults$target_id, allResults$comparator_id, allResults$analysis_id), drop = TRUE)
  cluster <- ParallelLogger::makeCluster(min(maxCores, 12))
  results <- ParallelLogger::clusterApply(cluster, 
                                          groups,
                                          computeGroupMetaAnalysis,
                                          shinyDataFolder = shinyDataFolder,
                                          allControls = allControls)
  ParallelLogger::stopCluster(cluster)
  results <- do.call(rbind, results)
  
  fileName <- file.path(maOutputFolder, "cohort_method_results_Meta-analysis.csv")
  write.csv(results, fileName, row.names = FALSE, na = "")
  fileName <- file.path(shinyDataFolder, "cohort_method_result_Meta-analysis.rds")
  results <- subset(results, select = -c(type, mdrr))
  saveRDS(results, fileName)
  
  database <- data.frame(database_id = "Meta-analysis",
                         database_name = "Meta-analysis",
                         description = "Meta-analysis",
                         is_meta_analysis = 1)
  fileName <- file.path(shinyDataFolder, "database_Meta-analysis.rds")
  saveRDS(database, fileName)
}

computeGroupMetaAnalysis <- function(group,
                                     shinyDataFolder,
                                     allControls) {
  
  # group <- groups[["224 219 1"]]
  analysisId <- group$analysis_id[1]
  targetId <- group$target_id[1]
  comparatorId <- group$comparator_id[1]
  OhdsiRTools::logTrace("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis", analysisId)
  outcomeGroups <- split(group, group$outcome_id, drop = TRUE)
  outcomeGroupResults <- lapply(outcomeGroups, computeSingleMetaAnalysis)
  groupResults <- do.call(rbind, outcomeGroupResults)
  
  
  ncs <- groupResults[groupResults$type == "Negative control", ]
  ncs <- ncs[!is.na(ncs$se_log_rr), ]
  if (nrow(ncs) > 5) {
    null <- EmpiricalCalibration::fitMcmcNull(ncs$log_rr, ncs$se_log_rr) # calibrate CIs without synthesizing positive controls, assumes error consistent across effect sizes
    model <- EmpiricalCalibration::convertNullToErrorModel(null)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = groupResults$log_rr,
                                                    seLogRr = groupResults$se_log_rr)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$log_rr,
                                                                      seLogRr = groupResults$se_log_rr,
                                                                      model = model)
    groupResults$calibrated_p <- calibratedP$p
    groupResults$calibrated_rr <- exp(calibratedCi$logRr)
    groupResults$calibrated_ci_95_lb <- exp(calibratedCi$logLb95Rr)
    groupResults$calibrated_ci_95_ub <- exp(calibratedCi$logUb95Rr)
    groupResults$calibrated_log_rr <- calibratedCi$logRr
    groupResults$calibrated_se_log_rr <- calibratedCi$seLogRr
  } else {
    groupResults$calibrated_p <- rep(NA, nrow(groupResults))
    groupResults$calibrated_rr <- rep(NA, nrow(groupResults))
    groupResults$calibrated_ci_95_lb <- rep(NA, nrow(groupResults))
    groupResults$calibrated_ci_95_ub <- rep(NA, nrow(groupResults))
    groupResults$calibrated_log_rr <- rep(NA, nrow(groupResults))
    groupResults$calibrated_se_log_rr <- rep(NA, nrow(groupResults))
  }
  pcs <- groupResults[groupResults$type == "Positive control", ]
  pcs <- pcs[!is.na(pcs$se_log_rr), ]
  if (nrow(pcs) > 5) {
    controls <- merge(groupResults, 
                      allControls, 
                      by.x = c("target_id", "comparator_id", "outcome_id"),
                      by.y = c("targetId", "comparatorId", "outcomeId"))
    model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = controls$log_rr,
                                                           seLogRr = controls$se_log_rr,
                                                           trueLogRr = log(controls$targetEffectSize),
                                                           estimateCovarianceMatrix = FALSE)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$log_rr,
                                                                      seLogRr = groupResults$se_log_rr,
                                                                      model = model)
    groupResults$calibrated_rr <- exp(calibratedCi$logRr)
    groupResults$calibrated_ci_95_lb <- exp(calibratedCi$logLb95Rr)
    groupResults$calibrated_ci_95_ub <- exp(calibratedCi$logUb95Rr)
    groupResults$calibrated_log_rr <- calibratedCi$logRr
    groupResults$calibrated_se_log_rr <- calibratedCi$seLogRr
  } 
  return(groupResults)
}

computeSingleMetaAnalysis <- function(outcomeGroup) {
  # outcomeGroup <- outcomeGroups[[2]]
  maRow <- outcomeGroup[1, ]
  outcomeGroup <- outcomeGroup[!is.na(outcomeGroup$se_log_rr), ] # drops rows with zero counts
  
  if (nrow(outcomeGroup) == 0) {
    maRow$target_subjects <- 0
    maRow$comparator_subjects <- 0
    maRow$target_days <- 0
    maRow$comparator_days <- 0
    maRow$target_outcomes <- 0
    maRow$comparator_outcomes <- 0
    maRow$rr <- NA
    maRow$ci_95_lb <- NA
    maRow$ci_95_ub <- NA
    maRow$p <- NA
    maRow$log_rr <- NA
    maRow$se_log_rr <- NA
    maRow$i_2 <- NA
  } else if (nrow(outcomeGroup) == 1) {
    maRow <- outcomeGroup[1, ]
    maRow$i_2 <- 0
  } else {
    maRow$target_subjects <- sumMinCellCount(outcomeGroup$target_subjects)
    maRow$comparator_subjects <- sumMinCellCount(outcomeGroup$comparator_subjects)
    maRow$target_days <- sum(outcomeGroup$target_days)
    maRow$comparator_days <- sum(outcomeGroup$comparator_days)
    maRow$target_outcomes <- sumMinCellCount(outcomeGroup$target_outcomes)
    maRow$comparator_outcomes <- sumMinCellCount(outcomeGroup$comparator_outcomes)
    meta <- meta::metagen(outcomeGroup$log_rr, outcomeGroup$se_log_rr, sm = "RR", hakn = FALSE)
    s <- summary(meta)
    maRow$i_2 <- s$I2$TE
    if (maRow$i_2 < .40) {
      rnd <- s$random
      maRow$rr <- exp(rnd$TE)
      maRow$ci_95_lb <- exp(rnd$lower)
      maRow$ci_95_ub <- exp(rnd$upper)
      maRow$p <- rnd$p
      maRow$log_rr <- rnd$TE
      maRow$se_log_rr <- rnd$seTE
    } else {
      maRow$rr <- NA
      maRow$ci_95_lb <- NA
      maRow$ci_95_ub <- NA
      maRow$p <- NA
      maRow$log_rr <- NA
      maRow$se_log_rr <- NA
    }
  }
  if (is.na(maRow$log_rr)) {
    maRow$mdrr <- NA
  } else {
    alpha <- 0.05
    power <- 0.8
    z1MinAlpha <- qnorm(1 - alpha/2)
    zBeta <- -qnorm(1 - power)
    pA <- maRow$target_subjects / (maRow$target_subjects + maRow$comparator_subjects)
    pB <- 1 - pA
    totalEvents <- abs(maRow$target_outcomes) + abs(maRow$comparator_outcomes)
    maRow$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  }
  maRow$database_id <- "Meta-analysis"
  maRow$sources <- paste(outcomeGroup$database_id[order(outcomeGroup$database_id)], collapse = ", ")
  return(maRow)
}

sumMinCellCount <- function(counts) {
  total <- sum(abs(counts))
  if (any(counts < 0)) {
    total <- -total
  }
  return(total)
}


getAllControls <- function(outputFolder) {
  allControlsFile <- file.path(outputFolder, "AllControls.csv")
  if (file.exists(allControlsFile)) {
    # Positive controls must have been synthesized. Include both positive and negative controls.
    allControls <- read.csv(allControlsFile)
  } else {
    # Include only negative controls
    pathToCsv <- system.file("settings", "NegativeControls.csv", package = "EhdenRaDmardsEstimation")
    allControls <- read.csv(pathToCsv)
    allControls$oldOutcomeId <- allControls$outcomeId
    allControls$targetEffectSize <- rep(1, nrow(allControls))
  }
  return(allControls)
}
