library(EhdenRaDmardsEstimation)

options(fftempdir = "S:/FFTemp")
maxCores <- parallel::detectCores()
studyFolder <- "G:/StudyResults/EhdenRaDmardsEstimation2"  # note 2


source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))

mailSettings <- list(from = Sys.getenv("emailAddress"),
                     to = c(Sys.getenv("emailAddress")),
                     smtp = list(host.name = Sys.getenv("emailHost"), port = 25,
                                 user.name = Sys.getenv("emailAddress"),
                                 passwd = Sys.getenv("emailPassword"), ssl = FALSE),
                     authenticate = FALSE,
                     send = TRUE)

# CCAE settings ----------------------------------------------------------------
databaseId <- "CCAE"
databaseName <- "CCAE"
databaseDescription <- "CCAE"
cdmDatabaseSchema <- "CDM_IBM_CCAE_V1061.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema = "scratch.dbo"
cohortTable = "bcn_ccae"

# Optum DOD settings -----------------------------------------------------------
databaseId <- "Optum"
databaseName <- "Optum"
databaseDescription <- "Optum DOD"
cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V1064.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_optum"

# CPRD settings ----------------------------------------------------------------
databaseId <- "CPRD"
databaseName <- "CPRD"
databaseDescription <- "CPRD"
cdmDatabaseSchema = "CDM_CPRD_V1017.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_cprd"

# MDCD settings ----------------------------------------------------------------
databaseId <- "MDCD"
databaseName <- "MDCD"
databaseDescription <- "MDCD"
cdmDatabaseSchema = "CDM_IBM_MDCD_V1023.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_mdcd"

# MDCR settings ----------------------------------------------------------------
databaseId <- "MDCR"
databaseName <- "MDCR"
databaseDescription <- "MDCR"
cdmDatabaseSchema = "CDM_IBM_MDCR_V1062.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_mdcr"

# JMDC -------------------------------------------------------------------------
databaseId <- "JMDC"
databaseName <- "JMDC"
databaseDescription <- "JMDC"
cdmDatabaseSchema = "CDM_JMDC_V1063.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_jmdc"

# Germany DA IQVIA--------------------------------------------------------------
databaseId <- "GERMANY"
databaseName <- "ResultsGERMANY"
databaseDescription <- "GERMANY DA IQVIA"
cdmDatabaseSchema = ""
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- ""
cohortTable <- ""

# France DA IQVIA--------------------------------------------------------------
databaseId <- "LPDFRANCE"
databaseName <- "ResultsLPDFRANCE"
databaseDescription <- "LPDFRANCE"
cdmDatabaseSchema = "LPDFRANCE"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- ""
cohortTable <- ""

# Belgium DA IQVIA--------------------------------------------------------------
databaseId <- "BELGIUM"
databaseName <- "ResultsBELGIUM"
databaseDescription <- "BELGIUM"
cdmDatabaseSchema <- ""
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- ""
cohortTable <- ""

# PanTher ----------------------------------------------------------------------
databaseId <- "PanTher"
databaseName <- "PanTher"
databaseDescription <- "PanTher"
cdmDatabaseSchema = "CDM_OPTUM_PANTHER_V1020.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_panther"

# Estonian HIS-------------------------------------------------------------------
databaseId <- "EstonianHIS"
databaseName <- "EstonianHIS"
databaseDescription <- "EstonianHIS"
cdmDatabaseSchema = ""
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- ""
cohortTable <- ""

# THIN --------------------------------------------------------------------------
databaseId <- "THIN"
databaseName <- "ResultsTHIN"
databaseDescription <- "THIN"
cdmDatabaseSchema = ""
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- ""
cohortTable <- ""

# SIDIAP -----------------------------------------------------------------------
databaseId <- "SIDIAP"
databaseName <- "ResultsSIDIAP"
databaseDescription <- "SIDIAP"
cdmDatabaseSchema = ""
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- ""
cohortTable <- ""

# AMBEMR -----------------------------------------------------------------------
databaseId <- "Amb_EMR"
databaseName <- "Ambulatory EMR"
databaseDescription <- "Ambulatory EMR"
cdmDatabaseSchema = ""
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- ""
cohortTable <- ""

# Run --------------------------------------------------------------------------
OhdsiRTools::runAndNotify(expression = {
        execute(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortTable = cohortTable,
                oracleTempSchema = NULL,
                outputFolder = outputFolder,
                databaseId = databaseId,
                databaseName = databaseName,
                databaseDescription = databaseDescription,
                createCohorts = FALSE,
                synthesizePositiveControls = FALSE,
                runAnalyses = FALSE,
                runDiagnostics = FALSE,
                packageResults = FALSE,
                maxCores = maxCores)
}, mailSettings = mailSettings, label = paste0("EhdenRaDmardsEstimation ", databaseId), stopOnWarning = FALSE)

resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)

# move database-specific shiny files to shinyDataAll folder

assessCovarBalanaceAndPrevalence(shinyDataFolder = file.path(studyFolder, "ShinyDataAll"),
                                 studyFolder = studyFolder)

doMetaAnalysis(studyFolder = studyFolder,
               outputFolders = c(file.path(studyFolder, "Amb_EMR"),
                                 file.path(studyFolder, "CCAE"),
                                 file.path(studyFolder, "GERMANY"),
                                 file.path(studyFolder, "MDCD"),
                                 file.path(studyFolder, "MDCR"),
                                 file.path(studyFolder, "Optum"),
                                 file.path(studyFolder, "PanTher"),
                                 file.path(studyFolder, "THIN"),
                                 file.path(studyFolder, "SIDIAP"),
                                 file.path(studyFolder, "BELGIUM"),
                                 file.path(studyFolder, "EstonianHIS"),
                                 file.path(studyFolder, "JMDC"),
                                 file.path(studyFolder, "LPDFRANCE"),
                                 file.path(studyFolder, "IPCI-HI-LARIOUS-RA")), 
               maOutputFolder = file.path(studyFolder, "MetaAnalysis"),
               maxCores = maxCores)

# move meta-analysis shiny files to shinyDataAll folder

premergeShinyDataFiles(dataFolder = file.path(studyFolder, "ShinyDataAll"),
                       newDataFolder = file.path(studyFolder, "NewShinyDataAll"))

createPlotsAndTables(resultsFolder = file.path(studyFolder, "NewShinyDataAll"),
                     reportFolder = file.path(studyFolder, "reportTest"),
                     createCountsTable = TRUE,
                     createCharsTable = TRUE,
                     createEventsTables = TRUE,
                     createForestPlots = TRUE)


