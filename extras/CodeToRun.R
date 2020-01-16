library(EhdenRaDmardsEstimation)

options(fftempdir = "E:/FFTemp")
maxCores <- parallel::detectCores()
studyFolder <- "E:/My Projects/DMARDsEstimation"

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

# # CCAE settings ----------------------------------------------------------------
# databaseId <- "CCAE"
# databaseName <- "CCAE"
# databaseDescription <- "CCAE"
# cdmDatabaseSchema <- "CDM_IBM_CCAE_V1061.dbo"
# outputFolder <- file.path(studyFolder, databaseId)
# cohortDatabaseSchema = "scratch.dbo"
# cohortTable = "bcn_ccae"
# 
# # Optum DOD settings -----------------------------------------------------------
# databaseId <- "Optum"
# databaseName <- "Optum"
# databaseDescription <- "Optum DOD"
# cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V1064.dbo"
# outputFolder <- file.path(studyFolder, databaseId)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_optum"
# 
# # CPRD settings ----------------------------------------------------------------
# databaseId <- "CPRD"
# databaseName <- "CPRD"
# databaseDescription <- "CPRD"
# cdmDatabaseSchema = "CDM_CPRD_V1017.dbo"
# outputFolder <- file.path(studyFolder, databaseId)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_cprd"
# 
# # MDCD settings ----------------------------------------------------------------
# databaseId <- "MDCD"
# databaseName <- "MDCD"
# databaseDescription <- "MDCD"
# cdmDatabaseSchema = "CDM_IBM_MDCD_V1023.dbo"
# outputFolder <- file.path(studyFolder, databaseId)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_mdcd"
# 
# # MDCR settings ----------------------------------------------------------------
# databaseId <- "MDCR"
# databaseName <- "MDCR"
# databaseDescription <- "MDCR"
# cdmDatabaseSchema = "CDM_IBM_MDCR_V1062.dbo"
# outputFolder <- file.path(studyFolder, databaseName)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_mdcr"
# 
# JMDC -------------------------------------------------------------------------
databaseId <- "JMDC"
databaseName <- "JMDC"
databaseDescription <- "JMDC"
cdmDatabaseSchema = "CDM_JMDC_V1063.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "bcn_jmdc"
# 
# # GermanyDA --------------------------------------------------------------------
# databaseId <- "GermanyDA"
# databaseName <- "GermanyDA"
# databaseDescription <- "GermanyDA"
# cdmDatabaseSchema = "CDM_IQVIA_GERMANY_DA_V1049.dbo"
# outputFolder <- file.path(studyFolder, databaseName)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_germanyda"
# 
# # FranceDA --------------------------------------------------------------------
# databaseId <- "FranceDA"
# databaseName <- "FranceDA"
# databaseDescription <- "FranceDA"
# cdmDatabaseSchema = "CDM_IQVIA_FRANCE_DA_V1047.dbo"
# outputFolder <- file.path(studyFolder, databaseName)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_franceda"
# 
# # PanTher ----------------------------------------------------------------------
# databaseId <- "PanTher"
# databaseName <- "PanTher"
# databaseDescription <- "PanTher"
# cdmDatabaseSchema = "CDM_PANTHER_V1020.dbo"
# outputFolder <- file.path(studyFolder, databaseName)
# cohortDatabaseSchema <- "scratch.dbo"
# cohortTable <- "bcn_panther"

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
                runAnalyses = TRUE,
                runDiagnostics = TRUE,
                packageResults = TRUE,
                maxCores = maxCores)
}, mailSettings = mailSettings, label = paste0("EhdenRaDmardsEstimation ", databaseId), stopOnWarning = FALSE)

resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
launchEvidenceExplorer(dataFolder = dataFolder, blind = TRUE, launch.browser = FALSE)
