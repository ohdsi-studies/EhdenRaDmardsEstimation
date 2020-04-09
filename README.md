DMARDs comparative safety in RA (EHDEN Study-a-thon Barcelona 2020)
=================

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Tags: **EHDEN**
- Study lead: **Jamie Weaver**
- Study lead forums tag: **[jweave17](https://forums.ohdsi.org/u/jweave17)**
- Study start date: **January 13, 2020**
- Study end date: **-**
- Protocol: [**Word file**](https://github.com/ohdsi-studies/EhdenRaDmardsEstimation/blob/master/documents/20200117_RAPLE_EUPAS_final.docx)
- Publications: **-**
- Results explorer: **[Shiny app](https://data.ohdsi.org/EhdenRaDmardsEstimation/)**

This study aims to estimate the population level-effects of conventional synthetic disease-modifying antirheumatic drugs among patients with rheumatoid arthritis.

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- R version 3.5.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

See [this video](https://youtu.be/K9_0s2Rchbo) for instructions on how to set up the R environment on Windows.

How to run
==========
1. In `R`, use the following code to install the dependencies:

	```r
	install.packages("devtools")
	library(devtools)
	install_github("ohdsi/SqlRender")
	install_github("ohdsi/DatabaseConnector")
	install_github("ohdsi/OhdsiSharing")
	install_github("ohdsi/FeatureExtraction")
	install_github("ohdsi/CohortMethod")
	install_github("ohdsi/EmpiricalCalibration")
	install_github("ohdsi/MethodEvaluation")
	```

	If you experience problems on Windows where rJava can't find Java, one solution may be to add `args = "--no-multiarch"` to each `install_github` call, for example:
	
	```r
	install_github("ohdsi/SqlRender", args = "--no-multiarch")
	```
	
	Alternatively, ensure that you have installed both 32-bit and 64-bit JDK versions, as mentioned in the [video tutorial](https://youtu.be/K9_0s2Rchbo).
	
2. In 'R', use the following code to install the EhdenRaDmardsEstimation package:

  	```r
	install_github("ohdsi-studies/EhdenRaDmardsEstimation", args = "--no-multiarch")
	```
	
3. Once installed, you can execute the study by modifying and using the following code:
	
	```r
	library(EhdenRaDmardsEstimation)
	
	# Optional: specify where the temporary files (used by the ff package) will be created:
	options(fftempdir = "c:/FFtemp")
	
	# Maximum number of cores to be used:
	maxCores <- parallel::detectCores()
	
	# Minimum cell count when exporting data:
	minCellCount <- 5
	
	# The folder where the study intermediate and result files will be written:
	outputFolder <- "c:/EhdenRaDmardsEstimation"
	
	# Details for connecting to the server:
	# See ?DatabaseConnector::createConnectionDetails for help
	connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "",
									server = "",
									user = "",
									password = "")
	
	# The name of the database schema where the CDM data can be found:
	cdmDatabaseSchema <- ""
	
	# The name of the database schema and table where the study-specific cohorts will be instantiated:
	cohortDatabaseSchema <- ""
	cohortTable <- ""
	
	# Some meta-information that will be used by the export function:
	databaseId <- "" # required
	databaseName <- "" # required
	databaseDescription <- ""
	
	# For Oracle: define a schema that can be used to emulate temp tables:
	oracleTempSchema <- NULL
	
	execute(connectionDetails = connectionDetails,
		cdmDatabaseSchema = cdmDatabaseSchema,
		cohortDatabaseSchema = cohortDatabaseSchema,
		cohortTable = cohortTable,
		oracleTempSchema = oracleTempSchema,
		outputFolder = outputFolder,
		databaseId = databaseId,
		databaseName = databaseName,
		databaseDescription = databaseDescription,
		createCohorts = TRUE,
		synthesizePositiveControls = FALSE,
		runAnalyses = TRUE,
		runDiagnostics = TRUE,
		packageResults = TRUE
		maxCores = maxCores,
		minCellCount = minCellCount)
	```

4. To view the results, use the Shiny app:

  ```r
	resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
launchEvidenceExplorer(dataFolder = dataFolder, blind = TRUE, launch.browser = FALSE)
  ```
  
  Note that you can save plots from within the Shiny app (You can do this by re-building the package). It is possible to view results from more than one database by applying `prepareForEvidenceExplorer` to the Results file from each database, and using the same data folder. Set `blind = FALSE` if you wish to be unblinded to the final results.


5. When completed, the output will exist as a .ZIP file in the `resultsZipFile` directory location. This file contains the results to submit to the study lead.

License
=======

The DMARDs comparative safety in RA package is licensed under Apache License 2.0

