#Code to Run

library(flexdashboard)
# Postgres server
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms='pdw',
                                                                server=Sys.getenv("PDW_SERVER"),
                                                                schema='cdmDatabaseSchema',
                                                                user=NULL,
                                                                password=NULL,
                                                                port='port')

# Postgres server-Database

oracleTempSchema <- NULL
cdmDatabaseSchema <- "cdm_database_schema.dbo"
cohortDatabaseSchema <- "cohort_database_schema.dbo"
vocaDatabaseSchema <- "voca_database_schema.dbo"
oncologyDatabaseSchema <- "oncology_database_schema.dbo"

episodeTable <- "episode"

cohortTable <- "cohort"

outputFolderPath <- 'C:/output'
outputFileTitle <- 'colorectal'

targetCohortIds <- c(4:11)
eventCohortIds <- c(42,62) # Colectomy,Death,Radiation...
adverseCohortIds <- 45 # Neutropenia
conditionCohortIds <- NULL

combinationWindow <- 5
maximumPathLength <- 3
minimumPathLength <- 1
minimumCellCount <- 5

cohortTableCreation <- FALSE
episodeCohortGeneration <- FALSE
heatmapColor <- 'blue'

executeVisualization(connectionDetails,
                     oracleTempSchema,
                     cohortDatabaseSchema,
                     cdmDatabaseSchema,
                     oncologyDatabaseSchema = NULL,
                     vocaDatabaseSchema,
                     cohortTable,
                     episodeTable,
                     conditionCohortTable,
                     outputFolderPath,
                     outputFileTitle,
                     combinationWindow = 5,
                     maximumPathLength = 3,
                     minimumPathLength = 3,
                     minimumCellCount = 5,
                     conditionCohortIds = NULL,
                     targetCohortIds,
                     eventCohortIds,
                     adverseCohortIds,
                     cohortTableCreation = FALSE,
                     episodeCohortGeneration = FALSE,
                     heatmapColor = 'blue')

patternRaw(connectionDetails,
           cohortDatabaseSchema,
           cohortTable,
           regimenCohortIds,
           eventCohortIds,
           interestCohortIds)


