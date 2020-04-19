# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PathwayVisualizer
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

# Create & Generate episode cohort

#' @export
createCohortTable <- function(connection,
                              oracleTempSchema = NULL,
                              cohortDatabaseSchema = cdmDatabaseSchema,
                              cohortTable){
  # Create Cohort table in your DB
  ParallelLogger::logInfo("Create table for the cohorts")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                           packageName = "PathwayVisualizer",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}

#' @export
generateEpisodeCohort <- function(connectionDetails,
                                  oracleTempSchema,
                                  cdmDatabaseSchema,
                                  oncologyDatabaseSchema,
                                  vocaDatabaseSchema,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  episodeTable,
                                  includeConceptIdSetDescendant,
                                  collapseGapSize,
                                  conceptIdSet,
                                  targetCohortId){

  if(length(targetCohortId) != 1) stop (
    "please specify targetCohortId as one integer. It cannot be multiple."
  )

  if(length(as.numeric(conceptIdSet)) <1 ) stop (
    "please specify concept Id Set as a numeric vector"
  )

  # Generate cohort records using episode table

  ParallelLogger::logInfo(paste0("Episode cohort_",targetCohortId, " generation start"))

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "EpisodeCohortGeneration.sql",
                                           packageName = "PathwayVisualizer",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           vocabulary_database_schema = vocaDatabaseSchema,
                                           target_database_schema = cohortDatabaseSchema,
                                           target_cohort_table = cohortTable,
                                           episode_table = episodeTable,
                                           include_descendant = includeConceptIdSetDescendant,
                                           collapse_gap_size = collapseGapSize,
                                           episode_source_concept_ids = paste(conceptIdSet,collapse=","),
                                           target_cohort_id = targetCohortId)

  DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

  DatabaseConnector::disconnect(connection)

}
