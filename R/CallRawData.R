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
#' CallRawData
#' Bring the chemotherapy raw data table.
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param resultDatabaseSchema
#' @param episodeTable
#' @param cohortTable
#' @param targetCohortIds
#' @param targetRegimenIds
#' @keywords raw data
#' @return raw data for visualization
#' @examples 
#' @export
episodeCycle <- function(connectionDetails,
                         vocaDatabaseSchema,
                         oncologyDatabaseSchema,
                         episodeTable,
                         targetRegimenIds){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32532)
  and episode_source_concept_id in (@target_regimen_ids)'
  sql <- SqlRender::render(sql,voca_database_schema = vocaDatabaseSchema,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable,
                           target_regimen_ids = targetRegimenIds)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)
}
#' @export
episodeLine <- function(connectionDetails,
                        vocaDatabaseSchema,
                        oncologyDatabaseSchema,
                        episodeTable,
                        targetRegimenIds){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32531) and episode_source_concept_id in (@target_regimen_ids)'
  sql <- SqlRender::render(sql,voca_database_schema = vocaDatabaseSchema,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable,
                           target_regimen_ids = targetRegimenIds)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)
}

#' @export
treatmentCohort <- function(connectionDetails,
                            resultDatabaseSchema,
                            cohortTable,
                            targetCohortIds){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select * from @result_database_schema.@cohort_table where cohort_definition_id in (@target_cohort_ids)'
  sql <- SqlRender::render(sql,result_database_schema = resultDatabaseSchema,
                           cohort_table = cohortTable,
                           target_cohort_ids = targetCohortIds)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)
}

