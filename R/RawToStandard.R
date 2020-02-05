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
#' RawToStandard
#' Raw data to standard data form for visualization
#' @param tableType 'cohort' or 'episode'
#' @param connectionDetails
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param cohortName
#' @param gapDate
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param targetRegimenIds
#' @keywords standard
#' @return standard data for visualization
#' @examples 
#' @import data.table
#' @import dplyr
#' @export
cohortToStandardCycle<- function(connectionDetails,
                                 resultDatabaseSchema,
                                 cohortTable,
                                 targetCohortIds,
                                 cohortName,
                                 gapDate){
  rawData<-treatmentCohort(connectionDetails,
                           resultDatabaseSchema,
                           cohortTable,
                           targetCohortIds)
  cohortNameTable<-data.frame(targetCohortIds,cohortName)
  cohortWtName<-dplyr::left_join(rawData,cohortNameTable,by=c('cohortDefinitionId' = 'targetCohortIds'))
  cohortWtDiff <- cohortWtName %>% group_by(subjectId,cohortDefinitionId) %>% arrange(subjectId,cohortStartDate) %>% mutate(dateDiff = (cohortStartDate-lag(cohortStartDate)))
  cohortWtDiff$dateDiff<-as.numeric(cohortWtDiff$dateDiff) 
  cohortWtDiff$flagSeq <- NA
  cohortWtDiff$flagSeq[is.na(cohortWtDiff$dateDiff)|cohortWtDiff$dateDiff>=gapDate] <- 1 
  standardCycle<-as.data.table(cohortWtDiff)
  standardCycle[, cycle := seq_len(.N), by=.(cumsum(!is.na(flagSeq)))]
  standardCycle<-standardCycle %>% select(cohortDefinitionId,subjectId,cohortStartDate,cohortEndDate,cohortName,cycle)
  standardCycle<-data.frame(standardCycle)
  return(standardCycle)}

#' @export
episodeToStandardCycle <- function(connectionDetails,
                                   vocaDatabaseSchema,
                                   oncologyDatabaseSchema,
                                   episodeTable,
                                   targetRegimenIds){
  rawData <- episodeCycle(connectionDetails,
                          vocaDatabaseSchema,
                          oncologyDatabaseSchema,
                          episodeTable,
                          targetRegimenIds)
  standardCycle<- rawData %>% select(episodeSourceConceptId,personId,episodeStartDatetime,episodeEndDatetime,conceptName,episodeNumber)
  colnames(standardCycle) <- c('cohortDefinitionId','subjectId','cohortStartDate','cohortEndDate','cohortName','cycle')
  return(standardCycle)
}

