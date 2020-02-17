# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of treatmentCycleVisualization
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
#' Yearly graph
#' Yearly graph in treatment regimen
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param targetRegimen
#' @param fromYear
#' @param toYear
#' @keywords year
#' @return Yearly treatment regimen highcharter graph
#' @examples
#' @import dplyr
#' @import tidyr
#' @import highcharter
#' @export yearlyGraph
yearlyGraph<-function(connectionDetails,
                        vocaDatabaseSchema,
                        oncologyDatabaseSchema,
                        episodeTable,
                        targetCohortIds,
                        fromYear,
                        toYear){
  ##Condition cohort##
  if(!is.null(conditionCohortIds)){
    conditionCohort<-cohortRecords(connectionDetails,
                                   resultDatabaseSchema,
                                   cohortTable,
                                   conditionCohortIds)}

  ##Treatment cohort##
  cohortDescript <- cohortDescription()
  treatmentLineCohort<-cohortRecords(connectionDetails,
                                     resultDatabaseSchema,
                                     cohortTable,
                                     targetCohortIds)
  if(!is.null(conditionCohortIds)){treatmentLineCohort<-treatmentLineCohort %>% subset(subjectId %in% conditionCohort$subjectId)}
  treatmentLineCohort$cohortStartDate<-as.Date(treatmentLineCohort$cohortStartDate)
  treatmentLineCohort$cohortEndDate<-as.Date(treatmentLineCohort$cohortEndDate)
  treatmentLineCohort<-dplyr::left_join(treatmentLineCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))

  treatmentLineCohort<-treatmentLineCohort %>% select(subjectId,cohortName,cohortStartDate)
  treatmentLineCohort$cohortStartDate<-format(as.Date(treatmentLineCohort$cohortStartDate, format="Y-%m-%d"),"%Y")

  treatmentLineCohort<-treatmentLineCohort %>% group_by(cohortStartDate,cohortName)
  treatmentLineCohort<-unique(treatmentLineCohort)
  treatmentLineCohort<-treatmentLineCohort %>% summarise(n=n()) %>%ungroup() %>%  arrange(cohortName,cohortStartDate) %>% subset(cohortStartDate <=toYear & cohortStartDate >=fromYear) %>% group_by(cohortStartDate) %>% mutate(total = sum(n)) %>% mutate(ratio = round(n/total*100,1)) %>% select(cohortStartDate,cohortName,ratio)
  colnames(treatmentLineCohort) <- c('Year','Regimen','ratio')
  h<-treatmentLineCohort %>% highcharter::hchart(.,type="line",hcaes(x = Year,y=ratio,group = Regimen))
  return(h)}
