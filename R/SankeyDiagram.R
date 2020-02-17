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
#' sankey for regimen and surgery
#' Visualization tool for sankey for regimen and surgery
#' @param connectionDetails
#' @param resultDatabaseSchema
#' @param conditionCohortIds
#' @param targetCohortIds
#' @param eventCohortIds
#' @param minimumRegimenChange
#' @param treatmentLine
#' @param collapseDates
#' @param nodeMinSubject
#' @keywords sankey
#' @return sankey for regimen with other evnets
#' @examples
#' @import dplyr
#' @import networkD3
#' @export
sankeyDiagram<-function(connectionDetails,
                        resultDatabaseSchema,
                        conditionCohortIds=NULL,
                        targetCohortIds,
                        eventCohortIds=NULL,
                        minimumRegimenChange = 0,
                        treatmentLine = 3,
                        collapseDates = 0,
                        nodeMinSubject = 0
){
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
  ##event cohort##
  if(!is.null(eventCohortIds)){
    eventCohort<-cohortRecords(connectionDetails,
                               resultDatabaseSchema,
                               cohortTable,
                               eventCohortIds)
    eventCohort<-dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
    if(!is.null(conditionCohortIds)){eventCohort<-eventCohort %>% subset(subjectId %in% conditionCohort$subjectId)}
    colnames(eventCohort) <- colnames(treatmentLineCohort)
    eventCohort$cohortStartDate<-as.Date(eventCohort$cohortStartDate)
    eventCohort$cohortEndDate<-as.Date(eventCohort$cohortEndDate)}

  ##Ignore the change to same regimen##
  treatmentLineCohort <- treatmentLineCohort %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI)
  treatmentLineCohort <- as.data.frame(treatmentLineCohort)
  ##Bind event and chemotherapy, Ignore duplicated event records##
  if(!is.null(eventCohortIds)){
    treatmentRecords<-rbind(treatmentLineCohort,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()
    treatmentRecords$cohortName <- as.character(treatmentRecords$cohortName)
    treatmentRecords <- as.data.frame(treatmentRecords)}else{
      treatmentRecords<-treatmentLineCohort %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId)%>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()
      treatmentRecords$cohortName <- as.character(treatmentRecords$cohortName)
      treatmentRecords <- as.data.frame(treatmentRecords)}
  ##If regimens apart from each other less than collapseDates, collapse using '/'##
  collapsedRecords<-data.table::rbindlist(lapply(unique(treatmentRecords$subjectId),function(targetSubjectId){
    reconstructedRecords <-data.frame()
    targetTreatmentRecords<-treatmentRecords %>% subset(subjectId == targetSubjectId)
    reconstructedRecords<-rbind(reconstructedRecords,targetTreatmentRecords[1,])

    if(nrow(targetTreatmentRecords)>=2){
      for(x in 2:nrow(targetTreatmentRecords)){
        if(as.integer(targetTreatmentRecords[x,3]-reconstructedRecords[nrow(reconstructedRecords),3])>collapseDates){
          reconstructedRecords <-rbind(reconstructedRecords,targetTreatmentRecords[x,])}else{sortNames<-sort(c(targetTreatmentRecords[x,5],reconstructedRecords[nrow(reconstructedRecords),5]))
          reconstructedRecords[nrow(reconstructedRecords),5]<-paste0(sortNames,collapse = '/')
          }}}
    return(reconstructedRecords)}))
  ##Set minimum regimen change count##
  treatmentRecords<-collapsedRecords
  minimunIndexId<-unique(treatmentRecords %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(line = row_number()) %>% subset(line >= minimumRegimenChange) %>% select(subjectId) %>% ungroup())
  treatmentRecords<-treatmentRecords %>% subset(subjectId %in% minimunIndexId$subjectId) %>% arrange(subjectId,cohortStartDate)
  ##Maximum treatment line in graph##
  treatmentRecords <- treatmentRecords %>% group_by(subjectId) %>% arrange(subjectId,cohortStartDate) %>% mutate(rowNumber = row_number()) %>% subset(rowNumber <= treatmentLine) %>% select(subjectId,cohortName,rowNumber) %>% mutate(regimenName = paste0(rowNumber,'_',cohortName)) %>% ungroup()
  ##Label##
  label <-unique(treatmentRecords %>% select(cohortName,regimenName) %>% arrange(regimenName))
  label <-label %>% mutate(num = seq(from = 0,length.out = nrow(label)))
  ##Nodes##
  treatmentRatio<-data.table::rbindlist(lapply(1:treatmentLine,function(x){treatmentRecords %>% subset(rowNumber==x) %>% group_by(regimenName) %>% summarise(n=n()) %>% mutate(ratio=round(n/sum(n)*100,1))}))
  treatmentRatio<-treatmentRatio %>% subset(n>=nodeMinSubject)
  label<-dplyr::left_join(treatmentRatio,label,by=c("regimenName"="regimenName")) %>% mutate(name = paste0(cohortName,' (n=',n,', ',ratio,'%)'))
  label<-label %>% mutate(num = seq(from = 0, length.out = nrow(label)))
  nodes<- label %>% select(name)
  nodes<-data.frame(nodes)
  ##Pivot table##
  pivotRecords<-reshape2::dcast(treatmentRecords,subjectId ~ rowNumber, value.var="regimenName")
  ##Link##
  link<-data.table::rbindlist(lapply(2:max(treatmentRecords$rowNumber),function(x){
    source <- pivotRecords[,x]
    target <- pivotRecords[,x+1]
    link <-data.frame(source,target)
    link$source<-as.character(link$source)
    link$target<-as.character(link$target)
    link<-na.omit(link)
    return(link)}))
  link$source<-as.character(link$source)
  link$target<-as.character(link$target)
  link<-link %>% select(source,target)%>% group_by(source,target)%>% summarise(n=n()) %>% ungroup()
  source<-dplyr::left_join(link,label,by = c("source" = "regimenName")) %>% select(num)
  target<-dplyr::left_join(link,label,by = c("target" = "regimenName")) %>% select(num)
  freq<-link %>% select(n)
  links<-data.frame(source,target,freq)
  links<-na.omit(links)
  colnames(links) <-c('source','target','value')
  links$source<-as.integer(links$source)
  links$target<-as.integer(links$target)
  links$value<-as.numeric(links$value)
  ##Sankey data##
  treatment <-list(nodes=nodes,links=links)
  sankeyDiagram <- networkD3::sankeyNetwork(Links = treatment$links, Nodes = treatment$nodes, Source = "source",Target = "target", Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30,sinksRight = FALSE)
  return(sankeyDiagram)
}
