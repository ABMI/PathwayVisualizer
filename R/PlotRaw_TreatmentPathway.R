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

#' @import data.table
#' @import dplyr
#' @export
plotRaw_3 <- function(connectionDetails,
                      cohortDatabaseSchema,
                      cohortTable,
                      eventCohortIds,
                      conditionCohortIds = NULL,
                      surgeryCohortIds,
                      numberedCohort,
                      cohortDescript,
                      combinationWindow,
                      maximumPathLength,
                      minimumPathLength,
                      minimumCellCount,
                      outputFileTitle,
                      outputFolderPath,
                      saveFile = TRUE,
                      setting = TRUE){

  # 1. Usage pattern graph
  # 2. Treatment Iteration heatmap
  # 3. Treatment Pathway - including table

  if(!is.null(conditionCohortIds)){

    conditionCohort <- loadCohort(connectionDetails,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  conditionCohortIds)

    numberedCohort <- numberedCohort %>% subset(subjectId %in% conditionCohort$subjectId)

  }

  numberedCohort <- numberedCohort %>% subset(cycle == 1)
  cohortData <- numberedCohort %>% select(-cohortName,-cycle)
  cohortData$cohortStartDate <- as.Date(cohortData$cohortStartDate)
  cohortData$cohortEndDate <- as.Date(cohortData$cohortEndDate)
  cohortData <- dplyr::left_join(cohortData,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))


  # Event cohort
  if(!is.null(eventCohortIds)){

    eventCohort <- loadCohort(connectionDetails,
                              cohortDatabaseSchema,
                              cohortTable,
                              eventCohortIds)

    eventCohort <- dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))

    if(!is.null(conditionCohortIds)){eventCohort <- eventCohort %>% subset(subjectId %in% conditionCohort$subjectId)}

    colnames(eventCohort) <- colnames(cohortData)

    eventCohort$cohortStartDate <- as.Date(eventCohort$cohortStartDate)
    eventCohort$cohortEndDate <- as.Date(eventCohort$cohortEndDate)

  }

  # Ignore the change to same regimen
  cohortData <- cohortData %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI)
  cohortData <- as.data.frame(cohortData)

  # Bind event and target cohort, Ignore duplicated event records
  if(!is.null(eventCohortIds)){

    eventAndTarget <- rbind(cohortData,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()

    eventAndTarget$cohortName <- as.character(eventAndTarget$cohortName)

    eventAndTarget <- as.data.frame(eventAndTarget)}else{
      eventAndTarget <- cohortData %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(lagCDI = lag(cohortName)) %>% subset(is.na(lagCDI)|lagCDI != cohortName) %>% select(-lagCDI) %>% ungroup()
      eventAndTarget$cohortName <- as.character(eventAndTarget$cohortName)
      eventAndTarget <- as.data.frame(eventAndTarget)
    }

  # If regimens apart from each other less than combinationWindow, collapse using '/'

  collapsedRecords <- data.table::rbindlist(lapply(unique(eventAndTarget$subjectId),function(targetSubjectId){
    reconstructedRecords <- data.frame()
    targeteventAndTarget <- eventAndTarget %>% subset(subjectId == targetSubjectId)
    reconstructedRecords <- rbind(reconstructedRecords,targeteventAndTarget[1,])

    if(nrow(targeteventAndTarget) >= 2){
      for(x in 2:nrow(targeteventAndTarget)){
        if(as.integer(targeteventAndTarget[x,3]-reconstructedRecords[nrow(reconstructedRecords),3])>combinationWindow){
          reconstructedRecords <-rbind(reconstructedRecords,targeteventAndTarget[x,])}else{sortNames<-sort(c(targeteventAndTarget[x,5],reconstructedRecords[nrow(reconstructedRecords),5]))
          reconstructedRecords[nrow(reconstructedRecords),5]<-paste0(sortNames,collapse = '+')
          }
      }
    }

    return(reconstructedRecords)
  }
  )
  )
  # Set minimum regimen change count
  eventAndTarget <- collapsedRecords
  minimunIndexId <- unique(eventAndTarget %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(line = row_number()) %>% subset(line >= minimumPathLength) %>% select(subjectId) %>% ungroup())
  eventAndTarget <- eventAndTarget %>% subset(subjectId %in% minimunIndexId$subjectId) %>% arrange(subjectId,cohortStartDate)

  # Maximum path length in graph
  eventAndTarget <- eventAndTarget %>% group_by(subjectId) %>% arrange(subjectId,cohortStartDate) %>% mutate(rowNumber = row_number()) %>% subset(rowNumber <= maximumPathLength) %>% select(subjectId,cohortName,rowNumber) %>% mutate(nameOfConcept = paste0(rowNumber,'_',cohortName)) %>% ungroup()
  # Padding only first line node
  eventAndTarget <- rbind(eventAndTarget,eventAndTarget %>% subset(rowNumber == 1) %>% subset(!subjectId %in% (eventAndTarget %>% subset(rowNumber == 2))$subjectId) %>% mutate(rowNumber = 2, cohortName = "received only first line", nameOfConcept = "2_received only first line")) %>% arrange(subjectId, rowNumber)

  if(setting){
  # Split neoadjuvant / adjuvant
  neoadjuvantSubjectId <- eventAndTarget %>% subset(rowNumber == 1 & cohortName == (cohortDescript %>% subset(cohortDefinitionId == surgeryCohortIds))[,2]) %>% select(subjectId)

  adjuvant <- eventAndTarget %>% subset(subjectId %in% neoadjuvantSubjectId$subjectId)
  adjuvant <- list(setting = adjuvant, settingName = 'adjuvant')

  neoadjuvant <- eventAndTarget %>% subset(!subjectId %in% neoadjuvantSubjectId$subjectId)
  neoadjuvant <- list(setting = neoadjuvant, settingName = 'neoadjuvant')

  settings <- list(neoadjuvant,adjuvant)

  settingRaw <- lapply(settings,afterSettingDefined,minimumCellCount = minimumCellCount,
                       maximumPathLength = maximumPathLength,
                       saveFile = saveFile,
                       outputFolderPath = outputFolderPath,
                       outputFileTitle = outputFileTitle)}

  noSettingData <- list(setting = eventAndTarget, settingName = 'total')

  noSettingRaw <- afterSettingDefined(settingDefinedData = noSettingData,
                                      minimumCellCount,
                                      maximumPathLength,
                                      saveFile,
                                      outputFolderPath,
                                      outputFileTitle)

  if(setting){
    rawDataList <- append(list(noSettingRaw),settingRaw)
  }else{
    rawDataList <- list(noSettingRaw)
    }

  # 4. Event incidence in each cycle
  # 5. Event onset timing
  return(rawDataList)

}

#' @export
afterSettingDefined <- function(settingDefinedData,
                                minimumCellCount,
                                maximumPathLength,
                                saveFile,
                                outputFolderPath,
                                outputFileTitle){

  settingDataName <- settingDefinedData$settingName
  settingDefinedData <- settingDefinedData$setting

  # Exclude patients until minimum nodes cell count under criteria

  nodePatientNo <- settingDefinedData %>% group_by(nameOfConcept) %>% summarise(n=n())

  while(min(nodePatientNo$n) < minimumCellCount){

    nodePatientNo <- settingDefinedData %>% group_by(nameOfConcept) %>% summarise(n=n())

    settingDefinedData <- settingDefinedData %>% subset(!subjectId %in% (settingDefinedData %>% subset(nameOfConcept %in% (nodePatientNo %>% subset(n < minimumCellCount))$nameOfConcept))$subjectId)
  }

  # Nodes
  treatmentRatio <- data.table::rbindlist(lapply(1:maximumPathLength,function(x){
    result <- settingDefinedData %>% subset(rowNumber==x) %>% group_by(nameOfConcept) %>% summarise(n=n()) %>% mutate(ratio=round(n/sum(n)*100,1))
    return(result)}
  )
  )

  # Label
  label <- unique(settingDefinedData %>% select(cohortName,nameOfConcept) %>% arrange(nameOfConcept))
  label <- label %>% mutate(num = seq(from = 0,length.out = nrow(label)))

  label <- dplyr::left_join(treatmentRatio,label,by=c("nameOfConcept"="nameOfConcept")) %>% mutate(name = paste0(cohortName,' (n=',n,', ',ratio,'%)'))
  label <- label %>% mutate(num = seq(from = 0, length.out = nrow(label)))

  nodes <- label %>% select(name,cohortName)
  colnames(nodes) <- c('name','group')
  nodes <- data.frame(nodes)

  # Pivot table
  pivotRecords <- reshape2::dcast(settingDefinedData,subjectId ~ rowNumber, value.var="nameOfConcept")

  # Write pathway table
  pathwayRecords <- lapply(1 : nrow(pivotRecords), function(x){

    pathway <- paste0(stringr::str_sub((pivotRecords %>% select(-subjectId))[x,],start = 3), collapse = '-')
    return(pathway)

  }
  )

  pathway <- data.frame(unlist(pathwayRecords))

  colnames(pathway) <- 'pathway'

  pathwayTable <- pathway %>% group_by(pathway) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percentLabel = round(n/sum(n)*100,2))
  sumPathway <- sum(pathwayTable$n)

  pathwayTable <- pathwayTable %>% mutate(label = paste0(n,' (',percentLabel,'%)')) %>% select(pathway,label)

  colnames(pathwayTable) <- c('Pathway',paste0('N = ',sumPathway))
  pathwayTable <- as.data.frame(pathwayTable)

  if(saveFile){
    fileNamePathway <- paste0(settingDataName,'_',outputFileTitle,'_','pathway.csv')
    write.csv(pathwayTable, file.path(outputFolderPath, fileNamePathway),row.names = F)
  }
  # Link
  link <- data.table::rbindlist(lapply(2:max(settingDefinedData$rowNumber),function(x){
    source <- pivotRecords[,x]
    target <- pivotRecords[,x+1]
    link <-data.frame(source,target)
    link$source<-as.character(link$source)
    link$target<-as.character(link$target)
    link<-na.omit(link)
    return(link)
  }))

  link$source <- as.character(link$source)
  link$target <- as.character(link$target)
  link <- link %>% select(source,target)%>% group_by(source,target)%>% summarise(n=n()) %>% ungroup()

  group <- dplyr::left_join(link,label,by = c("source" = "nameOfConcept")) %>% select(cohortName)
  source <- dplyr::left_join(link,label,by = c("source" = "nameOfConcept")) %>% select(num)
  target <- dplyr::left_join(link,label,by = c("target" = "nameOfConcept")) %>% select(num)
  freq <- link %>% select(n)
  links <- data.frame(group,source,target,freq)
  links <- na.omit(links)

  colnames(links) <- c('group','source','target','value')
  links$source <- as.integer(links$source)
  links$target <- as.integer(links$target)
  links$value <- as.numeric(links$value)

  # Write raw data
  treatment <- list(nodes=nodes,links=links,pathways = pathwayTable)
  if(saveFile){
    fileNameNodes <- paste0(settingDataName,'_',outputFileTitle,'_','SankeyNodes.csv')
    write.csv(nodes, file.path(outputFolderPath, fileNameNodes),row.names = F)
    fileNameLinks <- paste0(settingDataName,'_',outputFileTitle,'_','SankeyLinks.csv')
    write.csv(links, file.path(outputFolderPath, fileNameLinks),row.names = F)
  }

  return(treatment)
}
