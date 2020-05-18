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
plotRaw_5 <- function(connectionDetails,
                      cohortDatabaseSchema,
                      cohortTable,
                      numberedCohort,
                      cohortDescript,
                      eventCohortIds,
                      treatmentEffectDates = 3,
                      observationDate = 60,
                      outputFileTitle,
                      outputFolderPath,
                      saveFile = TRUE){

  # 1. Usage pattern graph
  # 2. Treatment Iteration heatmap
  # 3. Treatment Pathway - including table
  # 4. Event incidence in each cycle
  # 5. Event onset timing

  # Initial treatment
  cohortFirstIndex <- numberedCohort %>% subset(cycle == 1) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(index= row_number())
  indexedCohort <- left_join(numberedCohort,cohortFirstIndex)
  indexedCohort$index <- data.table::nafill(indexedCohort$index, type = "locf")
  numberedCohort <- indexedCohort %>% subset(index == 1) %>% select(-index)

  # Event Cohort
  eventCohort <- loadCohort(connectionDetails,
                            cohortDatabaseSchema,
                            cohortTable,
                            eventCohortIds)
  eventCohort <- dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
  eventCohort <- unique(eventCohort %>% mutate (cycle = 0) %>% select(-type) %>% subset(subjectId %in% numberedCohort$subjectId)) %>% select(-conceptId)

  # Ignore the occurrence of an event effected by treatment
  numberedCohort <- numberedCohort %>% subset(subjectId %in% eventCohort$subjectId)

  eventCohort <- data.table::rbindlist(lapply(unique(numberedCohort$subjectId),function(i){
    targetData <- numberedCohort %>% subset(subjectId == i)
    eventData <- eventCohort %>% subset(subjectId == i)
    for( x in 1:nrow(targetData)){
      eventData$cohortStartDate[eventData$cohortStartDate <= targetData$cohortStartDate[x]+treatmentEffectDates & eventData$cohortStartDate >= targetData$cohortStartDate[x] - treatmentEffectDates] <- NA
    }
    return(eventData)
  }))

  eventCohort <- na.omit(eventCohort)
  eventCohort <- as.data.frame(eventCohort)

  # Records with cohort name
  collapsedCohort <- rbind(numberedCohort,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% mutate(cohort_cycle = paste0(cycle,'_',
                                                                                                                               cohortName
  ))

  # Prev record column
  collapsedCohort <- collapsedCohort %>% arrange(subjectId,cohortStartDate,desc(cohort_cycle))%>% group_by(subjectId) %>% mutate(prev_c_n_c = lag(cohort_cycle)) %>% mutate(prevDate = lag(cohortStartDate)) %>% ungroup() %>% subset(cycle == 0) %>% subset(cohort_cycle != prev_c_n_c)
  # Subset event after target
  eventAfterTarget <- unique(na.omit(collapsedCohort %>% subset(cohortName %in% unique(eventCohort$cohortName)))) %>% subset(cohortStartDate - prevDate <= observationDate)

  # Date Diff
  eventAfterTarget <- eventAfterTarget %>% arrange(subjectId,cohortStartDate)%>% group_by(subjectId) %>% slice(1) %>% mutate(dateDiff = as.integer(cohortStartDate - prevDate)) %>% select(subjectId,prev_c_n_c,cohortStartDate,cohortEndDate,dateDiff)

  # Split Cycle Index
  targetIndex <- unique(numberedCohort %>% mutate(cohort_cycle = paste0(cycle,'_',cohortName)) %>% group_by(cohort_cycle) %>% select(cohortName,cohort_cycle,cycle))%>% ungroup()

  # Collapse summarised data
  collapsedCohort <- left_join(eventAfterTarget,targetIndex, by=c("prev_c_n_c"='cohort_cycle')) %>% select(-prev_c_n_c)
  collapsedCohort <- as.data.frame(collapsedCohort) %>% subset(cycle == 1)

  # summary of the data
  savedata <- collapsedCohort %>% group_by(cohortName,dateDiff) %>% summarise(n=n())

  plotdata <- as.data.frame(data.table::rbindlist(lapply(1:nrow(savedata),function(i){cohortName <- rep(savedata[i,]$cohortName,savedata[i,]$n)
  dateDiff <- rep(savedata[i,]$dateDiff,savedata[i,]$n)
  targerRecord<- data.frame(cohortName,dateDiff)
  return(targerRecord)})))

  plotdata<-plotdata %>% mutate(category = ifelse(dateDiff<1,'d1',ifelse(dateDiff<=7,'d2-d8',ifelse(dateDiff<=14,'d9-d15',ifelse(dateDiff<=21,'d16-d22',ifelse(dateDiff<=29,'-d30','>d30'))))))
  plotdata$category <- factor(plotdata$category,levels = c('d1','d2-d8','d9-d15','d16-d22','-d30','>d30'))

  if(saveFile){
    fileName <- paste0(outputFileTitle,'_','AdverseOnset.csv')
    write.csv(savedata, file.path(outputFolderPath, fileName),row.names = F)
  }
  return(savedata)

}
