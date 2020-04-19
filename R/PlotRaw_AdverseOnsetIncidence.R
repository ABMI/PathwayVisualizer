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
plotRaw_4 <- function(connectionDetails,
                      cohortDatabaseSchema,
                      cohortTable,
                      numberedCohort,
                      cohortDescript,
                      eventCohortIds,
                      restrictInitialTreatment = T,
                      restrictInitialEvent = T,
                      minimumCellCount,
                      outputFileTitle,
                      outputFolderPath){

  # 1. Usage pattern graph
  # 2. Treatment Iteration heatmap
  # 3. Treatment Pathway - including table
  # 4. Event incidence in each cycle

  # Initial treatment
  if(restrictInitialTreatment){
  cohortFirstIndex <- numberedCohort %>% subset(cycle == 1) %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% mutate(index= row_number())
  indexedCohort <- left_join(numberedCohort,cohortFirstIndex)
  indexedCohort$index <- data.table::nafill(indexedCohort$index, type = "locf")
  numberedCohort <- indexedCohort %>% subset(index == 1) %>% select(-index)
}
  # Event Cohort
  eventCohort <- loadCohort(connectionDetails,
                            cohortDatabaseSchema,
                            cohortTable,
                            eventCohortIds)
  eventCohort <- dplyr::left_join(eventCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))
  eventCohort <- unique(eventCohort %>% mutate (cycle = 0) %>% select(-type) %>% subset(subjectId %in% numberedCohort$subjectId)) %>% select(-conceptId)

  # Cohort name cycle
  collapsedCohort <- rbind(numberedCohort,eventCohort) %>% arrange(subjectId,cohortStartDate) %>% mutate(cohort_cycle = paste0(cycle,'_',cohortName))

  # Prev record
  collapsedCohort <- collapsedCohort %>% arrange(subjectId,cohortStartDate,desc(cohort_cycle)) %>% group_by(subjectId) %>% mutate(prev_c_n_c = lag(cohort_cycle)) %>% mutate(prevDate = lag(cohortStartDate)) %>% ungroup()

  # Event after target
  eventAfterTarget <- unique(na.omit(collapsedCohort %>% subset(cohortName %in% unique(eventCohort$cohortName)) %>% subset(cohort_cycle != prev_c_n_c)) %>% subset(cohortStartDate-prevDate<= eventPeriod))

  # Restrict first event
  if(restrictInitialEvent){
  eventAfterTarget <- eventAfterTarget %>% arrange(subjectId,cohortStartDate) %>% group_by(subjectId) %>% slice(1)
}
  summariseEvent <- unique(eventAfterTarget %>% group_by(prev_c_n_c)) %>% summarise(n=n())

  summariseTarget <- unique(numberedCohort %>% mutate(cohort_cycle = paste0(cycle,'_',cohortName)) %>% group_by(cohort_cycle)) %>% summarise(n=n())

  # Collapse summarised data
  collapsedSummarise <- left_join(summariseTarget,summariseEvent, by=c('cohort_cycle'="prev_c_n_c"))
  colnames(collapsedSummarise) <- c('cohort_cycle','total','event')
  collapsedSummarise <- as.data.frame(collapsedSummarise)
  collapsedSummarise[is.na(collapsedSummarise)] <- 0

  # Minimum Cell Count
  collapsedSummarise <- collapsedSummarise %>% subset(total >= minimumCellCount)

  seperateNameIndex <- unique(numberedCohort %>% mutate(cohort_cycle = paste0(cycle,'_',cohortName)) %>% select(cohortName,cycle,cohort_cycle))

  # Plot data
  plotData <- left_join(collapsedSummarise,seperateNameIndex) %>% mutate(ratio = event/total) %>% select(cycle,cohortName,event,total,ratio,cohort_cycle) %>% arrange(cohortName,cycle)

    fileName <- paste0(outputFileTitle,'_','AdverseOnsetIncidence.csv')
    write.csv(plotData, file.path(outputFolderPath, fileName),row.names = F)

    return(plotData)
  # 5. Event onset timing
}
