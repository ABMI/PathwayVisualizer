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
plotRaw_1 <- function(fromYear,
                      toYear,
                      numberedCohort,
                      cohortDescript,
                      outputFileTitle,
                      outputFolderPath,
                      saveFile = TRUE){

  # 1. Usage pattern graph

  numberedCohort <- numberedCohort %>% subset(cycle == 1)
  numberedCohort <- numberedCohort %>% select(-cohortName,-cycle)
  numberedCohort$cohortStartDate <- as.Date(numberedCohort$cohortStartDate)
  numberedCohort$cohortEndDate <- as.Date(numberedCohort$cohortEndDate)
  numberedCohort <- dplyr::left_join(numberedCohort,cohortDescript, by= c("cohortDefinitionId"="cohortDefinitionId"))

  numberedCohort <- numberedCohort %>% select(subjectId,cohortName,cohortStartDate)
  numberedCohort$cohortStartDate <- format(as.Date(numberedCohort$cohortStartDate, format="Y-%m-%d"),"%Y")

  numberedCohort <- numberedCohort %>% group_by(cohortStartDate,cohortName)
  numberedCohort <- unique(numberedCohort)
  numberedCohort <- numberedCohort %>%
    summarise(n=n()) %>%
    ungroup() %>%
    arrange(cohortName,cohortStartDate) %>%
    group_by(cohortStartDate) %>%
    mutate(total = sum(n)) %>%
    mutate(proportion = round(n/total*100,1)) %>%
    select(cohortStartDate,cohortName,proportion)

  colnames(numberedCohort) <- c('Year','Cohort','proportion')
  numberedCohort$Year <- as.integer(numberedCohort$Year)
  Year <- rep(c(fromYear:toYear),length(unique(numberedCohort$Cohort)))
  Cohort <- sort(rep(unique(numberedCohort$Cohort),length(c(fromYear:toYear))))
  index <- data.frame(Year,Cohort)
  index$Year <- as.integer(index$Year)
  index$Cohort <- as.character(index$Cohort)
  plotData <- left_join(index,numberedCohort)
  plotData[is.na(plotData)] <- 0

  # Write raw data
  if(saveFile){
  fileName <- paste0(outputFileTitle,'_','RegimenUsagePattern.csv')
  write.csv(plotData, file.path(outputFolderPath, fileName),row.names = F)
}
  return(plotData)

  # 2. Treatment Iteration heatmap
  # 3. Treatment Pathway - including table
  # 4. Event incidence in each cycle
  # 5. Event onset timing

}
