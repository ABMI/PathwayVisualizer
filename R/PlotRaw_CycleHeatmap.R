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
#' @import tidyr
#' @export
plotRaw_2 <- function(targetCohortIds,
                      numberedCohort,
                      outputFileTitle,
                      outputFolderPath){

  # 1. Usage pattern graph
  # 2. Treatment Iteration heatmap

  targetCohortIds <- targetCohortIds[targetCohortIds %in% unique(numberedCohort$cohortDefinitionId)]

  heatmapPlotData <- data.table::rbindlist(

    lapply(targetCohortIds,function(targetId){

      plotData <- distributionTable(data = numberedCohort,
                                    targetId)
      names(plotData) <- c('cycle','n','ratio','cohortName')
      return(plotData)

    }
    )

  )

  # Write raw data
  fileName <- paste0(outputFileTitle,'_','TreatmentHeatmap.csv')
  write.csv(heatmapPlotData, file.path(outputFolderPath, fileName),row.names = F)

  return(heatmapPlotData)

  # 3. Treatment Pathway - including table
  # 4. Event incidence in each cycle
  # 5. Event onset timing

}

#' @export
distributionTable <- function(data,
                              targetId){
  targetdata <- data %>% subset(cohortDefinitionId == targetId)
  maxCycle <- aggregate(targetdata$cycle,by = list(targetdata$subjectId), max)
  colnames(maxCycle) <- c('personId','CycleNum')

  # Total count
  totalCount <- length(unique(maxCycle$personId))

  # Count the number of patients in the value of each cycle number
  distribution <- as.data.frame(maxCycle %>% group_by(CycleNum) %>% summarise(n = n()))
  distribution$'%' <- round(prop.table(table(maxCycle$CycleNum))*100, digits = 1)
  sum <- sum(distribution$n)
  sumName <- paste0('N','(','total=',sum,')')
  distribution$cohortName <- unique(targetdata$cohortName)
  names(distribution) <- c('Treatment cycle',sumName,'%','cohortName')

  return(distribution)

}
