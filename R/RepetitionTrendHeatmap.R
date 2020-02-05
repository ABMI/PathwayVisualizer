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
#' Heatmap
#' Trends in regimen heatmap
#' @param standardData
#' @param targetId
#' @param connectionDetails
#' @param resultDatabaseSchema
#' @param cohortTable
#' @param targetCohortIds
#' @param cohortName
#' @param gapDate
#' @param heatmapPlotData
#' @param maximumCycleNumber
#' @param colors
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples 
#' @import dplyr
#' @import superheat
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @export distributionTable
distributionTable <- function(standardData,
                              targetId){
  targetStandardData <- standardData %>% subset(cohortDefinitionId == targetId)
  maxCycle<-aggregate(targetStandardData$cycle,by = list(targetStandardData$subjectId), max)
  colnames(maxCycle) <- c('person_id','Cycle_num')
  
  # Total count
  totalCount<-length(unique(maxCycle$personId))
  
  # Count the number of patients in the value of each cycle number
  distribution<-as.data.frame(maxCycle %>% group_by(Cycle_num) %>% summarise(n = n()))
  distribution$'%'<-round(prop.table(table(maxCycle$Cycle_num))*100, digits = 1)
  sum<- sum(distribution$n)
  sumName<- paste0('N','(','total=',sum,')')
  distribution$conceptName <- unique(targetStandardData$cohortName)
  names(distribution) <- c('Treatment cycle',sumName,'%','conceptName')
  return(distribution)}

#' @export
heatmapData<-function(connectionDetails,
                      resultDatabaseSchema,
                      cohortTable,
                      targetCohortIds,
                      cohortName,
                      gapDate = 60){
  
  standardCycleData<-cohortToStandardCycle(connectionDetails,
                                           resultDatabaseSchema,
                                           cohortTable,
                                           targetCohortIds,
                                           cohortName,
                                           gapDate)
  
  heatmapPlotData <-data.table::rbindlist(
    lapply(targetCohortIds,function(targetId){
      result<-distributionTable(standardData=standardCycleData,
                                targetId=targetId)
      names(result) <- c('cycle','n','ratio','conceptName')
      return(result)})
  )
  
  return(heatmapPlotData)
}

#' @export repetitionTrendHeatmap
repetitionTrendHeatmap<-function(heatmapPlotData,
                                 maximumCycleNumber = 20,
                                 colors){
  #label
  total<-heatmapPlotData %>%group_by(conceptName) %>% mutate(sum = sum(n)) %>% select (conceptName,sum)
  total<-unique(total)
  total$label<-paste0(total$conceptName,' \n','(n = ',total$sum,')')
  
  heatmapPlotDataN <- as_tibble(heatmapPlotData) %>% mutate(ratioLabel = paste0(ratio,'\n','(n = ',n,')')) %>%  select(cycle, conceptName, ratioLabel)%>% subset(cycle <=maximumCycleNumber)
  plotDataN <- tidyr::spread(heatmapPlotDataN, cycle, ratioLabel)
  plotDataN[is.na(plotDataN)] <- 0
  plotDataN$conceptName <- NULL
  #data pre-processing
  heatmapPlotData <- as_tibble(heatmapPlotData) %>% select(cycle, conceptName, ratio) %>% subset(cycle <=maximumCycleNumber) 
  class(heatmapPlotData$ratio) = "dbl"
  plotData <- tidyr::spread(heatmapPlotData, cycle, ratio)
  sort.order <- order(plotDataN$"1")
  

plotData <- left_join(plotData,total,by = c("conceptName"="conceptName"))
plotData <- as.data.frame(plotData)
plotData[is.na(plotData)] <- 0

row.names(plotData) <- plotData$label
plotData$conceptName <- NULL
plotData$sum <- NULL
plotData$label <- NULL
sort.order <- order(plotData$"1")
label<-as.matrix(plotDataN)
heatmap<-superheat::superheat(plotData,
                              X.text = label,
                              X.text.size = 2,
                              scale = FALSE,
                              left.label.text.size=3,
                              left.label.size = 0.3,
                              bottom.label.text.size=3,
                              bottom.label.size = .05,
                              heat.pal = RColorBrewer::brewer.pal(9, colors),
                              heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                              order.rows = sort.order,
                              title = "Trends of the Repetition in each Regimen")
}
