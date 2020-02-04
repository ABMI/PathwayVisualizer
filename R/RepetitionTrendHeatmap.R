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
#' Heatmap
#' Trends in regimen heatmap
#' @param episodeTableFromDatabase
#' @param visualizationTargetRegimenId
#' @param heatmapInRatio
#' @param maximumCycleNumber
#' @param colors
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param plotData
#' @param episodeSourceConceptId
#' @param targetEpisodeConceptId
#' @keywords heatmap
#' @return repitition trend heatmap
#' @examples 
#' @import dplyr
#' @import superheat
#' @import ggplot2
#' @import tidyr
#' @import RColorBrewer
#' @export distributionTable
episodeTableForVisualization <- function(connectionDetails,
                                         vocaDatabaseSchema,
                                         oncologyDatabaseSchema,
                                         episodeTable){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32532) 
  '
  sql <- SqlRender::render(sql,voca_database_schema = vocaDatabaseSchema,
                           oncology_database_schema = oncologyDatabaseSchema,
                           episode_table = episodeTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)
}

distributionTable <- function(episodeTable,
                              targetEpisodeConceptId){
  episode <- episodeTable %>% filter(episodeSourceConceptId == targetEpisodeConceptId)
  maxCycleNumberPerPerson<-aggregate(episode$episodeNumber,by = list(episode$personId), max)
  colnames(maxCycleNumberPerPerson) <- c('person_id','Cycle_num')
  
  # Total count
  totalCount<-length(unique(maxCycleNumberPerPerson$personId))
  
  # Count the number of patients in the value of each cycle number
  countCycle<-as.data.frame(maxCycleNumberPerPerson %>% group_by(Cycle_num) %>% summarise(n = n()))
  countCycle$'%'<-round(prop.table(table(maxCycleNumberPerPerson$Cycle_num))*100, digits = 1)
  sum<- sum(countCycle$n)
  sumName<- paste0('N','(','total=',sum,')')
  countCycle$conceptName <- unique(episode$conceptName)
  names(countCycle) <- c('Treatment cycle',sumName,'%','conceptName')
  return(countCycle)}

#' @export
regimenHeatmap<-function(episodeTableFromDatabase,
                         visualizationTargetRegimenId = NULL,
                         heatmapInRatio = TRUE,
                         maximumCycleNumber = NULL){
  
  if(is.null(visualizationTargetRegimenId)){visualizationTargetRegimenId<-unique(episodeTableFromDatabase$episodeSourceConceptId)}
  
  totalDistribution <-data.table::rbindlist(
    lapply(visualizationTargetRegimenId,function(episodeSourceConceptId){
      targetRegimenDistributionTable<-distributionTable(episodeTable=episodeTableFromDatabase,
                                                        targetEpisodeConceptId=episodeSourceConceptId)
      names(targetRegimenDistributionTable) <- c('cycle','n','ratio','conceptName')
      return(targetRegimenDistributionTable)})
  )
  if(!is.null(maximumCycleNumber)){
    totalDistribution <- subset(totalDistribution,cycle <= maximumCycleNumber)
  }
  
  if(heatmapInRatio){
    totalDistribution <- as_tibble(totalDistribution) %>% select(cycle, conceptName, ratio)
    class(totalDistribution$ratio) = "dbl"
    plotData <- tidyr::spread(totalDistribution, cycle, ratio)
  }else{totalDistribution <- as_tibble(totalDistribution) %>% select(cycle, conceptName, n)
  class(totalDistribution$n) = "dbl"
  plotData <- tidyr::spread(totalDistribution, cycle, n)}
  
  # 
  plotData <- as.data.frame(plotData)
  plotData[is.na(plotData)] <- 0
  row.names(plotData) <- plotData$conceptName
  plotData$conceptName <- NULL
  return(plotData)
}
#'@export generateHeatmap
generateHeatmap <- function(connectionDetails,
                            vocaDatabaseSchema,
                            oncologyDatabaseSchema,
                            episodeTable,
                            visualizationTargetRegimenId = NULL,
                            heatmapInRatio = TRUE,
                            maximumCycleNumber = NULL){
  episodeTableFromDatabase<- episodeTableForVisualization(connectionDetails,
                                                          vocaDatabaseSchema,
                                                          oncologyDatabaseSchema,
                                                          episodeTable)
  
  plotData<-regimenHeatmap(episodeTableFromDatabase,
                 visualizationTargetRegimenId,
                 heatmapInRatio,
                 maximumCycleNumber)
return(plotData)
}

#'@export repetitionTrendHeatmap
repetitionTrendHeatmap<-function(plotData,colors){
  sort.order <- order(plotData$"1")
  heatmap<-superheat::superheat(plotData,
                              scale = FALSE,
                              left.label.text.size=3,
                              left.label.size = 0.3,
                              bottom.label.text.size=3,
                              bottom.label.size = .05,
                              heat.pal = colors,
                              heat.pal.values = c(seq(0,0.3,length.out = 8),1),
                              order.rows = sort.order,
                              title = "Repeated cycle number in each regimen")}