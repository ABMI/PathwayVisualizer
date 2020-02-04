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
#' @export regimenInYear

regimenInYear<-function(connectionDetails,
                  vocaDatabaseSchema,
                  oncologyDatabaseSchema,
                  episodeTable,
                  targetRegimen,
                  fromYear,
                  toYear){
  treatmentLineData<-treatmentLineFromEpisode(connectionDetails,
                                              vocaDatabaseSchema,
                                              oncologyDatabaseSchema,
                                              episodeTable)
  treatmentLineData<-treatmentLineData %>% subset(episodeSourceConceptId%in%targetRegimen)
  treatmentLineData<-treatmentLineData %>% select(personId,conceptName,episodeStartDatetime)
  treatmentLineData$episodeStartDatetime<-format(as.Date(treatmentLineData$episodeStartDatetime, format="Y-%m-%d"),"%Y")
  
  treatmentLineData<-treatmentLineData %>% group_by(episodeStartDatetime,conceptName)
  treatmentLineData<-unique(treatmentLineData)
  treatmentLineData<-treatmentLineData %>% summarise(n=n()) %>%ungroup() %>%  arrange(conceptName,episodeStartDatetime) %>% subset(episodeStartDatetime <=toYear & episodeStartDatetime >=fromYear) %>% group_by(episodeStartDatetime) %>% mutate(total = sum(n)) %>% mutate(ratio = round(n/total*100,1)) %>% select(episodeStartDatetime,conceptName,ratio)
  colnames(treatmentLineData) <- c('Year','Regimen','ratio')
  h<-treatmentLineData %>% highcharter::hchart(.,type="line",hcaes(x = Year,y=ratio,group = Regimen))
  return(h)}
