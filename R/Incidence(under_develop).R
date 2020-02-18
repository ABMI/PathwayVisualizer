# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of treatmentCycleExtraction
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
#' NeutrophilGraph
#' Visualization tool for episode table.
#' @param connectionDetails
#' @param vocaDatabaseSchema
#' @param oncologyDatabaseSchema
#' @param episodeTable
#' @param cohortTable
#' @param neutrophilTargetRegimen
#' @param topNregimen
#' @param neutropeniaSeperationWithRatio
#' @param neutrophilCohortId
#' @keywords neutrophil,visualization
#' @return Graph or data for neutrophil analysis
#' @examples
#' @import dplyr
#' @import tidyr
#' @export
treatmentLineFromEpisode <- function(connectionDetails,
                                     vocaDatabaseSchema,
                                     oncologyDatabaseSchema,
                                     episodeTable){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- 'select episode.*,concept.concept_name from @oncology_database_schema.@episode_table episode
  left join @voca_database_schema.concept concept
  on episode.episode_source_concept_id = concept.concept_id
  where episode_concept_id in (32531)
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

#' @export
neutropenia<-function(connectionDetails,
                      cohortDatabaseSchema,
                      cohortTable,
                      neutrophilCohortId){
  connection <- DatabaseConnector::connect(connectionDetails)
  sql <- "select distinct * from @cohort_database_schema.@cohort where cohort_definition_id = @cohort_definition_id order by subject_id,cohort_start_date"
  sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort= cohortTable,
                           cohort_definition_id = neutrophilCohortId)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  result <- DatabaseConnector::querySql(connection, sql)
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  DatabaseConnector::disconnect(connection)
  return(result)}

#' @export extractDataNeutrophilAnalysis
extractDataNeutrophilAnalysis<-function(connectionDetails,
                                        vocaDatabaseSchema,
                                        oncologyDatabaseSchema,
                                        cohortDatabaseSchema,
                                        episodeTable,
                                        cohortTable,
                                        neutrophilTargetRegimen,
                                        topNregimen,
                                        neutrophilCohortId){
  treatmentLineData<-treatmentLineFromEpisode(connectionDetails,
                                              vocaDatabaseSchema,
                                              oncologyDatabaseSchema,
                                              episodeTable)
  episodeTable<-episodeTableForVisualization(connectionDetails,
                                             vocaDatabaseSchema,
                                             oncologyDatabaseSchema,
                                             episodeTable)
  neutropeniaData<-neutropenia(connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable,
                               neutrophilCohortId)

  neutropeniaData$conceptName <- "ANCUnder2000"
  neutropeniaData$episodeNumPadd <- 0
  names(neutropeniaData) <- c('episodeSourceConceptId','personId','episodeStartDatetime','episodeEndDatetime','conceptName','episodeNumber')
  neutropeniaData$episodeSourceConceptId <- 0

  treatmentLineData <-treatmentLineData %>% subset(episodeSourceConceptId %in% neutrophilTargetRegimen)

  firstLineIndex<-treatmentLineData %>% group_by(personId) %>% arrange(personId,episodeStartDatetime) %>% mutate(rown = row_number()) %>% subset(rown == 1) %>% ungroup()%>% select(episodeId)

  firstLineCycle<-episodeTable %>% subset(episodeParentId %in% firstLineIndex$episodeId) %>% select(episodeSourceConceptId,personId,episodeStartDatetime,episodeEndDatetime,conceptName,episodeNumber)

  neutropeniaAfterChemo<-rbind(firstLineCycle,neutropeniaData) %>% group_by(personId)%>% arrange(personId,episodeStartDatetime) %>% mutate(lagReg = lag(episodeNumber)) %>% subset(is.na(lagReg)==FALSE & lagReg !=0) %>% subset(episodeSourceConceptId == 0)%>% arrange(personId,episodeStartDatetime) %>% mutate(n=row_number()) %>% subset(n == 1)%>% select(episodeSourceConceptId,personId,episodeStartDatetime,episodeEndDatetime,conceptName,episodeNumber,lagReg)%>% ungroup()

  patientsNumberInFirstChemo<-firstLineCycle %>% select(conceptName,personId)
  patientsNumberInFirstChemo<-unique(patientsNumberInFirstChemo)%>% group_by(conceptName)%>% summarise(n=n()) %>% filter(rank(desc(n))<=topNregimen)

  firstLineCycle$lagReg <- 0

  personPerCycle<-firstLineCycle %>% group_by(personId) %>% mutate(cycle = row_number(episodeNumber)) %>% select(personId,conceptName,cycle) %>% ungroup %>% group_by(conceptName,cycle) %>% summarise(total=n())

  chemoAndNeutropenia<-rbind(neutropeniaAfterChemo,firstLineCycle) %>% arrange(personId,episodeStartDatetime,desc(episodeSourceConceptId))%>% group_by(personId)%>%  mutate(lagConceptName = lag(conceptName)) %>% ungroup()

  neutropeniaSeperation<-chemoAndNeutropenia %>% subset(episodeNumber == 0) %>% select(personId,lagReg,lagConceptName) %>% group_by(lagReg,lagConceptName) %>% summarise(n=n()) %>% subset(lagConceptName %in% patientsNumberInFirstChemo$conceptName) %>% ungroup()

  neutropeniaSeperationWithTotal<-merge(personPerCycle,neutropeniaSeperation,by.x=c("cycle","conceptName"),by.y=c("lagReg","lagConceptName"),all=T) %>% arrange(conceptName,cycle)

  neutropeniaSeperationWithTotal[is.na(neutropeniaSeperationWithTotal)] <- 0

  neutropeniaSeperationWithRatio<-neutropeniaSeperationWithTotal %>% subset(is.na(total)==FALSE)%>% mutate(ratio= (n/total*100)) %>% subset(conceptName %in%patientsNumberInFirstChemo$conceptName)%>% select(cycle,conceptName,ratio) %>% arrange(conceptName,cycle)
  names(neutropeniaSeperationWithRatio) <- c("cycle","Regimen","ratio")
  max(neutropeniaSeperationWithRatio$cycle)
  return(neutropeniaSeperationWithRatio)}

#'@export plotNeutrophil
plotNeutrophil<-function(neutropeniaSeperationWithRatio){
  ggplot(neutropeniaSeperationWithRatio, aes(x=cycle, y= ratio, color=Regimen)) + geom_line(size=1)+
    geom_point()+
    scale_x_continuous(limits = c(1,16),
                       breaks = c(seq(1:16)),
                       label = c(seq(1:16))) +labs(fill="Regimen") +
    labs(title = expression(Absolute~neutrophil~count<2.0%*%10^9/L~Timing),
         subtitle = "1 - 16 cycle",
         caption = "*ratio : the number of ANC < 2000 patients at each cycle of the regimen / the number of patients treated each regimen as first-line therapy",
         y = "*ratio (%)",
         x = "cycle (n)") +
    scale_y_continuous(limits = c(0, 100
    )) + theme_minimal()+scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                                     "#999999", "#0072B2", "#D55E00", "#CC79A7"))}
######################################################
##Incidence_under_development##



resultDatabaseSchema <-  'hkocdm'
cohortTable <- 'cohort_sample'
targetCohortIds <- c(seq(from = 36, to = 43))
identicalSeriesCriteria <- 60
conditionCohortIds <- NULL
####

####
# loading libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
colorList<-c("#00AFBB", "#E7B800", "#FC4E07")
set.seed(1)
randomColorNum<-sample(1:3,3)
selectedColor<-unlist(lapply(randomColorNum,function(x){colorList[x]}))
# creating data sample
set.seed(10)
cohorts <- data.frame(cohort = paste('cohort', formatC(c(1:36), width=2, format='d', flag='0'), sep = '_'),
                      Y_00 = sample(c(1300:1500), 36, replace = TRUE),
                      Y_01 = c(sample(c(800:1000), 36, replace = TRUE)),
                      Y_02 = c(sample(c(600:800), 24, replace = TRUE), rep(NA, 12)),
                      Y_03 = c(sample(c(400:500), 12, replace = TRUE), rep(NA, 24)))
# simulating seasonality (Black Friday)
cohorts[c(11, 23, 35), 2] <- as.integer(cohorts[c(11, 23, 35), 2] * 1.25)
cohorts[c(11, 23, 35), 3] <- as.integer(cohorts[c(11, 23, 35), 3] * 1.10)
cohorts[c(11, 23, 35), 4] <- as.integer(cohorts[c(11, 23, 35), 4] * 1.07)

# calculating retention rate and preparing data for plotting
df_plot <- reshape2::melt(cohorts, id.vars = 'cohort', value.name = 'number', variable.name = 'year_of_LT')

df_plot <- df_plot %>%
  group_by(cohort) %>%
  arrange(year_of_LT) %>%
  mutate(number_prev_year = lag(number),
         number_Y_00 = number[which(year_of_LT == 'Y_00')]) %>%
  ungroup() %>%
  mutate(ret_rate_prev_year = number / number_prev_year,
         ret_rate = number / number_Y_00,
         year_cohort = paste(year_of_LT, cohort, sep = '-')) %>% subset(year_of_LT %in% c('Y_01','Y_02','Y_03'))

##### The first way for plotting cycle plot via scaling
# calculating the coefficient for scaling 2nd axis
k <- max(df_plot$number_prev_year[df_plot$year_of_LT == 'Y_01'] * 1.15) / min(df_plot$ret_rate[df_plot$year_of_LT == 'Y_01'])
# calculating the coefficient for scaling 2nd axis

# retention rate cycle plot
ggplot(na.omit(df_plot), aes(x = year_cohort, y = ret_rate, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_point(size = 4) +
  geom_text(aes(label = percent(round(ret_rate, 2))),
            size = 4, hjust = 0.4, vjust = -0.6, fontface = "plain") +
  # smooth method can be changed (e.g. for "lm")
  geom_smooth(size = 2.5, method = 'loess', aes(fill = year_of_LT)) +
  geom_bar(aes(y = number_prev_year / k, fill = year_of_LT), alpha = 0.2, stat = 'identity') +
  geom_bar(aes(y = number / k, fill = year_of_LT), alpha = 0.6, stat = 'identity') +
  geom_text(aes(y = 0, label = cohort), angle = 90, size = 4, hjust = -0.05, vjust = 0.4) +
  geom_text(aes(y = number_prev_year / k, label = number_prev_year),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = number / k, label = number),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  scale_fill_manual(values = selectedColor) +
  scale_color_manual(values = selectedColor) +
  theme(legend.position='right',
        plot.title = element_text(size=20, face="bold", vjust=2),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Iteration Number of treatment', y = 'Number of Incidence / Incidence Rate') +
  ggtitle("Event Incidence Rate - Cycle plot")

##### The second way for plotting cycle plot via multi-plotting
# plot #1 - Retention rate
p1 <- ggplot(na.omit(df_plot), aes(x = year_cohort, y = ret_rate, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_point(size = 4) +
  geom_text(aes(label = percent(round(ret_rate, 2))),
            size = 4, hjust = 0.4, vjust = -0.6, fontface = "plain") +
  geom_smooth(size = 2.5, method = 'loess', color = 'darkred', aes(fill = year_of_LT)) +
  theme(legend.position='none',
        plot.title = element_text(size=20, face="bold", vjust=2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18, face="bold"),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = 'Retention Rate') +
  ggtitle("Customer Retention Rate - Cycle plot")

# plot #2 - number of customers
p2 <- ggplot(na.omit(df_plot), aes(x = year_cohort, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_bar(aes(y = number_prev_year, fill = year_of_LT), alpha = 0.2, stat = 'identity') +
  geom_bar(aes(y = number, fill = year_of_LT), alpha = 0.6, stat = 'identity') +
  geom_text(aes(y = number_prev_year, label = number_prev_year),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = number, label = number),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = 0, label = cohort), color = 'white', angle = 90, size = 4, hjust = -0.05, vjust = 0.4) +
  theme(legend.position='none',
        plot.title = element_text(size=20, face="bold", vjust=2),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, max(df_plot$number_Y_00 * 1.1))) +
  labs(x = 'Year of Lifetime by Cohorts', y = 'Number of Customers')

# multiplot
grid.arrange(p1, p2, ncol = 1)
# retention rate bubble chart
ggplot(na.omit(df_plot), aes(x = cohort, y = ret_rate, group = cohort, color = year_of_LT)) +
  theme_bw() +
  scale_size(range = c(15, 40)) +
  geom_line(size = 2, alpha = 0.3) +
  geom_point(aes(size = number_prev_year), alpha = 0.3) +
  geom_point(aes(size = number), alpha = 0.8) +
  geom_smooth(linetype = 2, size = 2, method = 'loess', aes(group = year_of_LT, fill = year_of_LT), alpha = 0.2) +
  geom_text(aes(label = paste0(number, '/', number_prev_year, '\n', percent(round(ret_rate, 2)))),
            color = 'white', size = 3, hjust = 0.5, vjust = 0.5, fontface = "plain") +
  theme(legend.position='none',
        plot.title = element_text(size=20, face="bold", vjust=2),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=10, angle=90, hjust=.5, vjust=.5, face="plain"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Cohorts', y = 'Retention Rate by Year of Lifetime') +
  ggtitle("Customer Retention Rate - Bubble chart")

# retention rate falling drops chart
ggplot(df_plot, aes(x = cohort, y = ret_rate, group = cohort, color = year_of_LT)) +
  theme_bw() +
  scale_size(range = c(15, 40)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 2, alpha = 0.3) +
  geom_point(aes(size = number), alpha = 0.8) +
  geom_text(aes(label = paste0(number, '\n', percent(round(ret_rate, 2)))),
            color = 'white', size = 3, hjust = 0.5, vjust = 0.5, fontface = "plain") +
  theme(legend.position='none',
        plot.title = element_text(size=20, face="bold", vjust=2),
        axis.title.x = element_text(size=18, face="bold"),
        axis.title.y = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=10, angle=90, hjust=.5, vjust=.5, face="plain"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Cohorts', y = 'Retention Rate by Year of Lifetime') +
  ggtitle("Customer Retention Rate - Falling Drops chart")
