#'@export
patternRaw <- function(connectionDetails,
                       cohortDatabaseSchema,
                       cohortTable,
                       regimenCohortIds,
                       eventCohortIds,
                       interestCohortIds){

  regimen <- loadCohort(connectionDetails = connectionDetails,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        targetCohortIds = regimenCohortIds)

  records <- rbind(regimen,loadCohort(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTable = cohortTable,
                                      targetCohortIds = eventCohortIds) %>% subset(subjectId %in% unique(regimen$subjectId) ))

  targetCohort <- loadCohort(connectionDetails = connectionDetails,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             targetCohortIds = interestCohortIds)

  cohortRecords <- left_join(records,targetCohort,c('subjectId' = 'subjectId')) %>% subset(cohortStartDate.x >= cohortStartDate.y & cohortEndDate.x <= cohortEndDate.y) %>% select(cohortDefinitionId.x,subjectId, cohortStartDate.x,cohortEndDate.x)

  colnames(cohortRecords) <- c('cohortDefinitionId','subjectId','cohortStartDate','cohortEndDate')

  gsSubjectRecords <- cohortRecords %>% subset(subjectId %in% unique(regimen$subjectId))%>% group_split(subjectId)


  return(lapply(gsSubjectRecords,generateDate,maximumDate = maximumDate))
}


#'@export
generateDate<- function(targetRecords,maximumDate){

  # targetRecords <- gsSubjectRecords[[6]]
  arrangedRecords <- targetRecords %>% arrange(cohortStartDate)

  data <- arrangedRecords %>% mutate(startDate = cohortStartDate - arrangedRecords$cohortStartDate[1], endDate = cohortEndDate - arrangedRecords$cohortStartDate[1]) %>% subset(startDate <= maximumDate & endDate <= maximumDate) %>% select(cohortDefinitionId,subjectId,startDate,endDate)

  if(nrow(data) == 0){return(NULL)}
  df <- data.table::rbindlist(lapply(1:nrow(data),function(i,data){ newData <- data[i,] %>% mutate(dates = paste0(c(data[i,]$startDate:data[i,]$endDate),collapse = ',')) %>% select(cohortDefinitionId,subjectId,dates)
  return(newData)
  },data = data))

  return(tidyr::separate_rows(df, dates, convert = TRUE))
}
