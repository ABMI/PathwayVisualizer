#' @export
TargetCohortEpisode <- function(connectionDetails,
                     errorReportFile = file.path(getwd(),"errorReport.txt"),
                     oracleTempSchema = NULL,
                     oncologyDatabaseSchema,
                     cohortDatabaseSchema,
                     episodeTable,
                     cohortTable,
                     cohortDefinitionId,
                     ...){
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "TargetCohortEpisode.sql",
                                           packageName = "PathwayVisualizer",
                                           dbms = attr(connection,"dbms"),
                                           oracleTempSchema = oracleTempSchema,
                                           oncology_database_schema = oncologyDatabaseSchema,
                                           result_database_schema = cohortDatabaseSchema,
                                           episode_table = episodeTable,
                                           cohort_table = cohortTable,
                                           cohort_definition_id = cohortDefinitionId)
  queryResult <- DatabaseConnector::querySql(connection = connection, sql = sql, errorReportFile = errorReportFile)
  colnames(queryResult) <- SqlRender::snakeCaseToCamelCase(colnames(queryResult))
  return(queryResult)
  DatabaseConnector::disconnect(connection)
}
