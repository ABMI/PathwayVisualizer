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

# Main

#' @import dplyr
#' @export
executeVisualization <- function(connectionDetails,
                                 oracleTempSchema,
                                 cohortDatabaseSchema,
                                 cdmDatabaseSchema,
                                 oncologyDatabaseSchema,
                                 vocaDatabaseSchema,
                                 cohortTable,
                                 episodeTable,
                                 conditionCohortIds = NULL,
                                 outputFolderPath,
                                 outputFileTitle,
                                 combinationWindow = 5,
                                 maximumPathLength = 3,
                                 minimumCellCount = 5,
                                 targetCohortIds,
                                 surgeryCohortIds,
                                 adverseCohortIds,
                                 visualize = TRUE,
                                 cohortTableCreation = FALSE,
                                 episodeCohortGeneration = FALSE
){

  # Create cohort table
  if(cohortTableCreation){

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    createCohortTable(connection,
                      oracleTempSchema,
                      cohortDatabaseSchema,
                      cohortTable)
    DatabaseConnector::disconnect(connection)

  }

  # Generate episode cohort
  if(episodeCohortGeneration){

    cohortDescription <- cohortDescription() %>%
      subset(cohortDefinitionId %in% targetCohortIds)

    for(i in 1:length(targetCohortIds)){

      conceptIdSet <- cohortDescription$conceptId[i]
      targetCohortId <- cohortDescription$cohortDefinitionId[i]
      generateEpisodeCohort(connectionDetails,
                            oracleTempSchema,
                            cdmDatabaseSchema,
                            oncologyDatabaseSchema,
                            vocaDatabaseSchema,
                            cohortDatabaseSchema,
                            cohortTable,
                            episodeTable,
                            includeConceptIdSetDescendant = F,
                            collapseGapSize = 0,
                            conceptIdSet,
                            targetCohortId)
    }
  }

  # Load cohort descriptions
  cohortDescript <- cohortDescription()

  # Cohort Records
  cohortRecords <- loadCohort(connectionDetails,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetCohortIds)

  numberedCohort <- cohortNumbering(connectionDetails,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    conditionCohortIds,
                                    cohortRecords,
                                    cohortDescript,
                                    identicalSeriesCriteria = 60)

  ParallelLogger::logInfo("Extracting raw data for plot")

  # Output folder generate
  if (!file.exists(outputFolderPath)){
    dir.create(outputFolderPath, recursive = TRUE)
  }

  # Extract raw data for plot

  # 1. Usage pattern graph (PlotRaw_UsagePattern.R)
  p1_data <- plotRaw_1(numberedCohort,
                       cohortDescript,
                       outputFileTitle,
                       outputFolderPath)

  # 2. Treatment Iteration heatmap (PlotRaw_CycleHeatmap.R)
  p2_data <- plotRaw_2(targetCohortIds,
                       numberedCohort,
                       outputFileTitle,
                       outputFolderPath)

  # 3. Treatment Pathway - including table (PlotRaw_TreatmentPathway.R)
  p3_data <- plotRaw_3(connectionDetails,
                       cohortDatabaseSchema,
                       cohortTable,
                       eventCohortIds = surgeryCohortIds,
                       numberedCohort,
                       cohortDescript,
                       combinationWindow,
                       maximumPathLength,
                       minimumCellCount,
                       outputFileTitle,
                       outputFolderPath)

  # 4. Event incidence in each cycle (PlotRaw_AdverseOnsetIncidence.R)
  p4_data <- plotRaw_4(connectionDetails,
                       cohortDatabaseSchema,
                       cohortTable,
                       numberedCohort,
                       cohortDescript,
                       eventCohortIds = adverseCohortIds,
                       restrictInitialTreatment = T,
                       restrictInitialEvent = T,
                       minimumCellCount,
                       outputFileTitle,
                       outputFolderPath)

  # 5. Event onset timing (PlotRaw_AdverseOnset.R)
  p5_data <- plotRaw_5(connectionDetails,
                       cohortDatabaseSchema,
                       cohortTable,
                       numberedCohort,
                       cohortDescript,
                       eventCohortIds = adverseCohortIds,
                       treatmentEffectDates = 3,
                       observationDate = 60,
                       outputFileTitle,
                       outputFolderPath)

  plots <- Visualization(p1_data,
                         p2_data,
                         p3_data,
                         p4_data,
                         p5_data,
                         outputFileTitle,
                         outputFolderPath,
                         maximumPathLength,
                         minimumCellCount,
                         visualize)

  return(if(visualize){plots})
}
