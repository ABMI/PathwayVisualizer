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

#' @export
Visualization <- function(p1_data = NULL,
                          p2_data = NULL,
                          p3_data = NULL,
                          p4_data = NULL,
                          p5_data = NULL,
                          outputFileTitle,
                          outputFolderPath,
                          maximumPathLength,
                          minimumCellCount,
                          visualize){

  # 1. Usage pattern graph
  if(is.null(p1_data)){
    fileNameUsage <- paste0(outputFileTitle,'_','RegimenUsagePattern.csv')
    UsagePath <- file.path(outputFolderPath, fileNameUsage)
    p1_data <- read.csv(UsagePath,stringsAsFactors = F)
  }
  # 2. Treatment Iteration heatmap
  if(is.null(p2_data)){
    fileNameIteration<- paste0(outputFileTitle,'_','TreatmentHeatmap.csv')
    IterationPath <- file.path(outputFolderPath, fileNameIteration)
    p2_data <- read.csv(IterationPath,stringsAsFactors = F)
  }
  # 3. Treatment Pathway - including table
  if(is.null(p3_data)){
    fileNameNodes <- paste0(outputFileTitle,'_','SankeyNodes.csv')
    fileNameLinks <- paste0(outputFileTitle,'_','SankeyLinks.csv')
    nodesPath <- file.path(outputFolderPath, fileNameNodes)
    linksPath <- file.path(outputFolderPath, fileNameLinks)
    nodes <- read.csv(nodesPath,stringsAsFactors = F)
    links <- read.csv(linksPath,stringsAsFactors = F)

    p3_data <- list()

    p3_data$nodes <- nodes
    p3_data$links <- links

  }
  # 4. Event incidence in each cycle
  if(is.null(p4_data)){
    fileNameEventIncidenceInCycle <- paste0(outputFileTitle,'_','AdverseOnsetIncidence.csv')
    fileNameEventIncidenceInCyclePath <- file.path(outputFolderPath, fileNameEventIncidenceInCycle)
    p4_data <- read.csv(fileNameEventIncidenceInCyclePath,stringsAsFactors = F)
  }
  # 5. Event onset timing
  if(is.null(p5_data)){
    fileNameEventIncidenceInDates<- paste0(outputFileTitle,'_','AdverseOnset.csv')
    fileNameEventIncidenceInDatesPath <- file.path(outputFolderPath, fileNameEventIncidenceInDates)
    p5_data <- read.csv(fileNameEventIncidenceInDatesPath,stringsAsFactors = F)
  }

  pathToRmd <- system.file("rmd","Treatment_Patterns.Rmd",package = "PathwayVisualizer")

  params <- list(outputFolderPath = outputFolderPath,
                 outputFileTitle = outputFileTitle,
                 maximumPathLength = maximumPathLength,
                 minimumCellCount = minimumCellCount,
                 p1_data = p1_data,
                 p2_data = p2_data,
                 p3_data = p3_data,
                 p4_data = p4_data,
                 p5_data = p5_data)

  rmarkdown::render(pathToRmd,"flex_dashboard",output_dir = outputFolderPath,output_file = paste0(outputFileTitle,'.','html'),
                    params = params,
                    clean = TRUE)

  return(
    if(visualize){rmarkdown::run(pathToRmd, render_args = list(params = params,clean = TRUE))}
  )
}
