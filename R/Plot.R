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

#' Visualization
#' @import dplyr
#' @import highcharter
#' @import plotly
#' @import viridis
#' @import gridExtra
#' @import scales
#' @import hrbrthemes
#' @import ggplot2
#' @import data.table

# plot1_Usage_patterns
#' @export
plot1 <- function(p1_data,
                  type = 'area'){

  if(type == 'area'){
    p1 <- ggplot(p1_data, aes(x= Year,y= proportion, fill = Cohort)) +
      geom_area(alpha=0.6 , size=1) +
      labs(y="Treatment (%)") +
      theme_Publication() +
      scale_fill_Publication() +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      scale_x_continuous(breaks = unique(p1_data$Year))}

  if(type == 'line'){
    p1 <- ggplot(p1_data, aes(x= Year,y= proportion, color = Cohort)) +
      geom_line() +
      labs(y="Treatment (%)") +
      theme_Publication() +
      scale_colour_Publication() +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      scale_x_continuous(breaks = unique(p1_data$Year)) +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  }

  p1_plot <- plotly::ggplotly(p1) %>% layout(showlegend = TRUE, legend = list(font = list(size = 20)))

  return(p1_plot)
}
# plot2_cycle_heatmap
#' @export
plot2 <- function(p2_data,
                  minimumCellCount){

  total <- p2_data %>% group_by(cohortName) %>% mutate(sum = sum(n)) %>% select (cohortName,sum)
  total <- unique(total)
  total$label <- paste0(total$cohortName,' \n','(n = ',total$sum,')')
  heatmapPlotData <- p2_data %>% subset(n >= minimumCellCount)

  p2_plot <- heatmapPlotData %>% highcharter::hchart(.,type="heatmap",hcaes(x = cycle,y=cohortName,value = ratio),dataLabels = list(allowOverlap = TRUE, enabled = TRUE,format = '{point.n}<br>{point.value}%'),align ='center') %>% hc_xAxis(title = list(style = list(fontSize = 14)),max = max(heatmapPlotData$cycle), tickInterval = 1,labels = list(style = list(fontSize = 14))) %>% hc_yAxis(title = list(text = 'Regimen',style = list(fontSize = 14)),labels = list(style = list(fontSize = 14))) %>% hc_colorAxis(stops = color_stops(ceiling(max(heatmapPlotData$ratio)),c("white","blue"))) %>% hc_tooltip(pointFormat = "Regimen: {point.y} <br> Cycle: {point.x} <br> Proportion: {point.value}%")

  return(p2_plot)
}

# plot3_treatment_pathway
#' @export
plot3 <- function(p3_data){

  nodes <- p3_data$nodes
  links <- p3_data$links

  p3_plot <- networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = "source",Target = "target", Value = "value", NodeID = "name", fontSize = 20, nodeWidth = 20, LinkGroup = "group",NodeGroup = "group",sinksRight = FALSE, nodePadding = 20, fontFamily = "Times")

  return(p3_plot)
}

# plot4_Event_incidence
#' @export
plot4 <-  function(p4_data,type = 'histogram'){

  if(type == 'regression'){

    plotData <- p4_data %>% mutate(label = paste(event,'/',total))
    Percent_labeled <- plotData %>% arrange(cohortName, cycle) %>% mutate(label = paste0(event,' / ',total,' (',ifelse(event != 0,paste0(round(event/total*100,1),'%'),' - '),')')) %>% arrange(cohortName, cycle) %>% mutate(ratio = round(ratio*100,1))
    Percent_labeled <- na.omit(Percent_labeled) %>% subset(event != 0)

    p2 <- ggplot(Percent_labeled) +
      geom_point(size = 1, aes(x = cycle, y = ratio, color = cohortName, fill = cohortName)) +
      geom_smooth(size = 0.5, method = 'lm', aes(x = cycle, y = ratio, color = cohortName, fill = cohortName)) + theme_Publication() + scale_fill_Publication() + scale_colour_Publication() + facet_wrap(~cohortName,ncol = 4) + theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      ylim(0,100) + scale_x_reverse(breaks = c(1:max(Percent_labeled$cycle))) + geom_text(aes(x = cycle,y = ratio, label = label), show.legend = FALSE, vjust = 0.2,hjust = -0.05, size = 3.5, position = position_dodge(width=0.6)) + coord_flip() + xlab("Cycle") +
      ylab("")

    p4_plot <- ggplotly(p2) %>% style(textposition = 'top right')

  }

  if(type == 'histogram'){

    plotData <- p4_data %>% mutate(label = paste(event,'/',total))
    Percent_labeled <- plotData %>% arrange(cohortName, cycle) %>% mutate(label = paste0(event,' / ',total,' (',ifelse(event != 0,paste0(round(event/total*100,1),'%'),' - '),')')) %>% arrange(cohortName, cycle) %>% mutate(ratio = round(ratio*100,1))
    Percent_labeled <- na.omit(Percent_labeled) %>% subset(event != 0)

    p2 <- ggplot(Percent_labeled) +
      geom_bar(aes(x = cycle, group = cohortName,y = ratio, fill = cohortName),stat = 'identity',show.legend = FALSE) + theme_Publication() + scale_fill_Publication() + scale_colour_Publication() + facet_wrap(~cohortName,ncol = 4) + theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
      ylim(0,100) + scale_x_reverse(breaks = c(1:max(Percent_labeled$cycle))) + geom_text(aes(x = cycle,y = ratio, label = label), show.legend = FALSE, vjust = 0.2,hjust = -0.05, size = 3.5, position = position_dodge(width=0.6)) + coord_flip() + xlab("Cycle") +
      ylab("")

    p4_plot  <-ggplotly(p2) %>% style(textposition = 'middle right')

  }
  return(p4_plot)
}

# plot5_Event_Onset_date
#' @export
plot5 <- function(p5_data){

  plotdata <- as.data.frame(data.table::rbindlist(lapply(1:nrow(p5_data),function(i){cohortName <- rep(p5_data[i,]$cohortName,p5_data[i,]$n)
  dateDiff <- rep(p5_data[i,]$dateDiff,p5_data[i,]$n)
  targerRecord <- data.frame(cohortName,dateDiff)
  return(targerRecord)})))

  plotdata <- plotdata %>%
    mutate(category = ifelse(dateDiff<1,'d1',ifelse(dateDiff<=7,'d2-d8',ifelse(dateDiff<=14,'d9-d15',ifelse(dateDiff<=21,'d16-d22',ifelse(dateDiff<=29,'-d30','>d30'))))))

  plotdata$category <- factor(plotdata$category,levels = c('d1','d2-d8','d9-d15','d16-d22','-d30','>d30'))

  p <- ggplot(plotdata,aes(x=cohortName, y=dateDiff)) +
    geom_violin(size=0.2,scale = 'width') +
    ggbeeswarm::geom_quasirandom(size = 1,aes(color = category))+
    scale_color_viridis(discrete=TRUE) + theme_ipsum() +
    theme(
      legend.position= 'right',legend.title=element_blank()
    ) +
    coord_flip() +
    xlab("") +
    ylab("Time from discharge (days)")

  p5_plot <- plotly::ggplotly(p)

  return(p5_plot)

}
