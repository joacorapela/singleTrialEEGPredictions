printPropSignModelsCorrelationsTabular <- 
 function(sortvar, clustersIDs, modalities, modalitiesLabels, 
                   conditions, conditionsLabels,
                   filterByPropSigFunc, 
                   largePropColor, 
                   data, con) {

    printTabularHeader <- function(conditionsLabels, con) {
        writeLines("\\begin{tabular}{|c|c|c c|c c|}\\hline", con) 

        line <- "\\multicolumn{1}{|c}{Cluster} & \\multicolumn{1}{|c|}{Standard} "
        if(length(conditionsLabels)>1) {
            for(i in 1:(length(conditionsLabels)-1)) {
                line <- sprintf("%s & \\multicolumn{2}{c|}{%s} ", line, 
                                conditionsLabels[i])
            }
        }
        line <- sprintf("%s & \\multicolumn{2}{c|}{%s}\\\\",
                        line, conditionsLabels[length(conditionsLabels)])
        writeLines(line, con)

        line <- "\\multicolumn{1}{|c}{} & \\multicolumn{1}{|c|}{Modality} "
        if(length(conditionsLabels)>1) {
            for(i in 1:(length(conditionsLabels)-1)) {
                line <- sprintf("%s & \\multicolumn{1}{c}{n} & \\multicolumn{1}{c|}{\\%%}", line)
            }
        }
        line <- sprintf("%s & \\multicolumn{1}{c}{n} & \\multicolumn{1}{c|}{\\%%}\\\\ \\hline \\hline",
                        line)
        writeLines(line, con)
    }
    printTabularFooter <- function(con) {
        writeLines("\\end{tabular}", con)
    }
    printPropSignModelsCorrelationsClustersRows <- 
     function(sortvar, clustersIDs, modalities, modalitiesLabels, conditions,
               filterByPropSigFunc, largePropColor, data, con) {
        for(i in 1:length(clustersIDs)) {
            printPropSignModelsCorrelationsClusterRows(
             sortvar=sortvar,
             clusterID=clustersIDs[i],
             modalities=modalities,
             modalitiesLabels=modalitiesLabels,
             conditions=conditions,
             filterByPropSigFunc=filterByPropSigFunc, 
             largePropColor=largePropColor,
             data=data, con=con)
        }
    }
    printPropSignModelsCorrelationsClusterRows <- 
     function(sortvar, clusterID, modalities, modalitiesLabels, conditions,
                       filterByPropSigFunc, 
                       largePropColor, 
                       data,
                       con) {
        show(sprintf("Processing clusterID=%02d", clusterID))
        printPropSignModelsCorrelationsModalityRow(
         sortvar=sortvar,
         clusterID=clusterID,
         clusterLabel=sprintf("%02d", clusterID),
         modality=modalities[1],
         modalityLabel=modalitiesLabels[1],
         conditions=conditions,
         filterByPropSigFunc=filterByPropSigFunc, 
         largePropColor=largePropColor,
         data=data,
         con=con)
        if(length(modalities)>1) {
            for(i in 2:length(modalities)) {
                printPropSignModelsCorrelationsModalityRow(
                 sortvar=sortvar,
                 clusterID=clusterID,
                 clusterLabel="",
                 modality=modalities[i],
                 modalityLabel=modalitiesLabels[i],
                 conditions=conditions,
                 filterByPropSigFunc=filterByPropSigFunc, 
                 largePropColor=largePropColor,
                 data=data,
                 con=con)
            }
        }
        writeLines("\\hline", con)
    }
    printPropSignModelsCorrelationsModalityRow <- 
     function(sortvar, clusterID, clusterLabel, modality, modalityLabel, 
                       conditions, 
                       filterByPropSigFunc, 
                       largePropColor, 
                       data,
                       con) {
        line <- sprintf("%s & %s", clusterLabel, modalityLabel)
        for(i in 1:length(conditions)) {
            dfRow <- which(data$modality==modality &
                            data$clusterID==clusterID &
                            data$condition==conditions[i])
            propInfo <- list(nSignModelCors=data[dfRow,"nSignModelCors"],
                              pSignModelCors=data[dfRow, "pSignModelCors"])
            conditionText <- 
             getConditionStatsDesc(propInfo=propInfo,
                                    filterByPropSigFunc=filterByPropSigFunc,
                                    largePropColor=largePropColor)
            line <- sprintf("%s & %s", line, conditionText)
        }
        line <- sprintf("%s \\\\", line)
        writeLines(line, con)
    }
    
    getConditionStatsDesc <- function(propInfo, filterByPropSigFunc,
                                                largePropColor) {
        filteredByPropSig <- filterByPropSigFunc(propInfo=propInfo)
        if(filteredByPropSig) {
            desc <- 
             sprintf("\\cellcolor{%s}{%02d} & \\cellcolor{%s}{%.02f\\%%}", 
                     largePropColor, propInfo$nSignModelCors, 
                     largePropColor, propInfo$pSignModelCors)
        } else {
            desc <- sprintf("%02d & %.02f\\%%", 
                            propInfo$nSignModelCors, 
                            propInfo$pSignModelCors)
        }
        return(desc)
    }

    printTabularHeader(conditionsLabels=conditionsLabels, con=con)
    printPropSignModelsCorrelationsClustersRows(
     sortvar=sortvar, clustersIDs=clustersIDs, 
     modalities=modalities, modalitiesLabels=modalitiesLabels, 
     conditions=conditions,
     filterByPropSigFunc=filterByPropSigFunc, 
     largePropColor=largePropColor,
     data=data, con=con)
    printTabularFooter(con=con)
}
