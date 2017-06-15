getSubjectsAndComponentsInCluster <- function(clusterID, scFilename) {
    subjectsAndComponents <- read.table(scFilename, colClasses=c(subjectName="character", component="numeric"), col.names=c("subjectName", "component"))
    return(subjectsAndComponents) 
}
