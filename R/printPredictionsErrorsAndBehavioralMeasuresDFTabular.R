
printPredictionsErrorsAndBehavioralMeasuresDFTabular <- 
 function(df, printDFRowFunc, 
              printTabularHeaderFunc,
              printTabularFooterFunc, 
              printSubjectSeparatorFunc,
              printFilename) {
    con <- file(printFilename, open="wt")
    printTabularHeaderFunc(con=con)
    printPredictionsErrorsAndBehavioralMeasuresTabularRows(
     df=df, 
     printDFRowFunc=printDFRowFunc,
     printSubjectSeparatorFunc=printSubjectSeparatorFunc,
     con=con)
    printTabularFooterFunc(con=con)
    close(con)
}

printPredictionsErrorsAndBehavioralMeasuresTabularRows <- 
 function(df, printDFRowFunc, printSubjectSeparatorFunc, con) {
    for(i in 1:nrow(df)) {
        printDFRowFunc(df[i, ], con=con)
        if((i==nrow(df) || df[i, "subjectName"]!=df[i+1, "subjectName"])) {
            printSubjectSeparatorFunc(con)
        }
    }
}
