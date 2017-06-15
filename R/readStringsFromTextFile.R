
readStringsFromTextFile <- function(filename) {
    con <- file(filename, open="rt")
    lines <- c()
    currentLine <- 1
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        lines[currentLine] <- line
        currentLine <- currentLine + 1
    } 
    close(con)
    return(lines)
}
