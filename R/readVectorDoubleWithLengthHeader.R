readVectorDoubleWithLengthHeader <- function(filename) {
    con <- file(filename, open="rb")
    n <- readBin(con=con, what=double(), n=1)
    vector <- readBin(con=con, what=double(), n=n)
    close(con)
    return(vector)
}
