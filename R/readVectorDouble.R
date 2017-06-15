readVectorDouble <- function(filename, n) {
    con <- file(filename, open="rb")
    vector <- readBin(con=con, what=double(), n=n)
    close(con)
    return(vector)
}
