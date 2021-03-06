##' Konvertering datasettet til R format
##'
##' Denne funksjon skal konvertere datafil fra SPSS eller CSV format
##' til en format som R kan behandle
##'
##' @param filnavn Filnavn hentet fra register database
##' @return Et dataset i R data.frame format
##'
##' @export


nbFile <- function(filnavn = NULL) {

    ## Hente fil og konvertert til R data.frame
    if (is.null(filnavn)) { filnavn = Filnavn }
    filType <- tools::file_ext(filnavn)

    ## SPSS file
    if (filType == "sav") {
        dataFile <- foreign::read.spss(filnavn, to.data.frame = TRUE)
    }

    ## DAT file - tab-delimited
    if (filType == "dat") {
        dataFile <- read.table(filnavn,
                               header = TRUE,
                               encoding = 'latin1',
                               stringsAsFactors = TRUE,
                               na.strings = "EMPTY")}

    ## CSV file
    if (filType == "csv") {
        dataFile <- read.csv(filnavn,
                            header = TRUE,
                            encoding = 'latin1',
                            sep=";",
                            strip.white = TRUE,
                            stringsAsFactors = TRUE,
                            na.strings = "EMPTY") #indicate empty as NA
    }

return(invisible(dataFile))

}
