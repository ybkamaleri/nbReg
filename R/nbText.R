##' Lager tekst til figur
##'
##' Denne funksjonen skal lage tekst til figur basert fra datasettet som allerede er filtret
##'
##' @inheritParams nbClean
##' @inheritParams nbFilter
##' @param fdata Filtrerings parametre
##' @return Rensert datasett og tekst til figur
##' 
##' @export


nbText <- function(data=NULL, fdata = NULL, minAlder = NULL, maxAlder = NULL,
                   datoFra = NULL, datoTil = NULL,
                   kjonn = NULL, dbType = NULL, dataValg = NULL) {

    if (is.null(data)) {data <- nbClean()}
    nData <- dim(data)[1]

     
    ## Ifelse %||%
    "%||%" <- function(a,b) if (!is.null(a)) a else b

    ## Omdefinere variabel
    minAlder <- minAlder %||% MinAlder
    maxAlder <- maxAlder %||% MaxAlder
    datoFra <- datoFra %||% DatoFra
    datoTil <- datoTil %||% DatoTil
    kjonn <- kjonn %||% Kjonn
    dbType <- dbType %||% DBType
    dataValg <- dataValg %||% DataValg
    
    ## Hente filtrering 
    if (is.null(fdata)) {utData <- nbFilter()}
    alderVec <- utData$alderVec
    kjonnVec <- utData$kjonnVec
    datoVec <- utData$datoVec
    regVec <- utData$regVec
    diaVec <- utData$diaVec
    fileSelect <- utData$fileSelect

    
    ## Gjennomføre filtrering
    dataFinal <- dplyr::filter_(.data = data, .dots = fileSelect )

    ## ## For scale-x values
    minX <- as.integer(if (min(dataFinal$Alder, na.rm = T) > minAlder) {min(dataFinal$Alder, na.rm = T)} else {minAlder})
    maxX <- as.integer(if (max(dataFinal$Alder, na.rm = T) < maxAlder) {max(dataFinal$Alder, na.rm = T)} else {maxAlder})
    
    ## Figurtekst
    figTxt <- c(paste0('Data', if (dataValg %in% 1:4){paste0(': ', c('Førstegangsregistrering',
                                                                     'Årskontroll',
                                                                     'Poliklinisk',
                                                                     'Alle type kontroller')[dataValg])}),
                if ((minAlder>0) | (maxAlder<100))
                {paste0('Pasienter fra ', minAlder, ' til ', maxAlder, ' år ')},
                
                if (kjonn %in% 1:2)
                {paste0('Kjønn: ', c('Gutter', 'Jenter')[kjonn])},

                if (dbType %in% 1:2)
                {paste0('Diabetes: ', c('Type 1', 'Alle typer')[dbType])},
                
                if (nData>0)
                {paste0('Periode: ', if (min(dataFinal$innYear, na.rm = T) > datoFra) {min(dataFinal$innYear, na.rm = T)
                                     } else {datoFra}, ' til ',
                        if (max(dataFinal$innYear, na.rm = T) < datoTil) {max(dataFinal$innYear, na.rm = T)
                        } else {datoTil})}        
                )

    utData <- list(data = dataFinal, figTxt = figTxt, minX = minX, maxX = maxX)
    return(invisible(utData))
}

