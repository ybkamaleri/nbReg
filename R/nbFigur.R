##' Valgt av sykehus og rapport
##'
##' Her skal deg gjøres valg av sykehus for sammenligne og type rapport
##'
##' @param data Datasettet
##' @param sykehus Utvalgte sykehus
##' @param rapportValg Rapport valg
##' @param yAksen Y aksen for figuren
##' @param save Filnavn for å lage figuren
##' @param format Figur format i.e "pdf", "jpg" eller "png"
##' @param path Hvor skal figuren lagres evt. hjemme område
##'
##' @import dplyr
##' @import ggplot2
##'
##' @export


nbFigur <- function(data = NULL, sykehus = NULL, rapportValg = NULL, yAksen = NULL,
                        save = NULL, format = "pdf", path = NULL) {

    if (is.null(data)) {utData <- nbSelectVar()} else {utData <- data}

    ## Hente data ut
    RegDataValg <- utData$data
    xScale      <- utData$xScale
    figT        <- utData$figT
    figTxt      <- utData$figTxt
    xLab        <- utData$xLab
    xBreaks     <- utData$xBreaks

    ## Ifelse %||%
    "%||%" <- function(a,b) if (!is.null(a)) a else b

    ## Omdefinere variablene
    sykehus     <- sykehus %||% Sykehus
    rapportValg <- rapportValg %||% RapportValg
    yAksen      <- yAksen %||% YAksen


####################################################
### Data utvalg for å sammenligne sykehus og landet
####################################################

    ## Valg av sykehuset 1:lokal 2:Resten
    samSyk <- lazyeval::interp(~f(var1 %in% sykehus, 1, 2),
                               .values = list(f = as.name("ifelse"),
                                              var1 = as.name("SykehusKode")))
    group <- "SykehusValg"
    RegDataValg <- dplyr::mutate_(.data = RegDataValg,
                                  .dots = setNames(list(samSyk), group))


################################################################
### rapportValg 1:Hele, 2:Lokal, 3:Øvrige/Lokal [RegDataValgSam]
################################################################

    ##-- Hele landet --##
    if (rapportValg == 1 & yAksen %in% 1:2) {
        RegDataValgSam <- mutate(.data = RegDataValg,
                                 SykehusValg = 1)

        ## Prosent
        RegDataPA <- RegDataValgSam %>%
            group_by(Variabel) %>%
            tally %>%
            mutate(yAksen = (100*n/sum(n)),
                   samSyk = 1)

        ##Antall
        if (yAksen == 2) {
            RegDataPA <- RegDataPA %>%
                select(Variabel, samSyk, yAksen=n)
        } else { RegDataPA = RegDataPA }
    }

    ## -- Lokal vs andre sykehus --##
    RegDataValgSam1 <- filter(RegDataValg, SykehusValg==1) #lokal
    RegDataValgSam2 <- filter(RegDataValg, SykehusValg==2) #andre

    if (rapportValg %in% 2:3 & yAksen %in% 1:2) {

        ## Prosent
        RegDataPA <- RegDataValgSam1 %>%
            group_by(Variabel) %>%
            tally %>%
            mutate(yAksen = (100*n/sum(n)),
                   samSyk = 1)
        ## Antall
        if (yAksen == 2) {
            RegDataPA <- RegDataPA %>%
                select(Variabel, samSyk, yAksen=n)
        } else { RegDataPA = RegDataPA }

        if (rapportValg == 3) { #andre sykehus
            RegDataPA2 <- RegDataValgSam2 %>%
                group_by(Variabel) %>%
                tally %>%
                mutate(yAksen = (100*n/sum(n)),
                       samSyk = 2)

            if (yAksen == 2) {
                RegDataPA2 <- RegDataPA2 %>%
                    select(Variabel, samSyk, yAksen=n)
            } else { RegDataPA2 = RegDataPA2 }
        }
    }


##########################
### Henter N for figur
##########################

    if (rapportValg %in% 2:3) {
        sykNavn <- RegDataValg$SykehusNavn[RegDataValg$SykehusKode==sykehus][1]

        N <- dim(RegDataValgSam1)[1]
        andreN <- dim(RegDataValgSam2)[1]

        sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
        sykehusAndre <- paste0("Øvrige sykehus (N = ", andreN, ") ")}


    if (rapportValg == 1) {
        N <- dim(RegDataValg)[1]
        sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
    }



##########################################
### Tekst til Figur
##########################################

    titBlank <- ""
    figTitle <- c(figT, figTxt)
    figSubT = paste(figTitle, collapse = "\n")
    txtSpace = length(figTxt)

    if (yAksen == 1) yLab="Prosent (%)"
    if (yAksen == 2) yLab="Antall pasienter"


    if (yAksen == 3) {
        maalvar = "hba"
        RegDataValg <- select(RegDataValg, -diaVarighet)
        yLab = "Gjennomsnitt HbA1c (95% CI)"
    }

    if (yAksen == 4) {
        maalvar = "diaVarighet"
        RegDataValg <- select(RegDataValg, -hba)
        yLab = "Diabetesvarighet (år)"
    }



####################
### FIGURER
####################


## ###########################
## ### Felles figur tema

    ## FigTh <- function(yL, xL, titB, figS, txtS) {
    ##     theme_bw() + ylab(yL) + xlab(xL) +
    ##     ggtitle(bquote(atop(.(titB),atop(.(figS), "")))) +
    ##     expand_limits(x = 0, y = 0) +
    ##     theme(plot.margin = unit(c(txtS,1,1,1), "lines"),
    ##           plot.title = element_text(hjust = 0, size=18),
    ##           axis.title = element_text(face = "bold", size = 12),
    ##           legend.position = 'top',
    ##           legend.text = element_text(size = 12),
    ##           legend.title = element_blank(),
    ##           axis.text = element_text(size = 11),
    ##           axis.line = element_line(size =.3, color = "#333333"),
    ##           panel.grid.major = element_line(color = "#CCCCCC",
    ##                                           linetype = 2),
    ##           panel.border = element_blank())
    ##     }

####################################

    if (xScale == 1) {


        if (yAksen==1) RegDataPA$yAksen = round(RegDataPA$yAksen, 1)
        if (yAksen==2) RegDataPA$yAksen = as.numeric(format(RegDataPA$yAksen, digits=0))


        if (yAksen %in% 1:2 & rapportValg %in% 1:2) {

            RegFig <- ggplot(RegDataPA, aes(x=Variabel, y=yAksen, fill=factor(0))) +
                geom_bar(stat = "identity") +
                scale_x_continuous(breaks = xBreaks) +
                scale_fill_manual(name="", values = "#6699CC", label=sykehusNavn) +
                geom_text(aes(label=yAksen), vjust=-0.25, colour = "black") +

                theme_light() + ylab(yLab) + xlab(xLab) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                expand_limits(x = 0, y = 0) +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())


        }


        if (yAksen %in% 1:2 & rapportValg == 3) {

            if (yAksen==1) RegDataPA$yAksen = round(RegDataPA$yAksen, 1)
            if (yAksen==2) RegDataPA$yAksen = as.numeric(format(RegDataPA$yAksen, digits=0))

            if (yAksen==1) {RegDataPA2$yAksen = round(RegDataPA2$yAksen, 1)}
            if (yAksen==2) {RegDataPA2$yAksen = as.numeric(format(RegDataPA2$yAksen, digits=0))}


            RegFig <- ggplot(NULL, aes(x=Variabel, y=yAksen)) +
                geom_bar(data = RegDataPA, aes(fill="syk1",
                                               shape="syk1"), stat = "identity") +
                geom_point(data = RegDataPA2, aes(shape= "syk2",
                                                  fill= "syk2"),
                           stat = "identity", size = 5, shape = 23) +
                scale_fill_manual(values = c("#6699CC","#000099"),
                                  labels = c("syk1" = sykehusNavn,  "syk2" = sykehusAndre)) +
                scale_x_continuous(breaks = xBreaks) +
                guides(fill = guide_legend(override.aes = list(shape = NA))) +
                theme_light() + ylab(yLab) + xlab(xLab) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                expand_limits(x = 0, y = 0) +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())



        }
    }



    ##-------------------------##
    ##-- Kategorisk variabel --##
    ##-------------------------##

    if (xScale == 2) {

        if (yAksen %in% 1:2 & rapportValg %in% 1:2) {

            if (yAksen==1) RegDataPA$yAksen=as.numeric(sprintf("%.1f", RegDataPA$yAksen))
            if (yAksen==2) RegDataPA$yAksen=as.numeric(sprintf("%.f", RegDataPA$yAksen))

            RegFig <- ggplot(RegDataPA, aes(x=Variabel, y=yAksen, fill = factor(samSyk))) +
                geom_bar(stat = "identity") +
                expand_limits(x = 0, y = 0) +
                ylab(yLab) + xlab(xLab) +
                scale_fill_manual(name="", values = "#6699CC", label=sykehusNavn) +
                geom_text(aes(label=yAksen), vjust=-0.25, colour = "black") +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +

    theme_light() +
    theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
          plot.title = element_text(hjust = 0, size=18),
          axis.title = element_text(face = "bold", size = 12),
          legend.position = 'top',
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          axis.text = element_text(size = 11),
          axis.line = element_line(size =.3, color = "#333333"),
          panel.grid.major = element_line(color = "#CCCCCC",
                                          linetype = 2),
          panel.border = element_blank())

        }


        if (yAksen %in% 1:2 & rapportValg == 3) {

            if (yAksen==1) RegDataPA$yAksen=as.numeric(sprintf("%.1f", RegDataPA$yAksen))
            if (yAksen==2) RegDataPA$yAksen=as.numeric(sprintf("%.f", RegDataPA$yAksen))

            if (yAksen==1) RegDataPA2$yAksen=as.numeric(sprintf("%.1f", RegDataPA2$yAksen))
            if (yAksen==2) RegDataPA2$yAksen=as.numeric(sprintf("%.f", RegDataPA2$yAksen))

            lengthVar <- if (length(RegDataPA$Variabel) > length(RegDataPA2$Variabel)) {
                             length(RegDataPA$Variabel) } else { length(RegDataPA2$Variabel)}


            RegFig <- ggplot(NULL, aes(x=Variabel, y=yAksen)) +
                geom_bar(data = RegDataPA, aes(fill="syk1",
                                               shape="syk1"), stat = "identity") +
                geom_point(data = RegDataPA2, aes(shape= "syk2",
                                                  fill= "syk2"),
                           stat = "identity", size = 5, shape = 23) +
                ylab(yLab) +
                xlab(xLab) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                ## scale_shape_manual(values = c(NA, 23),
                ##                    labels = c("syk1" = sykehusNavn,  "syk2" = sykehusAndre)) +
                scale_fill_manual(values = c("#6699CC", "#000099"),
                                  labels = c("syk1" = sykehusNavn,  "syk2" = sykehusAndre)) +

                guides(fill = guide_legend(override.aes = list(shape = NA))) +
                scale_x_discrete(limits = xBreaks[1:max(lengthVar)]) +
                theme_light() +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())

        }
    }



############################
###Figur for hbalc og ..
############################


    ##-------------------------##
    ##-- Kontinuell variabel --##
    ##-------------------------##

    if (xScale == 1) {

        ## -- Landet og Lokal sykehus --##
        if (yAksen %in% 3:4 & rapportValg %in% 1:2) {

            if (rapportValg == 2) {
                RegDataValg <- filter(.data=RegDataValg, SykehusValg==1)
            } else { RegDataValg }


            ## Antall n eksludert NA for legend
            if (rapportValg == 1) {
                N <- sum(complete.cases(RegDataValg))
                sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
            }

            if (rapportValg == 2) {
                sykNavn <- RegDataValg$SykehusNavn[RegDataValg$SykehusKode==sykehus][1]

                lokalRD = filter(.data=RegDataValg, SykehusValg==1)

                N = sum(complete.cases(lokalRD))

                sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
            }


            RegDataHA <- sumCI(data=RegDataValg, maalvar = maalvar,
                                   gpvars = "Variabel")

            ## Ta bort Mean når n=1
            RegDataHA$Mean[RegDataHA$N<=1] <- 0

            ## Lage dummy for legend
            RegDataHA <- mutate(.data = RegDataHA, samSyk = 1 )


            ## ## Errorbar for 1 SE
            ## RegFig <- ggplot(RegDataValg, aes(Variabel, hba)) +
            ##     stat_summary(fun.y = mean, geom = "bar", na.rm = T) +
            ##     stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = T, width=.4) +

            RegFig <- ggplot(RegDataHA, aes(x=Variabel, y=Mean)) +
                geom_bar(aes(fill=factor(samSyk)), position = position_dodge(), stat = "identity") +
                geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI), width=.3,
                              position = position_dodge(.9)) +
                ylab(yLab) + xlab(xLab) +
                scale_x_continuous(breaks = xBreaks) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                scale_fill_manual(name="", values = "#6699CC", label=sykehusNavn) +
                ## geom_text(aes(label=yAksen), vjust=-0.25, colour = "black") +
                theme_light() +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())

        }


        ## -- Lokal vs. Andre sykehus --##
        if (yAksen %in% 3:4 & rapportValg == 3 ) {


            ## Antall N lokal og andre
            if (rapportValg == 3) {

                sykNavn = RegDataValg$SykehusNavn[RegDataValg$SykehusKode==sykehus][1]

                lokalRD = filter(.data=RegDataValg, SykehusValg==1)
                andreRD = filter(.data=RegDataValg, SykehusValg==2)

                N = sum(complete.cases(lokalRD))
                andreN = sum(complete.cases(andreRD))

                sykehusNavn = paste0(sykNavn, " (N = ", N, ") ")
                sykehusAndre = paste0("Øvrige sykehus (N = ", andreN, ") ")
            }


            RegDataHA <- sumCI(data=RegDataValg, maalvar = maalvar,
                                   gpvars = c("Variabel","SykehusValg"))

            ## Ta bort Mean når n=1
            RegDataHA$Mean[RegDataHA$N<=1] <- 0

            ## ## Errorbar for 1 SE
            ## RegFig <- ggplot(RegDataValg, aes(Variabel, hba, fill=factor(SykehusValg))) +
            ##     stat_summary(fun.y = mean, geom = "bar", position = "dodge", na.rm = T) +
            ##     stat_summary(fun.data = mean_se, geom = "errorbar", na.rm = T,
            ##     position = position_dodge(width = 0.9), width=0.4) +

            ## Errorbar for CI
            RegFig <- ggplot(RegDataHA, aes(x=Variabel, y=Mean, fill=factor(SykehusValg))) +
                geom_bar(position = position_dodge(), stat = "identity") +
                geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI), width=.3,
                              position = position_dodge(.9)) +
                ylab(yLab) +
                xlab(xLab) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                ## scale_shape_manual(values = c(NA, 23),
                ##                    labels = c("syk1" = sykehusNavn,  "syk2" = sykehusAndre))
                scale_fill_manual(values = c("#6699CC","#000099"),
                                  breaks = c("1", "2"),
                                  labels = c(sykehusNavn, sykehusAndre)) +
                scale_x_continuous(breaks = xBreaks) +
                theme_light() +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())

        }
    }




    ##-------------------------##
    ##-- Kategorisk variabel --##
    ##-------------------------##

    if (xScale == 2) {


        ## -- Landet og Lokal sykehus --##
        if (yAksen %in% 3:4 & rapportValg %in% 1:2) {

            if (rapportValg == 2) {
                RegDataValg <- filter(.data=RegDataValg, SykehusValg==1)
            } else { RegDataValg }

            ## Antall n eksludert NA for legend
            if (rapportValg == 1) {
                N <- sum(complete.cases(RegDataValg))
                sykehusNavn <- paste0("Hele landet (N = ", N, ") ")
            }

            if (rapportValg == 2) {
                sykNavn <- RegDataValg$SykehusNavn[RegDataValg$SykehusKode==sykehus][1]

                lokalRD = filter(.data=RegDataValg, SykehusValg==1)

                N = sum(complete.cases(lokalRD))

                sykehusNavn <- paste0(sykNavn, " (N = ", N, ") ")
            }


            ## Aggregerte data for figur
            RegDataHA <- sumCI(data=RegDataValg, maalvar = maalvar,
                                   gpvars = "Variabel")

            ## Teller antall kategorisk for valg i x-skale
            lengthVar <- length(RegDataHA$Variabel[RegDataHA$N!=0])

            ## Ta bort n=1 siden det ikke kan beregne Mean
            RegDataHA$Mean[RegDataHA$N<=1] <- 0

            ## Lage dummy for legend
            RegDataHA <- mutate(.data = RegDataHA, samSyk = 1 )

            print(RegDataHA)


            RegFig <- ggplot(RegDataHA, aes(x=Variabel, y=Mean)) +
                geom_bar(aes(fill=factor(samSyk)), position = position_dodge(), stat = "identity") +
                geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI), width=.3,
                              position = position_dodge(.9)) +
                expand_limits(x = 0, y = 0) +
                ylab(yLab) + xlab(xLab) +
                scale_fill_manual(name="", values = "#6699CC", label=sykehusNavn) +
                scale_x_discrete(limits = xBreaks[1:max(lengthVar)]) +
                ## geom_text(aes(label=yAksen), vjust=-0.25, colour = "black") +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                theme_light() +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())

        }


        ## -- Lokal vs. Andre sykehus --##
        if (yAksen %in% 3:4 & rapportValg == 3 ) {

            RegDataHA <- sumCI(data=RegDataValg, maalvar = maalvar,
                                   gpvars = c("Variabel","SykehusValg"))

            ## Teller antall kategorisk for valg i x-skale
            lengthVar <- length(unique(RegDataHA$Variabel[RegDataHA$N!=0]))

            ## Antall N lokal og andre
            if (rapportValg == 3) {

                sykNavn = RegDataValg$SykehusNavn[RegDataValg$SykehusKode==sykehus][1]

                lokalRD = filter(.data=RegDataValg, SykehusValg==1)
                andreRD = filter(.data=RegDataValg, SykehusValg==2)

                N = sum(complete.cases(lokalRD))
                andreN = sum(complete.cases(andreRD))

                sykehusNavn = paste0(sykNavn, " (N = ", N, ") ")
                sykehusAndre = paste0("Øvrige sykehus (N = ", andreN, ") ")
            }


            ## Ta bort Mean når n=1
            RegDataHA$Mean[RegDataHA$N<=1] <- 0


            ## Errorbar for CI
            RegFig <- ggplot(RegDataHA, aes(x=Variabel, y=Mean, fill=factor(SykehusValg))) +
                geom_bar(position = position_dodge(), stat = "identity") +
                geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI), width=.3,
                              position = position_dodge(.9)) +
                ylab(yLab) +
                xlab(xLab) +
                ggtitle(bquote(atop(.(titBlank),atop(.(figSubT), "")))) +
                scale_fill_manual(values = c("#6699CC", "#000099"),
                                  labels = c(sykehusNavn, sykehusAndre)) +
                scale_x_discrete(limits = xBreaks) +
                theme_light() +
                theme(plot.margin = unit(c(txtSpace,1,1,1), "lines"),
                      plot.title = element_text(hjust = 0, size=18),
                      axis.title = element_text(face = "bold", size = 12),
                      legend.position = 'top',
                      legend.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.text = element_text(size = 11),
                      axis.line = element_line(size =.3, color = "#333333"),
                      panel.grid.major = element_line(color = "#CCCCCC",
                                                      linetype = 2),
                      panel.border = element_blank())

        }
    }

###############
### Output
###############




    if (is.null(save)) {
        print(RegFig)
        ##ggsave(Save)
        ##dev.off()
    } else {
        wdir <- getwd()
        path <-  path %||% wdir
        ggsave(filename = save, device = format)
        ##print(RegFig)
    }



}
