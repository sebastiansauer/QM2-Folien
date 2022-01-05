thema3 <- "QM2-Thema3-Post-befragen"
thema4 <- "QM2-Thema4-Gaussmodelle"
thema5 <- "QM2-Thema5-Lineare-Modelle"
thema6 <- "QM2-Thema6-Deskriptive-Forschungsfragen"
thema8 <- "QM2-Thema8-Kausalanalyse"
thema9 <- "QM2-Thema9-Binaere-AV"
thema10 <- "QM2-Thema10-Abschluss"
f2 <- "Test2"



sourcefile <- paste0("Themen/", thema10, ".Rmd")
outputfile <- paste0("Themen/", thema10, ".pdf")


pagedown::chrome_print(sourcefile,
                       output = outputfile,
                       wait = 30,
                       timeout = 120)

