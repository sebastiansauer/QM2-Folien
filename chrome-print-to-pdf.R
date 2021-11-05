thema3 <- "QM2-Thema3-Post-befragen"
thema4 <- "QM2-Thema4-Gaussmodelle"
thema5 <- "QM2-Thema5-Lineare-Modelle"
thema6 <- "QM2-Thema6-Deskriptive-Forschungsfragen"
f2 <- "Test2"

sourcefile <- paste0("Themen/", thema6, ".Rmd")
outputfile <- paste0("Themen/", thema6, ".pdf")


pagedown::chrome_print(sourcefile,
                       output = outputfile,
                       wait = 30,
                       timeout = 120)

