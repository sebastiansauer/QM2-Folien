filename <- "QM2-Thema3-Post-befragen"


pagedown::chrome_print(paste0("Themen/", filename, ".Rmd",
                       output="Themen/", filename, ".pdf",
                       wait = 10)
