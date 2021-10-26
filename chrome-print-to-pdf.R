filename <- "QM2-Thema3-Post-befragen"
f2 <- "Test2"


pagedown::chrome_print(paste0("Themen/", f2, ".Rmd"),
                       output = paste0("Themen/", f2, ".pdf"),
                       wait = 10)

