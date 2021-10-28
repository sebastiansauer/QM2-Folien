thema3 <- "QM2-Thema3-Post-befragen"
thema4 <- "QM2-Thema4-Gaussmodelle"
f2 <- "Test2"


pagedown::chrome_print(paste0("Themen/", thema3, ".Rmd"),
                       output = paste0("Themen/", thema3, ".pdf"),
                       wait = 10,
                       timeout = 120)

