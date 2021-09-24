

img <- function(img_file, auto_pdf = FALSE){
  # wrapper around knitr::include_graphics
  # saving typing the path
 path_img <- paste0(here::here(),"/img/", img_file)
 knitr::include_graphics(path_img, auto_pdf = auto_pdf)
}
