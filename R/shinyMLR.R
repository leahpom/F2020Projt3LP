#' @title 3D Regression Plots using shiny
#'
#'
#'
#' @description User selects a model and a corresponding 3D scatterplot with regression plane is returned
#'
#' @return  3D regression scatterplots with an overlaying plane
#'
#' @section shiny:
#' The shiny web server will run once this function is invoked and will open a web browser. You should learn how this is implemented.
#'
#' The web server can be studied \url{https://shiny.rstudio.com/tutorial/}
#'
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyMLR()}
shinyMLR<-function(){
  shiny::runApp(system.file("shinyMLR", package="F2020Projt3LP"),launch.browser = TRUE)
}
