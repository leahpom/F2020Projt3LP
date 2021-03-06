#' @title CASTINGS data set.
#'
#' @description  A dataset containing data from an experiment to look at the relationship between worker productivity and salary incentive
#'
#' @details This is a standard data set from the course MATH 5773 taken from the data sets provided by the text book.
#'
#' @format A data frame with 19 rows and 5 variables:
#' \describe{
#'   \item{PLANT}{Binary variable recording if the plant is a union or nonunion plant}
#'   \item{INCENTIVE}{Factor variable with three levels recording bonus cents per casting}
#'   \item{CASTINGS}{Acceptable machine castings per worker in a 4-week 40hr/week work period}
#'   \item{PDUMMY}{A dummy variable for if the plant is union or nonunion}
#'   \item{INC_PDUM}{A dumm variable for the incentive}
#' }
#'
#' @source \url{https://www.routledge.com/Statistics-for-Engineering-and-the-Sciences-Sixth-Edition/Mendenhall-Sincich/p/book/9781498728850}
"CASTINGS"
