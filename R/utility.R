#utility

#' @export
#' 
convertTimeToDecimal <- function(t) {
  floor(t/100)+(t%%100)/60
}