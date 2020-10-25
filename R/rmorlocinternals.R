#' @importFrom grDevices dev.off pdf
#' @useDynLib rmorlocinternals
#' @importFrom Rcpp sourceCpp
NULL


#' Deserialize JSON data to an R native type using a JSON type string
#'
#' @param data JSON data
#' @param schema JSON type string
#' @export
mlc_deserialize <- function(data, schema){
    x <- mlc_deserialize_(data=data, schema=schema)
    if(class(x) == "data.frame"){
        rownames(x) = NULL 
    }
    x
}
