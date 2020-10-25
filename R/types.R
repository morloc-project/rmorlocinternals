# This file holds type handling functions that are useful in tests and such.
# They are mostly for internal use, so are not exported.


#' Helper function for building list types
#'
#' The "list" here is a homogenous vector, like the Haskell or lisp list, not
#' the named, heterogenous lists of R. So `mlc_list(mlc_integer)` would be the
#' R version of the Haskell type `[Int]`, in this case, simply a numeric
#' vector.
#'
#' @param x The type parameter for a list
.mlc_list <- function(x){
  paste0("{\"list\":[", x, "]}")
}

.mlc_array <- function(...){
  paste0("[", paste0(c(...), collapse=","), "]")
}

#' Helper function for building tuple types
#'
#' A tuple corresponds to an unnamed R list.
#'
#' @param ... elements of the tuple as JSON strings
.mlc_tuple <- function(...){
  paste0("{\"tuple\":[", paste0(c(...), collapse=",") ,"]}")
}

#' Helper function for building JSON objects
#'
#' @param ... key-value pairs with JSON strings for values
.mlc_object <- function(...){
  entries <- list(...)
  make_field <- function(k) paste0('"', k, '"', ":", entries[[k]])
  entry_strings <- paste0(sapply(names(entries), make_field), collapse=",")
  paste0("{", entry_strings, "}", collapse="")
}

#' Helper function for building record types
#'
#' @param ... key-value pairs with JSON strings for values
.mlc_record <- function(...){
  paste0('{"record":', .mlc_object(...), '}')
}

#' Helper function for building table types
#'
#' @param ... key-value pairs with JSON strings for column types
.mlc_table <- function(...){
  entries <- list(...)
  for(name in names(entries)){
    entries[[name]] = .mlc_list(entries[[name]])
  }
  paste0('{"table":', do.call(.mlc_object,entries), '}')
}

#' An R scalar integer type
.mlc_integer <- '"integer"'

#' An R scalar numeric type
.mlc_numeric <- '"numeric"'

#' An R scalar character type
.mlc_character <- '"character"'

#' An R scalar character type
.mlc_logical <- '"logical"'

#' An R scalar character type
.mlc_null <- 'null'
