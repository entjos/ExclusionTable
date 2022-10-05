#' Prints `exl_tbl` objects
#'
#' This is a print function for `exl_tbl` objects, created with
#' `exlcusion_table()`. The function improves the readability of the output.
#'
#' @param x
#'    An `exl_tbl` object.
#'
#' @param ...
#'    Other arguments that should be past to print.
#'
#' @return
#'    No return value, called for side effects.
#' @importFrom magrittr %>%
#'
#' @method print exl_tbl
#'
#' @export

print.exl_tbl <- function(x, ...){

  # Check for right class
  if(!inherits(x, "exl_tbl")){

    stop("Object is not of class exl_tbl")

  }

  # Determine the output length for display -----------------------------------
  if(!is.null(x$table_in) & !is.null(x$table_ex)){

    # Check the maximal number of characters in each line of table_ex
    length_ex <- vapply(seq(nrow(x$table_ex)),
                        FUN.VALUE = 0L,
                        function(i){

                          # Compare with the nchar of colnames for each cell
                          vapply(seq(ncol(x$table_ex)),
                                 FUN.VALUE = 0L,
                                 function(j){

                                   max(nchar(x$table_ex[i, j]),
                                       nchar(colnames(x$table_ex)[j]),
                                       na.rm = TRUE)

                                 }) %>% sum()

                        }) %>% max()

    # Check the maximal number of characters in each line of table_in
    length_in <- vapply(seq(nrow(x$table_in)),
                        FUN.VALUE = 0L,
                        function(i){

                          # Compare with the nchar of colnames for each cell
                          vapply(seq(ncol(x$table_in)),
                                 FUN.VALUE = 0L,
                                 function(j){

                                   max(nchar(x$table_in[i, j]),
                                       nchar(colnames(x$table_in)[j]),
                                       na.rm = TRUE)

                                 }) %>% sum()

                        }) %>% max()

    # Take maxium nchar from table_in and table_ex
    length_out <- max(length_ex, length_in)


  } else if(!is.null(x$table_in)) {

    # Check the maximal number of characters in each line
    length_out <- vapply(seq(nrow(x$table_in)),
                         FUN.VALUE = 0L,
                         function(i){

                           # Compare with the nchar of colnames for each cell
                           vapply(seq(ncol(x$table_in)),
                                  FUN.VALUE = 0L,
                                  function(j){

                                    max(nchar(x$table_in[i, j]),
                                        nchar(colnames(x$table_in)[j]),
                                        na.rm = TRUE)

                                  }) %>% sum()

                         }) %>% max()

  } else if(!is.null(x$table_ex)) {

    # Check the maximal number of characters in each line
    length_out <- vapply(seq(nrow(x$table_ex)),
                         FUN.VALUE = 0L,
                         function(i){

                           # Compare with the nchar of colnames for each cell
                           vapply(seq(ncol(x$table_ex)),
                                  FUN.VALUE = 0L,
                                  function(j){

                                    max(nchar(x$table_ex[i, j]),
                                        nchar(colnames(x$table_ex)[j]),
                                        na.rm = TRUE)

                                  }) %>% sum()

                         }) %>% max()

  }

  # Add 6 spaces to length out for spaces between cells
  length_out <- length_out + 6L

  # Start printing output -----------------------------------------------------
  cat("\n", rep("=", length_out), sep = "")
  cat("\nExcluded the following observations:")
  cat("\n", rep("=", length_out), sep = "")

  # Print Inclusions
  if(!is.null(x$table_in)){

    cat("\nExclusions based on INCLUSION criteria\n\n")

    print(x$table_in)

  }

  # Print exclusions
  if(!is.null(x$table_ex)){

    cat("\nExclusions based on EXCLUSION criteria\n\n")

    print(x$table_ex)

  }

  cat("\n", rep("=", length_out), sep = "")
}
