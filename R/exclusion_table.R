#' Exclusion Table
#'
#' This function keeps track of how many observations you exclude by using
#' specific inclusion and exclusion criteria. It assumes that your criteria
#' are logical filter statements, i.e. statements that you would pass to
#' `dplyr::filter()` or to `{data.table}`.
#'
#' @param data
#'    A dataframe on which the exclusions are to be performed.
#'
#' @param exclusion_criteria
#'    A character vector of logical expressions that are used for
#'    exclusions. All observations who meet this
#'    criteria will be excluded. Specifically, observations for which the
#'    logical expression is `TRUE` will be excluded. Please keep in mind
#'    how your expression will handle `NA` values.
#'
#' @param inclusion_criteria
#'    A character vector of logical expressions that are used for
#'    inclusions. All individuals who meet these criteria will be included.
#'    Specifically, observations for which the logical expression is `FALSE`
#'    will be excluded. Please keep in mind how your expression will handle
#'    `NA` values.
#'
#' @param labels_exclusion
#'    An optional character vector of labels that are used to label the
#'    steps of exclusions. The default labels are the logical expressions
#'    passed to `exclusion_criteria`.
#'
#' @param labels_inclusion
#'    An optional character vector of labels that are used to label the
#'    steps of inclusions. The default labels are the logical expressions
#'    passed to `inclusion_criteria`
#'
#' @param obj
#'    A named list of objects that will be passed to the filtering call.
#'    The list can be access using `obj$<name of object>` in the filtering
#'    call.
#'
#' @param keep_data
#'    A logical statement to indicate whether the new dataset without the
#'    excluded observations should be outputted. The default is `TRUE`.
#'
#' @param id
#'    Optional name of a unique ID variable in the dataset.
#'
#' @return
#'    `exclusion_table` returns a `exl_tbl` object which is a list of
#'    data frames including the following information:
#'    \item{`table_in`}{a `data.frame` including the number of observations
#'    excluded for each inclusion criteria listed in `inclusion_criteria`.}
#'    \item{`table_ex`}{a `data.frame` including the number of observations
#'    excluded for each exclusion criteria listed in `exclusion_criteria`.}
#'    \item{`dataset`}{a `data.frame` of the supplied dataset after applying
#'    all inclusion and exclusion criteria.}
#'
#'    If `id` is supplied, an additional column is added to `table_in` and
#'    `table_ex` including a list of the IDs that have been excluded from
#'    the dataset in each step.
#'
#' @examples
#' #Example without using the obj argument
#' exclusion_table(
#'    data = mtcars,
#'    exclusion_criteria = c("disp <= 70 | disp >= 300",
#'                           "as.character(gear) == '4'"),
#'    labels_exclusion   = c("First exclusion",
#'                           "Second exclusion")
#')
#'
#' #Example using the obj argument
#' my_selection <- c(8, 6)
#'
#' exclusion_table(
#'   data = mtcars,
#'   exclusion_criteria = c("cyl %in% my_selection"),
#'   labels_exclusion   = c("First exclusion"),
#'   obj = list(my_selection = my_selection)
#' )
#'
#' @export exclusion_table

exclusion_table <- function(data = NULL,
                            inclusion_criteria = NULL,
                            exclusion_criteria = NULL,
                            labels_inclusion = inclusion_criteria,
                            labels_exclusion = exclusion_criteria,
                            obj = NULL,
                            keep_data = TRUE,
                            id = NULL){

  # Check inputs

  # General checks on types
  if(is.null(data)){
    cli::cli_abort(c("Argument {.var data} is missing.", " " = "Please specify a dataset."))
  }

  if(is.data.frame(data) == FALSE){
    cli::cli_abort("{.var data} is not a {.cls data.frame} object.")
  }

  if(is.null(inclusion_criteria) && is.null(exclusion_criteria)){
    cli::cli_abort(c(
      "Require at least one criterion",
      "x" = "Both {.var inclusion_criteria} and {.var exclusion_criteria} are unspecified.",
      " " = "Please specify at least one of them.")
    )
  }

  # Check inclusion_criteria argument if specified
  if(!is.null(inclusion_criteria)){

    if(is.character(inclusion_criteria) == FALSE){
      cli::cli_abort(c(
        "{.var inclusion_criteria} must be a {.cls character} vector.",
        "x" = "{.var inclusion_criteria} is a {.cls {class(inclusion_criteria)} vector.")
      )
    }

    if(is.character(labels_inclusion) == FALSE){
      cli::cli_abort(c(
        "{.var labels_inclusion} must be a {.cls character} vector.",
        "x" = "But is a {.cls {class(labels_inclusion)} vector.")
      )
    }

    if(length(labels_inclusion) != length(inclusion_criteria)){
      cli::cli_abort(c(
        "{.var labels_inclusion} and {.var inclusion_criteria} must have same length.",
        "x" = "{.var labels_inclusion} has length {length(labels_inclusion)} but
        {.var inclusion_criteria} has length {length(inclusion_criteria)}.")
      )
    }
  }

  # Check exclusion_criteria argument if specified
  if(!is.null(exclusion_criteria)){

    if(is.character(exclusion_criteria) == FALSE){
      cli::cli_abort(c(
        "{.var exclusion_criteria} must be a {.cls character} vector.",
        "x" = "{.var exclusion_criteria} is a {.cls {class(exclusion_criteria)} vector.")
      )
    }

    if(is.character(labels_exclusion) == FALSE){
      cli::cli_abort(c(
        "{.var labels_exclusion} must be a {.cls character} vector.",
        "x" = "{.var labels_exclusion} is a {.cls {class(labels_exclusion)} vector.")
      )
    }

    if(length(labels_exclusion) != length(exclusion_criteria)){
      cli::cli_abort(c(
        "{.var labels_exclusion} and {.var exclusion_criteria} must have same length.",
        "x" = "{.var labels_exclusion} has length {length(labels_exclusion)} but
        {.var exclusion_criteria} has length {length(exclusion_criteria)}.")
      )
    }

  }

  if(!is.null(id)){
    if(!is.character(id)){
      cli::cli_abort(
        c(x = "{.var id} is not of type character")
      )
    } else if (!id %in% colnames(data)){
      cli::cli_abort(
        c(x = "{.var id} cannot be found in {.var data}.")
      )
    } else if (length(id) > 1){
      cli::cli_abort(
        c(x = "{.var id} includes more than one column name.",
          i = "Suppliying multiple ID columns is not supported.")
      )
    } else if(length(unique(data[[id]])) < nrow(data)){
      c(x = "{.var id} is not unique.",
        i = "Please supply an ID variable that uniquely identifies observations")
    }
  }

  # Apply inclusion criteria
  if(!is.null(inclusion_criteria)){

    table_in <-
      lapply(seq_along(inclusion_criteria),
             function(i){

               # Get no rows prior exclusion
               n_prior <- nrow(data)

               # Define filtering string
               filter_string <- paste0("(", inclusion_criteria[[i]], ")")

               # Exclude observations
               excluded_data <-
                 subset(x      = data,
                        subset = eval(parse(text = filter_string)))

               # Get no rows post exclusion
               n_post <- nrow(excluded_data)

               # Save id number of excluded observations
               if(!is.null(id)){

                 ids <- data[[id]][!data[[id]] %in% excluded_data[[id]]]

               }

               # Save data set with exclusions
               data <<- excluded_data

               # Create table
               table <- data.table::data.table(
                 inclusion  = labels_inclusion[[i]],
                 n_prior    = n_prior,
                 n_post     = n_post,
                 n_excluded = n_prior - n_post
               )

               if(!is.null(id)){
                 table$ids <- list(ids)
               }

               return(table)

             }) |>
      data.table::rbindlist()

    # Add total no excluded observations
    table_in <-
      rbind(table_in,
            data.table::data.table(
              inclusion  = "TOTAL",
              n_prior    = max(table_in$n_prior),
              n_post     = min(table_in$n_post),
              n_excluded = max(table_in$n_prior) - min(table_in$n_post)
            ), fill = TRUE)

    if(!is.null(id)){

      table_in$ids[[nrow(table_in)]] <- Reduce(c, table_in$ids)

    }

  }

  # Apply exclusion criteria
  if(!is.null(exclusion_criteria)){

    table_ex <-
      lapply(seq_along(exclusion_criteria),
             function(i){

               # Get no rows prior exclusion
               n_prior <- nrow(data)

               # Define filtering string
               filter_string <- paste0("!(", exclusion_criteria[[i]], ")")

               # Exclude observations
               excluded_data <-
                 subset(x      = data,
                        subset = eval(parse(text = filter_string)))

               # Get no rows post exclusion
               n_post <- nrow(excluded_data)

               # Save id number of excluded observations
               if(!is.null(id)){

                 ids <- data[[id]][!data[[id]] %in% excluded_data[[id]]]

               }

               # Save data set with exclusions
               data <<- excluded_data

               # Create table
               table <- data.table::data.table(
                 exclusion  = labels_exclusion[[i]],
                 n_prior    = n_prior,
                 n_post     = n_post,
                 n_excluded = n_prior - n_post
               )

               if(!is.null(id)){
                 table$ids <- list(ids)
               }

               return(table)

             }) |>
      data.table::rbindlist()

    # Add total no excluded observations
    table_ex <-
      rbind(table_ex,
            data.table::data.table(
              exclusion  = "TOTAL",
              n_prior    = max(table_ex$n_prior),
              n_post     = min(table_ex$n_post),
              n_excluded = max(table_ex$n_prior) - min(table_ex$n_post)
            ), fill = TRUE)

    if(!is.null(id)){

      table_ex$ids[[nrow(table_ex)]] <- Reduce(c, table_ex$ids)

    }
  }

  # Return dataset after exclusions and table with exclusions

  # Return list including data
  if(keep_data == TRUE){

    if(is.null(exclusion_criteria)){

      # Return output
      out <- list(table_in = data.frame(table_in),
                  dataset  = data.frame(data))

    } else if(is.null(inclusion_criteria)){

      # Return output
      out <- list(table_ex = data.frame(table_ex),
                  dataset  = data.frame(data))

    } else {

      # Return output
      out <- list(table_in = data.frame(table_in),
                  table_ex = data.frame(table_ex),
                  dataset  = data.frame(data))

    }

    # Returning list without data
  } else if(keep_data == FALSE){

    if(is.null(exclusion_criteria)){

      # Return output
      out <- list(table_in = table_in)

    } else if(is.null(inclusion_criteria)){

      # Return output
      out <- list(table_ex = table_ex)

    } else {

      # Return output
      out <- list(table_in = table_in,
                  table_ex = table_ex)

    }

  }

  # Set object class

  class(out) <- "exl_tbl"

  return(out)

}
