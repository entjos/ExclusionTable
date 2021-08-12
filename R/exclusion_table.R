#' Exclusion Table
#'
#' This function keeps track of how many observations you exclude by using
#' specific inclusion and exclusion criteria. It assumes that your criteria
#' are logical filter statements, i.e. statements that you would pass to
#' `dplyr::filter()` or to `{data.table}`.
#'
#' @param data
#'    A dataframe on  wich the exclusions are to be performed.
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
#' @param keep_data
#'    A logical statement to indicate whether the new dataset without the
#'    excluded observations should be outputted. The default is `TRUE`.
#'
#' @return
#'    A list of tables including information on no. excluded individuals
#'    for each exclusion and inclusion criteria. If `keep_data == TRUE`
#'    also the dataset without the excludes observation is included
#'    as a `data.frame` in the list.
#'
#' @examples
#' exclusion_table(
#'    data = mtcars,
#'    exclusion_criteria = c("disp <= 70 | disp >= 300",
#'                           "as.character(gear) == '4'"),
#'    labels_exclusion   = c("First exclusion",
#'                           "Second exclusion")
#')
#'
#' @import data.table
#'
#' @export exclusion_table

exclusion_table <- function(
  data = NULL,
  inclusion_criteria = NULL,
  exclusion_criteria = NULL,
  labels_inclusion = inclusion_criteria,
  labels_exclusion = exclusion_criteria,
  keep_data = TRUE)
{

  # Check inputs

  # General checks on types
  if(is.null(data)){
    stop("argument 'data' is missing. Plaese specify a dataset.")
  }

  if(is.data.frame(data) == FALSE){
    stop("'data' is not an object of class dataframe.")
  }

  if(is.null(inclusion_criteria) & is.null(exclusion_criteria)){
    stop(paste0("both 'inclusion_criteria' and 'exclusion_criteria'",
                " are unspecified. Plase specify at least one of them."))
  }

  if(!is.null(inclusion_criteria) & !is.null(exclusion_criteria)){
    message(paste0("inclusion criterias will be applied prior to",
                   " to exclusion criteria"))
  }

  # Check inclusion_criteria argument if specified
  if(!is.null(inclusion_criteria)){

    if(is.character(inclusion_criteria) == FALSE){
      stop(paste0("'inclusion_criteria' is not a",
                  " character vector."))
    }

    if(is.character(labels_inclusion) == FALSE){
      stop(paste0("'labels_inclusion' is not a",
                  " character vector."))
    }

    if(length(labels_inclusion) != length(inclusion_criteria)){
      stop(paste0("'inclusion_criteria' and 'labels_inclusion' must",
                  " have the same length."))
    }
  }

  # Check exclusion_criteria argument if specified
  if(!is.null(exclusion_criteria)){

    if(is.character(exclusion_criteria) == FALSE){
      stop(paste0("'exclusion_criteria' is not a",
                  " character vector."))
    }

    if(is.character(labels_exclusion) == FALSE){
      stop(paste0("'labels_exclusion' is not a",
                  " character vector."))
    }

    if(length(labels_exclusion) != length(exclusion_criteria)){
      stop(paste0("'exclusion_criteria' and 'labels_exclusion' must",
                  " have the same length."))
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
                 data.table::setDT(data)[eval(parse(text = filter_string)), ]

               # Get no rows post exclusion
               n_post <- nrow(excluded_data)

               # Save data set with exclusions
               assign("data",
                      excluded_data,
                      envir = sys.frame(1))

               # Create table
               table <- data.frame(inclusion  = labels_inclusion[[i]],
                                   n_prior    = n_prior,
                                   n_post     = n_post,
                                   n_excluded = n_prior - n_post)

               return(table)

             }) %>%
      dplyr::bind_rows()

    # Add total no excluded observations
    table_in <-
      rbind(table_in,
            data.frame(inclusion  = "TOTAL",
                       n_prior    = max(table_in$n_prior),
                       n_post     = min(table_in$n_post),
                       n_excluded = max(table_in$n_prior) - min(table_in$n_post)
            ))
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
                 data.table::setDT(data)[eval(parse(text = filter_string)), ]

               # Get no rows post exclusion
               n_post <- nrow(excluded_data)

               # Save data set with exclusions
               assign("data",
                      excluded_data,
                      envir = sys.frame(1))

               # Create table
               table <- data.frame(exclusion  = labels_exclusion[[i]],
                                   n_prior    = n_prior,
                                   n_post     = n_post,
                                   n_excluded = n_prior - n_post)

               return(table)

             }) %>%
      dplyr::bind_rows()

    # Add total no excluded observations
    table_ex <-
      rbind(table_ex,
            data.frame(exclusion  = "TOTAL",
                       n_prior    = max(table_ex$n_prior),
                       n_post     = min(table_ex$n_post),
                       n_excluded = max(table_ex$n_prior) - min(table_ex$n_post)
            ))
  }

  # Return dataset after exclusions and table with exclusions

  # Return list including data
  if(keep_data == TRUE){

    if(is.null(exclusion_criteria)){

      return(list(
        table_in = table_in,
        dataset = data.frame(data)
      ))

    } else if(is.null(inclusion_criteria)){

      return(list(
        table_ex = table_ex,
        dataset = data.frame(data)
      ))

    } else {

      return(list(
        table_in = table_in,
        table_ex = table_ex,
        dataset = data.frame(data)
      ))

    }

    # Returning list without data
  } else if(keep_data == FALSE){

    if(is.null(exclusion_criteria)){

      return(list(
        table_in = table_in
      ))

    } else if(is.null(inclusion_criteria)){

      return(list(
        table_ex = table_ex
      ))

    } else {

      return(list(
        table_in = table_in,
        table_ex = table_ex
      ))

    }

  }

}
