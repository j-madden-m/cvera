#' @title core_vars
#' @description \code{top} Simple function to extract (e.g. dplyr::select)
#' core variables from master_tb dataset, useful for quick sanity checks and can
#' be added into a pipe. Saves having to type them out manually.
#' @param df Master bTB dataset (master_tb). (...) = add in additional variables
#' @return returns dataset with core variables
#' @details DETAILS
#'
#'
#' @examples
#' \dontrun{
#' #master_tb %>%
#' # filter(total_reactor_skin > 10) %>%
#' # core_vars()
#' #add variables
#' #master_tb %>%
#' # filter(total_reactor_skin > 10) %>%
#' # core_vars(county)
#' #remove one of the core variables
#' #master_tb %>%
#' # filter(total_reactor_skin > 10) %>%
#' # core_vars(-test_type)
#' }
#' @export
#'

core_vars <- function(df, ...) {
  df %>%
    select(herd_no, fixed_test_date, skin_fixed_test_date,
           test_type, trading_status,
           bd_no, starts, ends, gif_actual_date, test_date_lab, total_animals,
           total_clear,
           total_inconclusive, total_reactor_skin, total_reactor_slaughter,
           gif_cases, total_standard_reactor, total_standard_reactor_lesions,
           total_non_permit_animal_lesion, total_reactor_permit_lesions, ...)
}




