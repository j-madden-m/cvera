#' @title bd_dataset_fun
#' @description \code{top} Creates BD file containing information on each BD.
#' @param df Master bTB dataset (master_tb).
#' @return returns BD dataset which only contains information on individual
#' BDs. If a herd never had a BD, they will not be in this file. Each row is
#' a summary of the BD (e.g. number of reactors over the BD period etc.). A
#' herd can have multiple rows if it has had more than one BD.
#' @details DETAILS
#'
#'
#' @examples
#' \dontrun{
#' #bd_df <- bd_dataset_fun(master_tb)
#' }
#' @export
#' @importFrom sjlabelled var_labels
#'



bd_dataset_fun <- function(df) {

  print("Run time approximately 3 minutes")

  bd_only <- df %>%
    filter(!is.na(bd_no))

  bd_test <- bd_only %>%
    group_by(herd_no, bd_no) %>%
    summarise(bd_start = if(all(is.na(starts))) NA else min(starts, na.rm = TRUE),
              #bd_start_yr = min(test_year),
              bd_start_yr = min(test_skin_lab_year),
              bd_end = if(all(is.na(ends))) NA else max(ends, na.rm = TRUE),
              #bd_end_yr = max(test_year),
              bd_end_yr = max(test_skin_lab_year),
              no_of_tests = n(),
              test_types = paste(test_type, collapse = ", "),
              bd_initiated = first(first_test_type),
              bd_initiated_skin_lab = first(lab_or_skin),
              mean_total_animals = round(mean(total_animals), 0), mean_total_clear = round(mean(total_clear), 0),
              min_animal_skin = min(total_animals), max_animal_skin = max(total_animals),
              min_reactor_skin = min(total_reactor_skin), max_reactor_skin = max(total_reactor_skin),
              min_inconclusive_skin = min(total_inconclusive), max_inconclusive_skin = max(total_inconclusive),
              min_clear_skin = min(total_clear), max_clear_skin = max(total_clear),

              #index results
              no_animals_index_test = first(total_animals),
              no_inconclusive_index_test = first(total_inconclusive),
              no_skin_reactor_index_test = first(total_reactor_skin),
              no_slaughter_reactor_index_test = first(total_reactor_slaughter),  #this should be the same as slaughter, no?
              no_standard_reactor_index_test = first(total_standard_reactor),
              no_standard_reactor_lesions_index_test = first(total_standard_reactor_lesions),
              no_reactor_permit_lesions_index_test = first(total_reactor_permit_lesions),
              herd_type = first(herd_type),
              dvo = first(dvo),
              county = first(county)

    ) %>%
    ungroup() %>%
    bind_cols(., {bd_only %>%
        group_by(herd_no, bd_no) %>%
        #maybe next year change gif_cases to NA if before 2019?
        summarise_at(vars(matches("total"), gif_cases), sum, na.rm = TRUE) %>%
        ungroup() %>%
        dplyr::select( -herd_no, -bd_no)
    }) %>%
    sjlabelled::var_labels(
      herd_no ="Herd number",
      bd_no = "BD (breakdown) number",
      min_animal_skin = "Minimum number of animals skin tested during BD",
      max_animal_skin = "Maximum number of animals skin tested during BD",
      min_reactor_skin = "Minimum number of reactors detected by skin test during BD",
      max_reactor_skin = "Maximum number of reactors detected by skin test during BD",
      min_inconclusive_skin = "Minimum number of inconclusive detected by skin test during BD",
      max_inconclusive_skin = "Maximum number of inconclusive detected by skin test during BD",
      min_clear_skin = "Minimum number of clear animals detected by skin test during BD",
      max_clear_skin = "Maximum number of clear animals detected by skin test during BD",
      bd_start_yr = "Year in which breakdown started",
      bd_end_yr = "Year in which breakdown ended",
      bd_start = "Start date of breakdown",
      bd_end = "End date of breakdown",
      no_of_tests = "Number of tests during BD (excluding slaughter detection, if applicable)",
      test_types = "All skin tests performed during BD in order",
      bd_initiated = "Test type that initiated BD (or slaughter detection)",
      bd_initiated_skin_lab = "Skin test or lab that initiated BD (or slaughter detection)",
      total_reactor_skin = "Total skin reactors",
      total_standard_reactor = "Total standard skin reactors above 4mm diff",
      total_standard_reactor_lesions = "Total standard skin reactors with lesions",
      total_reactor_permit_lesions = "Total skin reactors with lesions",
      mean_total_animals = "Average total animals over all tests",
      mean_total_clear = "Average total clear over all tests",
      total_animals = "Total animals skin tested during BD (could be repeat counting)",
      total_clear = "Total animals clear skin tested during BD (could be repeat counting)",
      total_inconclusive = "Total inconclusive animals skin tested during BD (could be repeat counting)",
      total_reactor_slaughter = "Total reactors detected at slaughter",
      total_non_permit_animal_lesion = "Total reactors with lesions without permit",
      gif_cases = "Total GIF cases (only after May 2019)",
      no_animals_index_test = "Number of animal at index test",
      no_inconclusive_index_test = "Number of inconclusive at index test",
      no_skin_reactor_index_test = "Number of skin reactors at index test",
      no_slaughter_reactor_index_test = "Number of TB positive animals detected at/near index test",
      no_standard_reactor_index_test = "Number of standard reactors at index test",
      no_standard_reactor_lesions_index_test = "Number of standard reactors with lesions at index test",
      no_reactor_permit_lesions_index_test = "Number reactors with lesions at index test",
      dvo = "District Veterinary Office",
      county = "County herd is registered with"
    ) %>%
    #rename(total_reactor_skin = total_reactors) %>%
    #insert date of download each year - automate this
    mutate(bd_end = case_when(is.na(bd_end) & bd_end_yr >= 2020 ~ as.Date(parse_date_time("22/02/2022", c("dmy"))),
                              !is.na(bd_end) ~ bd_end,
                              TRUE ~ as.Date(NA)),
           bd_end_yr = year(bd_end),
           all_cases = total_reactor_skin + ifelse(is.na(total_reactor_slaughter), 0, total_reactor_slaughter) + gif_cases,
           bd_duration_days = bd_end - bd_start,
           bd_duration_yrs = round((bd_duration_days/365), digits = 1)) %>%

    group_by(herd_no) %>%
    mutate(duration_between_bd = bd_start - lag(bd_end)) %>%
    ungroup() %>%

    sjlabelled::var_labels(
      all_cases ="Total/All reactors (skin + slaughter + gif)",
      bd_duration_days = "Duration of BD in days",
      bd_duration_yrs = "Duration of BD in years",
      duration_between_bd = "Duration between consecutive BDs" )

  bd_test

}


