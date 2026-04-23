#' Read the Latest bTB all_cases dataset (animal level bTB dataset)
#'
#' This function finds the most recent `year_XXXX_qX` folder inside a base
#' directory, identifies `.rds` files whose filename contains `"all_cases"`,
#' and loads the most recently modified file.
#'
#' @param base_path Character. Path containing folders of the form
#'   `year_XXXX_qX`.
#'
#' @return A dataset read from the most recent `all_cases*.rds` file.
#'
#' @examples
#' \dontrun{
#' all_cases_cvera_read_in("N:/data/tb/master_tb_data_jamie_m")
#' }
#'
#' @export
#' @importFrom dplyr %>% arrange desc slice pull
#' @importFrom fs dir_ls path_file file_info
#' @importFrom crayon red yellow

all_cases_cvera_read_in <- function(base_path = "N:/data/tb/master_tb_data_jamie_m") {

  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)

  matched_folders <- folders[grepl("^year_\\d{4}_q[1-4]$", folders)]

  message(crayon::red("The following folders are available:"))
  print(matched_folders)

  folder_info <- data.frame(
    folder = matched_folders,
    year = as.integer(sub("year_(\\d{4})_q[1-4]", "\\1", matched_folders)),
    quarter = as.integer(sub("year_\\d{4}_q([1-4])", "\\1", matched_folders)),
    stringsAsFactors = FALSE
  )

  message(crayon::red("Parsed folder information:"))
  print(folder_info)

  latest_folder <- folder_info %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(quarter)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(folder)

  latest_folder_location <- file.path(base_path, latest_folder)

  all_cases_files <- fs::dir_ls(latest_folder_location)

  all_cases_files <- all_cases_files[
    grepl("(?i)all_cases", fs::path_file(all_cases_files), perl = TRUE) &
      grepl("\\.rds$", fs::path_file(all_cases_files), ignore.case = TRUE)
  ]

  if (length(all_cases_files) == 0) {
    stop(crayon::yellow("No all_cases*.rds files found in latest folder."))
  }

  message(crayon::red("All files containing 'all_cases' in the latest folder:"))
  print(all_cases_files)

  all_cases_file <- all_cases_files[
    which.max(fs::file_info(all_cases_files)$modification_time)
  ]

  message(crayon::red("The path and latest dataset being read in is:"))
  print(all_cases_file)

  readRDS(all_cases_file)
}










