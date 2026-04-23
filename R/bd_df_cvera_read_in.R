#' Read the Latest bTB breakdown dataset (bd_df)
#'
#' This function finds the most recent `year_XXXX_qX` folder inside a base
#' directory, identifies `.rds` files whose filename contains `"bd_df"`,
#' and loads the most recently modified file.
#'
#' @param base_path Character. Path containing folders of the form
#'   `year_XXXX_qX`.
#'
#' @return A dataset read from the most recent `bd_df*.rds` file.
#'
#' @examples
#' \dontrun{
#' bd_df_cvera_read_in("N:/data/tb/master_tb_data_jamie_m")
#' }
#'
#' @export
#' @importFrom dplyr %>% arrange desc slice pull
#' @importFrom fs dir_ls path_file file_info
#' @importFrom crayon red yellow

bd_df_cvera_read_in <- function(base_path = "N:/data/tb/master_tb_data_jamie_m") {


  # Get all folders in the directory
  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)

  # Filter for those that match "year_XXXX_qX" format
  matched_folders <- folders[grepl("^year_\\d{4}_q[1-4]$", folders)]

  message(crayon::red("The following folders are available:"))
  print(matched_folders)

  # Extract year and quarter
  folder_info <- data.frame(
    folder = matched_folders,
    year = as.integer(sub("year_(\\d{4})_q[1-4]", "\\1", matched_folders)),
    quarter = as.integer(sub("year_\\d{4}_q([1-4])", "\\1", matched_folders)),
    stringsAsFactors = FALSE
  )

  message(crayon::red("Parsed folder information:"))
  print(folder_info)

  # Get the latest folder by year and quarter
  latest_folder <- folder_info %>%
    dplyr::arrange(dplyr::desc(year), dplyr::desc(quarter)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(folder)

  # Construct the full path
  #latest_folder_location <- file.path(base_path, latest_folder, "data/edited_output")
  latest_folder_location <- file.path(base_path, latest_folder)

  bd_df_files <- fs::dir_ls(latest_folder_location)

  # List all files in that folder that contain "bd_df" (case-insensitive ?!)
  # and ending in rds
  bd_df_files <- bd_df_files[
    grepl("(?i)bd_df", fs::path_file(bd_df_files), perl = TRUE) &
      grepl("\\.rds$", fs::path_file(bd_df_files), ignore.case = TRUE)
  ]


  if (length(bd_df_files) == 0) {
    stop(crayon::yellow("No bd_df*.rds files found in latest folder."))
  }

  # Print them in red
  message(crayon::red("All files containing 'bd_df' in the latest folder:"))
  print(bd_df_files)

  # Get the most recently modified bd_df*.rds file in the latest folder
  bd_df_file <- bd_df_files[
    which.max(fs::file_info(bd_df_files)$modification_time)
  ]

  message(crayon::red("The path and latest dataset being read in is:"))
  print(bd_df_file)

  readRDS(bd_df_file)
}

