#' View skin test type definitions (SICTT)
#'
#' Reads the test types file definitions and returns a formatted table.
#'
#' @param test_type_filter Optional character vector of test types to filter.
#'   If NULL (default), all rows are returned.
#'
#' @return A gt table.
#'
#'#' @examples
#' \dontrun{
#' # print all sictt definitions:
#' test_types_sictt_table()
#' # only print specific ones:
# test_types_sictt_table(c("1", "9A"))
#' }
#'
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter rename %>%
#' @importFrom gt gt fmt_markdown cols_width tab_style cells_column_labels cells_body cell_fill cell_text



test_types_sictt_table <- function(test_type_filter = NULL) {

  # test_types <- readxl::read_excel("data/test_types.xlsx")
  # file_path <- system.file("extdata", "test_types.xlsx", package = "cvera")
  #
  # if (!file.exists(file_path)) {
  #   stop("test_types.xlsx not found in package extdata/")
  # }
  #
  # test_types <- readxl::read_excel("data/test_types.xlsx")
  # usethis::use_data(test_types, internal = TRUE, overwrite = TRUE)
  #
  # test_types <- readxl::read_excel(file_path)
  # test_types <- test_types
  # put dataset directly into package using:
  # test_types <- readxl::read_excel("data/test_types.xlsx")
  # usethis::use_data(test_types, internal = TRUE, overwrite = TRUE)

  #  filtering
  if (!is.null(test_type_filter)) {
    test_types <- test_types %>%
      dplyr::filter(test_type %in% as.character(test_type_filter))
  }

  test_types <- test_types %>%
    dplyr::rename(
    `SICTT test type` = test_type,
    `Test name` = test_description,
    `Explanation` = explanation
  )

  test_types |>
    gt::gt() |>
    gt::fmt_markdown(gt::everything()) |>
    gt::cols_width(
      1 ~ gt::px(40)
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "lightblue"),
        gt::cell_text(
          weight = "bold",
          size = gt::px(18),
          font = "Arial"
        )
      ),
      locations = gt::cells_column_labels(gt::everything())
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels(columns = 3)
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = 1:2)
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#cfe8d1"),
      locations = gt::cells_body(
        rows = seq_len(nrow(test_types))[seq_len(nrow(test_types)) %% 2 == 0]
      )
    ) %>%
    # gt::tab_style(
    #   style = gt::cell_fill(color = "#cfe8d1"),
    #   locations = gt::cells_body(
    #     rows = seq(2, nrow(test_types), by = 2)
    #   )
    # )
    gt::tab_source_note(
      source_note = gt::md(
        "**Veterinary handbook for bTB include information on the tests although exact definitions of the tests are not included**

**2010 edition:**
<https://www.bovinetb.info/docs/veterinary-handbook-for-herd-management-in-the-bovine-tb-eradication-programme.pdf>


**2016/2017 edition:**
<https://www.researchgate.net/publication/323402319_Veterinary_Handbook_for_herd_management_in_the_bovine_TB_Eradication_Programme>"
      )
    )
}
