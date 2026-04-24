#' bTB case count time series plot by month
#'
#' Creates a time series plot of bTB case counts by detection type.
#'
#'
#' @param df A data frame containing case-level data. Defaults to `all_cases_collapsed`.
#' @param month_interactive Logical. If `TRUE`, returns an interactive plot. If `FALSE`, returns a static ggplot.
#'
#' @return A ggplot object (static) or a ggiraph htmlwidget (interactive).
#'
#' @details
#' The function aggregates bTB cases by month and detection type, and
#' produces a time series visualisation. Interactive plots include hover
#' tooltips with detailed information.
#'
#' @examples
#' \dontrun{
#' case_count_time_series()
#' }
#'
#' @export
#'
#' @importFrom magrittr %>%

case_count_time_series <- function(df = all_cases_collapsed, month_interactive = TRUE) {

  next_line <- "\n"
  gif_string <- paste0("IFN-", '\u03b3')

  nth_element <- function(vector, starting_position, n) {
    vector[seq(starting_position, length(vector), n)]
  }

  tableau30_colors <- c(
    "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F", "#8CD17D",
    "#B6992D", "#F1CE63", "#499894", "#86BCB6", "#E15759", "#FF9D9A",
    "#79706E", "#BAB0AC", "#D37295", "#FABFD2", "#B07AA1", "#D4A6C8",
    "#9D7660", "#D7B5A6",
    "#393b79", "#637939", "#8c6d31", "#843c39", "#7b4173",
    "#5254a3", "#8ca252", "#bd9e39", "#ad494a", "#a55194"
  )

  scale_fill_tableau30 <- function(...) {
    ggplot2::scale_fill_manual(values = tableau30_colors, ...)
  }

  min_year <- min(df$year)
  max_year <- max(df$year)
  max_date <- format(max(df$merged_date_min), "%d-%B-%Y")

  df_sum <- df %>%
    dplyr::group_by(year_month, best_estimate_gif_skin_lab_string) %>%
    dplyr::summarise(cases = dplyr::n()) %>%
    dplyr::ungroup()

  df_sum_all <- df %>%
    dplyr::group_by(year_month) %>%
    dplyr::summarise(cases = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(best_estimate_gif_skin_lab_string = "All cases")

  df_final <- df_sum %>%
    dplyr::bind_rows(df_sum_all) %>%
    dplyr::arrange(year_month) %>%
    dplyr::mutate(
      best_estimate_gif_skin_lab_string = forcats::fct_relevel(
        best_estimate_gif_skin_lab_string,
        c("All cases", "SICTT",
          stringr::str_subset(unique(best_estimate_gif_skin_lab_string), "^IFN"),
          "Lab")
      )
    )

  if (sjmisc::str_contains(names(df_final), "year_month")) {
    df_final <- df_final %>%
      dplyr::mutate(
        year = lubridate::year(year_month),
        year_month_character = format(year_month, '%B-%Y'),
        hover_text_point_in_df = paste0(
          year, next_line,
          year_month_character, next_line,
          "Diagnostic type: ", best_estimate_gif_skin_lab_string, next_line,
          "Cases: ", cases
        ),
        hover_text_line = paste0(
          dplyr::across(dplyr::starts_with("best_estimate_gif_skin_lab_string"), ~ .x) %>% unlist()
        )
      )
  }

  min_max_dates_df <- df_final %>%
    dplyr::mutate(
      min_prev = min(cases, na.rm = TRUE),
      max_prev = max(cases, na.rm = TRUE)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      min_date = min(year_month),
      max_date = min_date %m+% lubridate::years(1),
      max_date = max_date %m-% lubridate::days(1),
      min_prev = dplyr::first(min_prev),
      max_prev = dplyr::first(max_prev)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year_f = as.character(year))

  time_var_class <- class(df_final$year_month)

  if (time_var_class == "Date") {
    scale_x_type <- list(
      ggplot2::scale_x_continuous(
        breaks = nth_element(lubridate::ymd(unique(df_final[["year"]]), truncated = 2L), 1, 1)
      ),
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)),
      ggplot2::geom_rect(
        data = min_max_dates_df,
        mapping = ggplot2::aes(
          xmin = min_date,
          xmax = max_date,
          ymin = min_prev,
          ymax = max_prev,
          fill = year_f
        ),
        inherit.aes = FALSE,
        alpha = 0.2
      ),
      scale_fill_tableau30(name = "Year", guide = "none")
    )
  } else {
    scale_x_type <- ggplot2::scale_x_continuous(breaks = nth_element(unique(df_final[[x_var]]), 1, 1)
    )
  }

  ggplot_static <- ggplot2::ggplot(df_final,
                                   ggplot2::aes(x = year_month, y = cases,
                                                color = best_estimate_gif_skin_lab_string)) +
    ggplot2::geom_line(linewidth = 1.5, alpha = 1) +
    ggplot2::labs(x = "Year",
                  y = "No. of bTB cases",
                  title =  paste0("bTB cases by detection type (", min_year, " - ", max_year, ")"),
                  subtitle = paste0("Latest data up until: ", max_date),
                  caption = paste0("SICTT: Single Intradermal Comparative Tuberculin Test",
                                   "\n",
                                   # "\n",
                                   "IFN-γ: interferon-gamma assay",
                                   "\n",
                                   "Lab: positive tissue sample on histopathology and/or culture and/or PCR, after detecting presence of visible lesions at slaugher-house inspection",
                                   "\n",
                                   "Note, the most recent month or two will be missing culture positive cases that have not yet been determined yet")) +
    ggsci::scale_colour_nejm(name = "bTB cases") +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    scale_x_type +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 18, face = "bold"),
      legend.text = ggtext::element_markdown(size = 16),
      plot.caption.position = "plot",
      legend.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.caption = ggplot2::element_text(size = 12, color = "black", face = "italic"),
      plot.title.position = "plot",
      legend.position = "top",
      axis.text = ggplot2::element_text(size = 14, face = "bold", colour = "black"),
      axis.title = ggplot2::element_text(size = 16, face = "bold", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
    )

  if (!month_interactive) {
    return(ggplot_static)
  }

  ggplot_interactive <- ggplot_static +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = hover_text_point_in_df, data_id = hover_text_point_in_df),
      size = 1.5, hover_nearest = TRUE, alpha = 0.6
    ) +
    ggiraph::geom_line_interactive(
      ggplot2::aes(tooltip = hover_text_line, data_id = hover_text_line),
      linewidth = 1.5, alpha = 0.6
    )

  ggiraph::girafe(
    ggobj = ggplot_interactive,
    options = list(
      ggiraph::opts_zoom(max = 5),
      ggiraph::opts_toolbar(position = "bottomleft"),
      ggiraph::opts_hover(css = "stroke:red;stroke-width:2;")
    ),
    width_svg = 12,
    height_svg = 10
  )
}


