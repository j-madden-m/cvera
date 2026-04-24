#' @title herd_plot
#' @description \code{top} is a small function to not just present the first rows
#' @param herd_no_character herd number
#' @param start_date start date of plot search. Format, '%Y-%m-%d'. Default is NULL so all years are plotted.
#' @param end_date end date of plot search. Format, '%Y-%m-%d'. Default is NULL so all years are plotted.
#' @param format date format, Default: '%Y-%m-%d'
#' @param include_tables Include herd-level tables of master_tb and breakdown datatset bd_df for the herd of interest. Default is FALSE which produces interactive plot without tables.
#' @param alternative_herd_no_name If you want to change the herd_no in title to something else.
#' @return returns new column in BD dataset.
#' @details DETAILS
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' herd_plot("x1234567")
#' herd_plot("x1234567", start_date = "2010-01-01")
#' herd_plot("x1234567", include_tables = TRUE, alternative_herd_no_name = "other name/number")
#'
#'  }
#' }
#' @export
#'
#' @importFrom dplyr filter mutate select group_by summarise ungroup arrange distinct bind_rows %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_rect geom_hline geom_vline
#' @importFrom stats na.omit
#'
#' @import ggiraph
#' @import ggnewscale
#' @import ggthemes
#' @import gt
#' @import htmltools
#'

herd_plot <- function(herd_no_character, start_date = NULL, end_date = NULL, format = "%Y-%m-%d",
                      include_tables = FALSE, alternative_herd_no_name = NULL) {

  required_datasets <- c("master_tb", "all_cases_collapsed", "bd_df")
  missing_datasets <- required_datasets[!sapply(required_datasets, exists, envir = .GlobalEnv)]

  if (length(missing_datasets) > 0) {
    stop(paste0("Missing required dataset(s) in environment: ",
                paste(missing_datasets, collapse = ", ")))
  }

  next_line <- "\n"
  gif_string <- paste0("IFN-", '\u03b3')

  if (nchar(herd_no_character) != 8) {
    stop("Invalid herd number supplied (not equal to 8 characters in length)")} else {
      message("Correct herd number length")
    }

  if (herd_no_character %in% master_tb$herd_no == FALSE) {
    stop("Invalid herd number supplied (herd number is not in master_tb)")} else {
      message("Herd number is contained in master_tb")
    }

  if (herd_no_character %in% bd_df$herd_no == FALSE) {
    message("Herd number is not in BD dataset i.e. has no cases")}

  #search by dates if included
  if (is.null(start_date) & is.null(end_date)) {

    master_tb_tmp <- master_tb %>%
      dplyr::filter(herd_no  %in%  herd_no_character)

    bd_df_tmp <- bd_df %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::mutate(hover_text = paste0("BD no: ", bd_no,
                                        next_line,
                                        "All bTB cases: ", all_cases,
                                        next_line,
                                        "Duration (years): ", bd_duration_yrs,
                                        next_line,
                                        "BD start date: ", format(bd_start, "%d-%B-%Y"),
                                        next_line,
                                        "BD end date: ", format(bd_end, "%d-%B-%Y"),
                                        next_line,
                                        "BD initiated: ", bd_initiated,
                                        next_line,
                                        "First SICTT: ", bd_first_skin_test_type))

    all_cases_tmp <- all_cases_collapsed %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::select(merged_date_min, case, matches("best_estimate_gif_skin_lab"))

  } else if (!is.null(end_date) & is.null(start_date)) {

    end_date_var <- as.Date(end_date, format = "%Y-%m-%d")

    master_tb_tmp <- master_tb %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::filter(fixed_test_date <= end_date_var)

    bd_df_tmp <- bd_df %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::mutate(hover_text = paste0("BD no: ", bd_no,
                                        next_line,
                                        "All bTB cases: ", all_cases,
                                        next_line,
                                        "Duration (years): ", bd_duration_yrs,
                                        next_line,
                                        "BD start date: ", format(bd_start, "%d-%B-%Y"),
                                        next_line,
                                        "BD end date: ", format(bd_end, "%d-%B-%Y"),
                                        next_line,
                                        "BD initiated: ", bd_initiated,
                                        next_line,
                                        "First SICTT: ", bd_first_skin_test_type)) %>%
      dplyr::filter(bd_start <= end_date_var)

    all_cases_tmp <- all_cases_collapsed %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::select(merged_date_min, case, matches("best_estimate_gif_skin_lab")) %>%
      dplyr::filter(merged_date_min <= end_date_var)


  } else if (!is.null(start_date) & is.null(end_date)) {

    start_date_var <- as.Date(start_date, format = "%Y-%m-%d")

    master_tb_tmp <- master_tb %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::filter(fixed_test_date >= start_date_var)

    bd_df_tmp <- bd_df %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::mutate(hover_text = paste0("BD no: ", bd_no,
                                        next_line,
                                        "All bTB cases: ", all_cases,
                                        next_line,
                                        "Duration (years): ", bd_duration_yrs,
                                        next_line,
                                        "BD start date: ", format(bd_start, "%d-%B-%Y"),
                                        next_line,
                                        "BD end date: ", format(bd_end, "%d-%B-%Y"),
                                        next_line,
                                        "BD initiated: ", bd_initiated,
                                        next_line,
                                        "First SICTT: ", bd_first_skin_test_type)) %>%
      dplyr::filter(bd_start >= start_date_var)

    all_cases_tmp <- all_cases_collapsed %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::select(merged_date_min, case, matches("best_estimate_gif_skin_lab")) %>%
      dplyr::filter(merged_date_min >= start_date_var)

  } else if (!is.null(start_date) & !is.null(end_date)) {

    start_date_var <- as.Date(start_date, format = "%Y-%m-%d")
    end_date_var <- as.Date(end_date, format = "%Y-%m-%d")

    master_tb_tmp <- master_tb %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::filter(fixed_test_date >= start_date_var & fixed_test_date <= end_date_var)

    bd_df_tmp <- bd_df %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::mutate(hover_text = paste0("BD no: ", bd_no,
                                        next_line,
                                        "All bTB cases: ", all_cases,
                                        next_line,
                                        "Duration (years): ", bd_duration_yrs,
                                        next_line,
                                        "BD start date: ", format(bd_start, "%d-%B-%Y"),
                                        next_line,
                                        "BD end date: ", format(bd_end, "%d-%B-%Y"),
                                        next_line,
                                        "BD initiated: ", bd_initiated,
                                        next_line,
                                        "First SICTT: ", bd_first_skin_test_type)) %>%
      dplyr::filter(bd_start >= start_date_var & bd_start <= end_date_var)

    all_cases_tmp <- all_cases_collapsed %>%
      dplyr::filter(herd_no  %in%  herd_no_character) %>%
      dplyr::select(merged_date_min, case, matches("best_estimate_gif_skin_lab")) %>%
      dplyr::filter(merged_date_min >= start_date_var & merged_date_min <= end_date_var)

  }




  # want to summarise by date and diagnostic type
  all_cases_tmp_sum <- all_cases_tmp %>%
    dplyr::group_by(merged_date_min, best_estimate_gif_skin_lab) %>%
    dplyr::summarise(no_of_cases_on_day = sum(case), best_estimate_gif_skin_lab_string = first(best_estimate_gif_skin_lab_string)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hover_text_point_in_df = paste0('Date of diagnosis: ', format(merged_date_min, "%d-%B-%Y"), "<br>",
                                                  #format(skin_fixed_test_date, "%d/%m/%Y"),  "<br>",
                                                  'Detection method: ', best_estimate_gif_skin_lab_string, "<br>",
                                                  "No of bTB cases detected: ", no_of_cases_on_day, "<br>"))

  # extract meta data
  #herd_no <- herds_to_filter
  county <- master_tb_tmp$county[1]
  no_of_breakdowns <- nrow(bd_df_tmp)
  herd_no_name <- master_tb_tmp$herd_no[1]

  # herd_type is a little awkward as it can change year to year
  # test herd has herd type
  unique(master_tb_tmp$herd_type_ml_description)
  herd_type_for_herd <- master_tb_tmp %>%
    dplyr::arrange(skin_fixed_test_date_year) %>%
    dplyr::distinct(skin_fixed_test_date_year, herd_type_ml_description) %>%        # remove duplicates within year
    # this is need because the order is important
    dplyr::mutate(group = dplyr::consecutive_id(herd_type_ml_description)) %>%
    dplyr::group_by(group, herd_type_ml_description) %>%
    dplyr::summarise(
      start_year = min(skin_fixed_test_date_year ),
      end_year = max(skin_fixed_test_date_year )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(herd_type_by_year = paste0(start_year, "-", end_year, ": ", herd_type_ml_description))
  herd_type_for_herd_vector <- paste(herd_type_for_herd$herd_type_by_year, collapse = "\n")

  # pull out min/max test dates
  min_skin_test_date <- min(master_tb_tmp$skin_fixed_test_date)
  max_skin_test_date <- max(master_tb_tmp$skin_fixed_test_date)

  bd_df_small <- bd_df_tmp %>%
    dplyr::select(herd_no, bd_no, starts = bd_start, ends = bd_end) %>%
    dplyr::mutate(bd_yes_no = "BD")

  # create a dataset of first and last skin test dates (we can then fill in inbetween)
  df_min <- data.frame(herd_no = c(herd_no_character),
                       bd_no = c(NA),
                       starts = as.Date(c(min_skin_test_date)),
                       ends = as.Date(c(NA)))

  df_max <- data.frame(herd_no = c(herd_no_character),
                       bd_no = c(NA),
                       starts = as.Date(c(NA)),
                       ends = as.Date(c(max_skin_test_date)))

  bd_df_small_summary <- df_min %>%
    dplyr::bind_rows(bd_df_small) %>%
    dplyr::bind_rows(df_max) %>%
    dplyr::mutate(starts = if_else(is.na(starts), lag(ends), starts),
                  ends = if_else(is.na(ends), lead(starts), ends)) %>%
    dplyr::select(-herd_no)

  df2_base <- bd_df_small_summary %>%
    #arrange(starts) %>%
    dplyr::mutate(next_start = lead(starts))

  gap_rows <- df2_base %>%
    dplyr::filter(!is.na(next_start) & next_start > ends) %>%
    dplyr::mutate(
      bd_no = as.numeric(NA),
      starts = ends,
      ends = next_start,
      bd_yes_no = as.character(NA)
    ) %>%
    dplyr::select(names(bd_df_small_summary))

  bd_free_dates <- dplyr::bind_rows(df2_base %>% dplyr::select(names(bd_df_small_summary)), gap_rows) %>%
    dplyr::arrange(starts) %>%
    dplyr::mutate(bd_yes_no = ifelse(is.na(bd_yes_no), "Free", bd_yes_no))

  # if herd has no BDs i.e. constantly free:
  if (nrow(bd_df_tmp) == 0) {
    bd_free_dates <- data.frame(bd_no = NA,
                                starts = min_skin_test_date,
                                ends = max_skin_test_date,
                                bd_yes_no = "Free")
  }

  max_no_animals_vector <- max(master_tb_tmp$total_animals)

  herd_df <- master_tb_tmp %>%
    dplyr::mutate(hover_text_point_in_df = paste0('Skin test date: ', format(skin_fixed_test_date, "%d-%B-%Y"), "<br>",
                                                  #format(skin_fixed_test_date, "%d/%m/%Y"),  "<br>",
                                                  'Skin test type: ', test_type, " (", test_type_description, ")",  "<br>",
                                                  #BD start date (fixed_test_date): ', fixed_test_date,  "<br>",
                                                  "Trading status (on date of skin test): ", trading_status, "<br>",
                                                  'Total animals skin tested: ', total_animals, "<br>",
                                                  'Total reactors: ', total_reactor_skin, "<br>",
                                                  'Total inconclusives: ', total_inconclusive, "<br>",
                                                  'Total slaughter cases (at date closeby): ', total_reactor_slaughter, "<br>",
                                                  "Slaughter detection date (if applicable):", format(test_date_lab, "%d-%B-%Y"), "<br>",
                                                  'Total GIF cases (at date closeby): ', gif_cases, "<br>",
                                                  'GIF date: ', format(gif_actual_date, "%d-%B-%Y"), "<br>"))


  if (!is.null(alternative_herd_no_name)) {
    herd_number <- alternative_herd_no_name
  } else if (is.null(alternative_herd_no_name)) {
    herd_number <- herd_df$herd_no[1]
  }
  no_of_breakdowns <- max(herd_df$bd_no, na.rm = T)

  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "gray80", linewidth = 1) +
    ggplot2::geom_vline(xintercept = as.Date(paste0(format(min(herd_df$skin_fixed_test_date), "%Y"), "-01-01")), colour = "gray80", linewidth = 1) +
    #geom_rect has to be underneath geom_line, otherwise we can see hoover info
    ggplot2::geom_rect(data = bd_free_dates,
                       aes(xmin = starts,
                           xmax = ends,
                           fill = bd_yes_no,
                           ymin = -10,
                           ymax = max_no_animals_vector + 10),
                       inherit.aes = FALSE,
                       #fill = df2$colour_me,
                       alpha = 0.5) +
    ggplot2::geom_line(data = herd_df, aes(x = skin_fixed_test_date, y = total_animals, colour = "Total animals"),
                       # colour = "#1B9E77", # "#76B7B2", #  "deepskyblue4", # "green",
                       linewidth = 3) +
    ggplot2::scale_colour_manual(
      name = "Number of animals at SICTT",
      values = c("Total animals" = "#1B9E77")
    ) +
    ggplot2::geom_point(data = herd_df, aes(x = skin_fixed_test_date, y = total_animals), #, group = 1),
                        # text = paste0('Skin test date: ', format(skin_fixed_test_date, "%d-%B-%Y"), "<br>",
                        #               #format(skin_fixed_test_date, "%d/%m/%Y"),  "<br>",
                        #               'Skin test type: ', test_type,  "<br>",
                        #               'BD start date (fixed_test_date): ', fixed_test_date,  "<br>",
                        #               'Total animals tested: ', total_animals, "<br>",
                        #               'Total reactors: ', total_reactor_skin, "<br>",
                        #               'Total inconclusives: ', total_inconclusive, "<br>",
                        #               'Total slaughter cases: ', total_reactor_slaughter, "<br>",
                        #               'Total GIF cases: ', gif_cases, "<br>",
                        #               'GIF date: ', gif_actual_date, "<br>")),
                        size = 5, colour = scales::alpha("black", 0.4)) + # "#1B9E77") +
    ggnewscale::new_scale_color() +
    # btb cases lollipop style plot
    ggplot2::geom_segment(data = all_cases_tmp_sum, aes(x = merged_date_min, xend = merged_date_min,
                                                        y = 0, yend = no_of_cases_on_day,
                                                        colour = best_estimate_gif_skin_lab_string),
                          linewidth = 2) +
    ggplot2::geom_point(data = all_cases_tmp_sum, aes(x = merged_date_min, y = no_of_cases_on_day,
                                                      colour = best_estimate_gif_skin_lab_string),
                        size = 5) +
    #theme_jamie_dark +
    ggplot2::scale_fill_manual(name = "Breakdown status",
                               values = c(
                                 "Free" = "#D4ED9C",
                                 "BD"  = "#E75A49"
                               )
    ) +
    ggthemes::scale_colour_tableau(name = "Count of bTB cases", palette = "Tableau 10", direction = -1) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      #date_minor_breaks = "1 month",
      date_labels = "%Y"
    ) +
    ggplot2::theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
                   plot.background = element_rect(color = "#4E5D6C", fill = "#4E5D6C"),
                   #x2 = unit(0, 'npc'))),
                   #panel.grid.major = element_blank(),
                   # panel.grid.major = element_line(color = "gray", alpha = 0.1),
                   # panel.grid.minor = element_blank(),
                   #panel.grid.major = element_line(color = "gray", size = 0.5, alpha = 0.1),  # Add alpha to grid lines
                   #panel.grid.minor = element_line(color = "gray", size = 0.2, alpha = 0.1) ,
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_line(linewidth = 0.1, color = scales::alpha("gray80", 0.4)), #color = "gray80"),
                   # explicitly set the horizontal lines (or they will disappear too)
                   panel.grid.major.x = element_line(linewidth = 0.1, color = scales::alpha("gray80", 0.4)), #color = "gray80"),
                   #panel.grid.minor.x = element_line(size = 0.1, color = "white"),
                   # turn off minor gridlines
                   panel.grid.minor.x = element_blank(),
                   plot.caption.position = "plot",
                   plot.title.position = "plot",
                   legend.background = element_rect(fill = "#4E5D6C", colour = NA),
                   # plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
                   text = element_text(color = "white"),
                   plot.subtitle = element_text(colour = "gray80", size = 14),
                   axis.text = element_text(size = 16, face = "bold", colour = "gray80"), # "white"),
                   axis.title = element_text(size = 16, face = "bold", colour = "gray80"),
                   axis.title.y.right = element_text(vjust = 1),
                   legend.text = element_text(size = 16),
                   legend.title = element_text(size = 18),
                   plot.title = element_text(size = 18),
                   plot.caption = element_text(size = 14),
                   axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
    ggplot2::labs(x = "SICTT date",
                  y = "Number of animals at SICTT",
                  title = paste0("Overview of herd-level bTB status for herd: ", herd_no_name),
                  subtitle = paste0("Herd number: ", herd_no_name, #herd_number,
                                    next_line,
                                    "County: ", county,
                                    next_line,
                                    "Total no. of BDs: ", no_of_breakdowns,
                                    next_line,
                                    "Herd-type: ", herd_type_for_herd_vector),
                  caption = paste0("Count of bTB cases: aggregated at day level",
                                   next_line,
                                   "SICTT: Single Intradermal Comparative Tuberculin Test",
                                   next_line,
                                   "Summary  of bTB status, SICTTs performed, number of bTB cases",
                                   next_line,
                                   "whats not included (yet?): ",
                                   next_line,
                                   "- dates of negative ", gif_string, " & laboratory tests",
                                   next_line,
                                   "- actual count of animals from AIM")) +
    # breakdown segments
    ggiraph::geom_rect_interactive(data = bd_df_tmp, mapping = aes(xmin = bd_start,
                                                                   xmax = bd_end,
                                                                   ymin = -10,,
                                                                   ymax = max_no_animals_vector + 10,
                                                                   #fill = t,
                                                                   tooltip = hover_text,
                                                                   #onclick = oc,
                                                                   data_id = hover_text),
                                   #color = "black",
                                   color = NA,
                                   alpha = 0.0001,
                                   linejoin = "bevel",
                                   lineend = "round") +
    # skin test points
    ggiraph::geom_point_interactive(data = herd_df, aes(x = skin_fixed_test_date, y = total_animals,
                                                        tooltip = hover_text_point_in_df, data_id = hover_text_point_in_df),
                                    size = 1.5, hover_nearest = TRUE, alpha = 0.6,
                                    #colour = scales::alpha("black", 0.4)
    ) +
    # btb case points
    ggiraph::geom_point_interactive(data = all_cases_tmp_sum, aes(x = merged_date_min, y = no_of_cases_on_day,
                                                                  tooltip = hover_text_point_in_df, data_id = hover_text_point_in_df),
                                    size = 1.5, hover_nearest = TRUE, alpha = 0.6,
                                    #colour = scales::alpha("black", 0.4)
    )

  # if we dont want tables include, plot straight away
  if (include_tables == FALSE) {
    # plot interactive ggiraph object
    return(ggiraph::girafe(
      ggobj = gg,
      options = list(
        #opts_sizing(width = .7),
        opts_zoom(max = 8),
        opts_toolbar(position = #"bottom",
                       "bottomleft"),
        opts_hover(css = "stroke:red;stroke-width:2;")
      ),
      width_svg = 12, height_svg = 10))
  }

  # if we want plot and tables together:
  if (include_tables == TRUE) {
    bd_df_tmp_gt <- bd_df_tmp |>
      gt::gt() |>
      gt::tab_header(
        title = "Breakdown dataset for herd",
        subtitle = "Summary of BD events"
      ) |>
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
        locations = gt::cells_body(columns = 1)
      )

    master_tb_tmp_gt <- master_tb_tmp |>
      gt::gt() |>
      gt::tab_header(
        title = "master_tb dataset for herd",
        subtitle = "Raw herd-level skin-level of BD events"
      ) |>
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
        locations = gt::cells_body(columns = 1)
      )


    browsable(tagList(
      # plot interactive ggiraph object
      ggiraph::girafe(
        ggobj = gg,
        options = list(
          #opts_sizing(width = .7),
          opts_zoom(max = 8),
          opts_toolbar(position = #"bottom",
                         "bottomleft"),
          opts_hover(css = "stroke:red;stroke-width:2;")
        ),
        width_svg = 12, height_svg = 10),

      # spacer
      div(style = "height: 30px;"),
      # spacer
      div(style = "height: 30px;"),

      # master_tb
      master_tb_tmp_gt,

      # spacer
      div(style = "height: 30px;"),

      # bd_df
      bd_df_tmp_gt

    ))
  }

}




