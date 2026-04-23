#' Install RStudio cvera snippets for cvera package
#'
#' Copies the package-provided R snippets into the user's RStudio snippets file.
#' If a previous version of the snippets block exists, it will be replaced.
#'
#' @return Invisible NULL
#' @examples
#' \dontrun{
#' if(interactive()){
#' install_cvera_snippets()
#'  }
#' }
#' @export
#'
#'
#'
install_cvera_snippets <- function() {
  src <- system.file("snippets", "cvera_snippets.txt", package = "cvera")

  if (src == "") {
    stop("Snippet file not found in package.")
  }

  # --- detect RStudio snippets path ---
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {

    if (rstudioapi::versionInfo()$version < "1.3") {
      base_path <- file.path(path.expand("~"), ".R", "snippets")
    } else {
      if (.Platform$OS.type == "windows") {
        base_path <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
      } else {
        base_path <- file.path(path.expand("~"), ".config", "rstudio", "snippets")
      }
    }

  } else {
    base_path <- file.path(path.expand("~"), ".config", "rstudio", "snippets")
  }

  dir.create(base_path, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(base_path, "r.snippets")

  # --- read files ---
  new_lines <- readLines(src, warn = FALSE)
  old_lines <- if (file.exists(dest)) readLines(dest, warn = FALSE) else character()

  start_marker <- "# >>> cverasnippets start >>>"
  end_marker   <- "# >>> cverasnippets end >>>"

  # --- remove existing block if present ---
  start_idx <- grep(start_marker, old_lines, fixed = TRUE)
  end_idx   <- grep(end_marker, old_lines, fixed = TRUE)

  if (length(start_idx) > 0 && length(end_idx) > 0 && end_idx > start_idx) {
    old_lines <- old_lines[-seq(start_idx, end_idx)]
  }

  # --- combine: keep existing + clean insert ---
  updated <- c(
    old_lines,
    "",
    new_lines
  )

  writeLines(updated, dest)

  message("Snippets installed (existing block replaced if present).")
}

