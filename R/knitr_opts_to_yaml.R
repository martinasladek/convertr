
#' Move Knitr Options from .rmd Setup Chunk into .qmd YAML
#'
#' @description Take an existing .rmd file and convert its YAML header options to be compatible with Quarto. Currently minimal functionality. html output options are automatically set to `self-contained: TRUE`. Underscores are replaced with hyphens for all other options.
#'
#' @param input_dir Input directory for an old .rmd file
#' @param output_dir Output directory for a .qmd file with a converted YAML header
#'
#' @import "tibble"
#'
#' @return .qmd file
#' @export
#'
#' @examples
#'  \dontrun{
#'  knitr_opts_to_yaml(
#'    input_dir = "path/to/some_an_old_rmd_file.rmd",
#'    output_dir = "path/to/new_qmd_file.qmd"
#'   )
#' }
knitr_opts_to_yaml <- function(input_dir, output_dir){

  rmd_lines <- tibble::tibble(
    lines = readLines(input_dir)
  )

  # identify existing yaml lines

  rmd_lines <- rmd_lines |>
    dplyr::mutate(
      is_yaml = str_detect(lines, "---"),
      is_yaml = if_else(is_yaml, cumsum(is_yaml), as.integer(0)),
      is_yaml = row_number() <= which(is_yaml == 2)
    )

  rmd_main <- rmd_lines |>
    dplyr::filter(!is_yaml) |>
    dplyr::select(lines)

  rmd_yaml <- rmd_lines |>
    dplyr::filter(is_yaml) |>
    dplyr::select(lines)

  # get options

  opts_chunk_main_line <- rmd_lines |>
    dplyr::filter(startsWith(lines, "knitr::opts_chunk$set"))


  opts_chunk_lines <- opts_chunk_main_line |>
    dplyr::mutate(
      lines = str_remove_all(lines, pattern = "knitr::opts_chunk\\$set\\(") |>
        str_remove_all(string = _, pattern = "\\)") |>
        #str_replace_all(string = _, pattern = "=", replacement = ":") |>
        str_remove_all(string = _, pattern = " ") |>
        str_replace_all(string = _, pattern = "=", replacement = ": ") |>
        tolower()
    ) |>
    dplyr::pull(lines)

  opt_yaml_lines <- opts_chunk_lines |>
    stringr::str_split(string = _, pattern = ",") |>
    unlist()

  opt_yaml_lines <- tibble::tibble(
    lines = c("knitr:", "  opts_chunk:", paste0("    ", opt_yaml_lines))
  )

  # merge with existing yaml (append to the end before the final fence)

  qmd_yaml <- rmd_yaml |>
    dplyr::add_row(
      lines = opt_yaml_lines$lines,
      .before = nrow(rmd_yaml)
    )

  # change output options

  # Identify output:

  qmd_yaml <- qmd_yaml |>
    dplyr::mutate(
      is_fence = lines == "---",
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      section = str_split(lines, ":"),
      section = if_else(startsWith(lines, " "), as.character(NA), section[1])
    ) |>
    dplyr::ungroup() |>
    tidyr::fill(section)

  # edit output section

  qmd_yaml <- qmd_yaml |>
    dplyr::mutate(
      lines = if_else(section == "output", str_replace_all(lines, "html_document", ""), lines),
      lines = if_else(section == "output", str_replace_all(lines, "_", "-"), lines)
    )

  output_line = qmd_yaml |>
    dplyr::mutate(row = 1:nrow(qmd_yaml)) |>
    dplyr::filter(section == "output" & str_detect(lines, "output:")) |>
    dplyr::pull(row)

  qmd_yaml <- qmd_yaml |>
    dplyr::add_row(
      lines = "  html: ", is_fence = FALSE, section = "output",
      .after = output_line
    )

  qmd_yaml <- qmd_yaml |>
    dplyr::add_row(
      lines = "    self-contained: true", is_fence = FALSE, section = "output",
      .after = output_line + 1
    )

  qmd_yaml <- qmd_yaml |>
    dplyr::filter(!str_detect(lines, "  :"))

  rmd_lines <- qmd_yaml |>
    dplyr::bind_rows(
      x = _, rmd_main
    ) |>
    dplyr::filter(
      lines != opts_chunk_main_line$lines
    ) |>
    dplyr::pull(lines)

  writeLines(rmd_lines, output_dir)


}
