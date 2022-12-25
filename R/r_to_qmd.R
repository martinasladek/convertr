#' Convert R Script into a Quarto file
#'
#' @description Convert a .R file into a .qmd file. Section titles are converted into headings (e.g `# heading -----` as defined by the the shortcut ctrl/cmd + shift + R). Comments are converted into text in between codechunks (unless the comments are within the code, in which case they stay inside of the code chunk).
#'
#' @param input_dir Directory of the R script file that you want to convert
#' @param output_dir Output directory for the new qmd file
#'
#' @import "purrr"
#'
#' @return An R script file
#' @export
#'
#' @examples
#'  \dontrun{
#'  r_to_qmd(
#'    input_dir = "path/to/some_R_script.R",
#'    output_dir = "path/to/new_converted_qmd_file.qmd"
#'   )
#' }
r_to_qmd <- function(input_dir, output_dir){

  r_lines <- data.frame(
    lines = readLines(input_dir)
  )

  ## identify section titles

  r_lines <- r_lines |>
    dplyr::mutate(
      is_title = endsWith(lines, suffix = "---") & lines != "# ---",
      lines = if_else(is_title, paste0("#", lines), lines),
      lines = if_else(is_title, str_remove_all(lines, "--"), lines)
    )

  ## identify comments

  r_lines <- r_lines |>
    dplyr::mutate(
      is_comment = startsWith(lines, prefix = "#") & !is_title,
      lines = if_else(is_comment, str_remove_all(lines, "# "), lines),
      lines = if_else(is_comment, str_remove_all(lines, "#"), lines),
      is_comment = is_comment | is_title,
      section_index = cumsum(is_comment)
    )

  ## define codechunks

  r_lines <- r_lines |>
    dplyr::group_by(section_index) |>
    dplyr::mutate(
      is_space = lines == "",
      codechunk_index = if_else(!is_title & !is_comment & !is_space, section_index, as.integer(0)),
      is_code = codechunk_index != 0,
      code_line_index = if_else(is_code, cumsum(is_code), as.integer(NA))
    ) |>
    dplyr::select(lines, section_index, is_comment, code_line_index) |>
    dplyr::group_split()

  ## process codechunks and add fences

  r_lines <-
    purrr::map(.x = r_lines, .f = process_codechunks) |>
    purrr::reduce(.x = _, .f = rbind.data.frame) |>
    dplyr::pull(lines)

  writeLines(r_lines, output_dir)

}
