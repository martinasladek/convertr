#' Convert a Quarto file into an R script
#'
#' @description Convert a .qmd file into a .R file. Headings are converted into section titles (e.g `# heading -----` as defined by the the shortcut ctrl/cmd + shift + R). Text outside of code chunks is converted into comments. Chunk options defined by `#|` are retained as comments.
#'
#' @param input_dir Directory of the qmd file that you want to convert
#' @param output_dir Output directory for the new R script
#'
#' @return A .qmd file
#' @import "dplyr" "tidyr" "stringr"
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'  \dontrun{
#'  qmd_to_r(
#'    input_dir = "path/to/some_quarto_file.qmd",
#'    output_dir = "path/to/new_converted_r_script.R"
#'   )
#' }
#'
qmd_to_r <- function(input_dir, output_dir){

  # read in a quarto file

  qmd_lines <-  data.frame(
    lines = readLines(input_dir)
  )

  # identify the YAML header

  qmd_lines <- qmd_lines |>
    dplyr::mutate(
      is_yaml = stringr::str_detect(lines, "---"),
      is_yaml = dplyr::if_else(is_yaml, cumsum(is_yaml), as.integer(0)),
      is_yaml = dplyr::row_number() <= which(is_yaml == 2),
      lines = if_else(is_yaml, paste0("# ", lines), lines)
    )

  # identify codechunks

  qmd_lines <- qmd_lines |>
    dplyr::mutate(
      end_fence = lines == "```",
      start_fence = startsWith(lines, "```{"),
      is_fence = end_fence == 1 | start_fence == 1,
      fence_index_start = dplyr::if_else(start_fence, cumsum(start_fence), as.integer(NA)),
      fence_index_end = dplyr::if_else(end_fence, cumsum(end_fence), as.integer(NA))
    ) |>
    tidyr::fill(fence_index_start) |>
    tidyr::fill(fence_index_end, .direction = "up") |>
    dplyr::mutate(
      code_chunk_index = dplyr::case_when(fence_index_start == fence_index_end ~ fence_index_start, TRUE ~ as.integer(0)),
      is_code = dplyr::if_else(code_chunk_index == 0, FALSE, TRUE)
    )

  # identify title and convert into script sections

  qmd_lines <- qmd_lines |>
    dplyr::mutate(
      is_title = dplyr::if_else(startsWith(x = lines, "#") & !is_code & !is_yaml, TRUE, FALSE),
      lines = dplyr::if_else(is_title, str_replace_all(lines, "#", ""), lines),
      n_fill = dplyr::if_else(is_title, 72 - nchar(lines), 0)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      lines = dplyr::if_else(is_title, paste0("# ", lines, " ", paste0(rep("-", n_fill), collapse = "")), lines),
      lines = dplyr::if_else((lines != "" & !is_code & !is_title & !is_yaml), paste0("# ", lines), lines)
    ) |>
    dplyr::filter(!is_fence) |>
    dplyr::pull(lines)

  # save into output dir

  writeLines(qmd_lines, output_dir)

}
