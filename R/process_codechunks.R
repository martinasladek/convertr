#' Process Code Chunks from an R Script
#'
#' @description Internal function to add fences to code chunks when converting from R scripts. Used by `r_to_qmd()`. Not exported.
#' @param r_lines_i r_lines file without fences.
#'
#' @keywords Internal
#' @return Modified r_lines object tagged with code chunk information
#'
#'
process_codechunks <- function(r_lines_i){

  if(all(is.na(r_lines_i$code_line_index))){
    return(r_lines_i)
  } else {

    section_index_i <- r_lines_i$section_index[1]
    min_code_line <- min(r_lines_i$code_line_index, na.rm = TRUE)
    max_code_line <- max(r_lines_i$code_line_index, na.rm = TRUE)

    r_lines_i <- r_lines_i |>
      dplyr::add_row(
        lines = "```{r}",
        .before = which(r_lines_i$code_line_index == min_code_line)
      )

    r_lines_i <- r_lines_i |>
      dplyr::add_row(
        lines = "```",
        .after = which(r_lines_i$code_line_index == max_code_line)
      )


    r_lines_i <- r_lines_i |>
      dplyr::add_row(
        lines = "",
        .after = which(r_lines_i$is_comment)
      )

    return(r_lines_i)

  }
}
