qmd_to_r <- function(input_dir, output_dir){
  
  # read in a quarto file
  
  qmd_lines <-  data.frame(
    lines = readLines(input_dir)
  )
  
  # identify the YAML header
  
  qmd_lines <- qmd_lines |> 
    dplyr::mutate(
      is_yaml = str_detect(lines, "---"), 
      is_yaml = if_else(is_yaml, cumsum(is_yaml), as.integer(0)), 
      is_yaml = row_number() <= which(is_yaml == 2),
      lines = if_else(is_yaml, paste0("# ", lines), lines)
    ) 
  
  # identify codechunks
  
  qmd_lines <- qmd_lines |> 
    dplyr::mutate(
      end_fence = lines == "```",
      start_fence = startsWith(lines, "```{"), 
      is_fence = end_fence == 1 | start_fence == 1,
      fence_index_start = if_else(start_fence, cumsum(start_fence), as.integer(NA)), 
      fence_index_end = if_else(end_fence, cumsum(end_fence), as.integer(NA))
    ) |> 
    tidyr::fill(fence_index_start) |> 
    tidyr::fill(fence_index_end, .direction = "up") |> 
    dplyr::mutate(
      code_chunk_index = case_when(fence_index_start == fence_index_end ~ fence_index_start, TRUE ~ as.integer(0)), 
      is_code = if_else(code_chunk_index == 0, FALSE, TRUE)
    ) 
  
  # identify title and convert into script sections 
  
  qmd_lines <- qmd_lines |> 
    dplyr::mutate(
      is_title = if_else(startsWith(x = lines, "#") & !is_code & !is_yaml, TRUE, FALSE), 
      lines = if_else(is_title, str_replace_all(lines, "#", ""), lines), 
      n_fill = if_else(is_title, 72 - nchar(lines), 0)) |> 
    dplyr::rowwise() |> 
    dplyr::mutate( 
      lines = if_else(is_title, paste0("# ", lines, " ", paste0(rep("-", n_fill), collapse = "")), lines), 
      lines = if_else((lines != "" & !is_code & !is_title & !is_yaml), paste0("# ", lines), lines)
    ) |>
    dplyr::filter(!is_fence) |> 
    dplyr::pull(lines) 
  
  # save into output dir
  
  writeLines(qmd_lines, output_dir)
  
}
