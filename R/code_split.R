#' @title 用于拆分 Stata-Markdown 文件的辅助函数
#' @param file file to be splited
#' @import stringr
#' @import dplyr
#' @importFrom purrr map2
#' @importFrom readr write_rds
#' @export
code_split <- function(file = "main.smd"){
  # delete pre-files
  try({
    fs::dir_delete("asis_blocks")
    fs::dir_delete("logfile_blocks")
    fs::dir_delete("picture_blocks")
    file.remove("temp_smd_dofile.do")
    file.remove(paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
    file.remove("new_md_content.rds")
    file.remove("code_blocks.rds")
  })
  md_content <- readLines(file)
  md_content %>%
    as_tibble() %>%
    mutate(value = paste0("[[ROW_", row_number(), "]]--", value)) %>%
    pull(value) -> md_content

  code_blocks <- str_extract_all(paste(md_content, collapse = "\n"), "```[\\s\\S]*?```")[[1]]
  output_dir <- "logfile_blocks"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  picture_dir <- "picture_blocks"
  if (!dir.exists(picture_dir)) {
    dir.create(picture_dir)
  }
  asis_dir <- "asis_blocks"
  if (!dir.exists(asis_dir)) {
    dir.create(asis_dir)
  }
  code_blocks[str_detect(code_blocks, "\\{stata")] -> code_blocks
  code_blocks %>%
    as_tibble() %>%
    mutate(rawid = row_number(),
           id = paste0("\n\n*- [[CODE_BLOCK_", row_number(), "]]")) %>%
    select(rawid, id, everything()) -> code_blocksdf
  code_blocksdf %>%
    filter(str_detect(value, "\\{stata asis\\}")) %>%
    mutate(value = str_remove_all(value, "```|\\{stata asis\\}(\\s*)\n"),
           value = str_remove_all(value, "\\[\\[ROW_(\\d{1,})\\]\\]--")) %>%
    mutate(res = map2(rawid, value, ~writeLines(.y, paste0(asis_dir, "/code_block", .x, ".do")))) -> tempres
  code_blocksdf %>%
    mutate(value = str_remove_all(value, "```|\\{stata\\}|\\{stata asis\\}"),
           value = paste0("set linesize 255\ncap graph close\ncap log close\nlog using logfile_blocks/tempsmddoclog", row_number(), ", replace text nomsg\n", value, "\nlog close\ncap graph export picture_blocks/tempsmddocpic", row_number(), ".png, replace width(4800)"),
           value = paste0(id, "\n", value),
           value = str_remove_all(value, "\\[\\[ROW_(\\d{1,})\\]\\]--")) %>%
    pull(value) %>%
    writeLines("temp_smd_dofile.do")
  new_md_content <- paste0(md_content, collapse = "\n")
  for (i in seq_along(code_blocks)) {
    code_block <- code_blocks[i]
    new_md_content <- str_replace_all(new_md_content, stringr::fixed(code_block), paste0("[[CODE_BLOCK_", i, "]]"))
  }
  new_md_content %>%
    str_remove_all("\\[\\[ROW_(\\d{1,})\\]\\]--") -> new_md_content
  new_md_content %>%
    write_rds("new_md_content.rds")
  code_blocks %>%
    write_rds("code_blocks.rds")
}
