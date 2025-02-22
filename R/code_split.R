#' @title 用于拆分 Stata-Markdown 文件的辅助函数
#' @param file file to be splited
#' @import stringr
#' @import dplyr
#' @importFrom purrr map2
#' @export
code_split <- function(file = "main.smd"){
  md_content <- readLines(file)
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
    dplyr::filter(str_detect(value, "\\{stata asis\\}")) %>%
    mutate(value = str_remove_all(value, "```|\\{stata asis\\}(\\s*)\n")) %>%
    mutate(res = purrr::map2(rawid, value, ~writeLines(.y, paste0(asis_dir, "/code_block", .x, ".do")))) -> tempres
  code_blocksdf %>%
    mutate(value = str_remove_all(value, "```|\\{stata\\}|\\{stata asis\\}"),
           value = paste0("set linesize 80\ncap graph close\ncap log close\nlog using logfile_blocks/tempsmddoclog", row_number(), ", replace text nomsg\n", value, "\nlog close\ncap graph export picture_blocks/tempsmddocpic", row_number(), ".png, replace width(4800)"),
           value = paste0(id, "\n", value)) %>%
    pull(value) %>%
    writeLines("temp_smd_dofile.do")
  new_md_content <- paste0(md_content, collapse = "\n")
  for (i in seq_along(code_blocks)) {
    code_block <- code_blocks[i]
    new_md_content <- str_replace_all(new_md_content, stringr::fixed(code_block), paste0("[[CODE_BLOCK_", i, "]]"))
  }
}
