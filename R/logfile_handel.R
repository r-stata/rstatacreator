#' @title 用于处理 Stata-Markdown 文件编译的后续程序
#' @param file original filename
#' @param mdfile new_md_content filename
#' @param codefile code_blocks filename
#' @param delete_pic 是否删除中间生成的图片
#' @param pandoc_dir pandoc_dir
#' @import stringr
#' @import dplyr
#' @import fs
#' @import rmarkdown
#' @importFrom rmarkdown render
#' @importFrom tools file_path_sans_ext
#' @importFrom readr read_rds
#' @export
logfile_handel <- function(file = "main.smd",
                           mdfile = "new_md_content.rds",
                           codefile = "code_blocks.rds",
                           delete_pic = F,
                           pandoc_dir = rmarkdown::find_pandoc()$dir) {
  stopifnot(file.exists("logfile_blocks"))
  stopifnot(file.exists(mdfile))
  stopifnot(file.exists(codefile))
  read_rds("new_md_content.rds") -> new_md_content
  read_rds("code_blocks.rds") -> code_blocks
  str_remove_all(code_blocks, "\\[\\[ROW_(\\d{1,})\\]\\]--") -> code_blocks

  for (i in seq_along(code_blocks)) {
    code_block <- code_blocks[i]
    new_md_content <- str_replace_all(new_md_content, stringr::fixed(code_block), paste0("[[CODE_BLOCK_", i, "]]"))
  }

  for(x in fs::dir_ls("logfile_blocks")) {
    readLines(x) %>%
      paste0(collapse = "\n") %>%
      str_replace_all("///(\\s*)\n> ", "/// \n\\. ") %>%
      str_remove_all("\n> ") %>%
      str_split_1("\\n") %>%
      as_tibble() %>%
      dplyr::filter(value != "" & value != ". " & value != ". log close") %>%
      mutate(value = if_else(str_detect(value, "^\\. "),
                             paste0(str_remove(value, "^\\. ")),
                             paste0("*> ", value))) %>%
      pull(value) %>%
      writeLines(x)
  }
  # add water mark
  lapply(fs::dir_ls("picture_blocks", regexp = "png"),
         rstatatools::addrswm)
  for (i in 1:length(code_blocks)) {
    asis_blocks <- ""
    picture_blocks <- ""
    logfile_blocks <- ""
    if (file.exists(paste0("asis_blocks/code_block", i, ".do"))) {
      asis_blocks <- paste0("```stata\n", str_trim(paste0(readLines(paste0("asis_blocks/code_block", i, ".do")), collapse = "\n")), "\n```")
    }
    if (file.exists(paste0("picture_blocks/tempsmddocpic", i, ".png"))) {
      picture_blocks <- paste0("![](", paste0("picture_blocks/tempsmddocpic", i, ".png"), ")")
    }
    if (!file.exists(paste0("asis_blocks/code_block", i, ".do"))) {
      logfile_blocks <- paste0("```stata\n", str_trim(paste0(readLines(paste0("logfile_blocks/tempsmddoclog", i, ".log")), collapse = "\n")), "\n```")
    }

    new_md_content <- str_replace(new_md_content, paste0("\\[\\[CODE_BLOCK_", i, "\\]\\]"), paste0(asis_blocks, "\n", logfile_blocks, "\n\n", picture_blocks))
  }
  writeLines(new_md_content, paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
  try({
    if (rmarkdown::pandoc_available()) {
      rmarkdown::render(paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
    }
    if (!rmarkdown::pandoc_available()) {
      message("如果没有生成对应的 html 文件，需要使用 RStudio 手动编译！")
    }
  })
  if (delete_pic == T) {
    fs::dir_delete("asis_blocks")
    fs::dir_delete("logfile_blocks")
    fs::dir_delete("picture_blocks")
    file.remove("temp_smd_dofile.do")
    file.remove("new_md_content.rds")
    file.remove("code_blocks.rds")
  }
  if (delete_pic == F) {
    fs::dir_delete("asis_blocks")
    fs::dir_delete("logfile_blocks")
    file.remove("temp_smd_dofile.do")
    file.remove("new_md_content.rds")
    file.remove("code_blocks.rds")
  }
}
