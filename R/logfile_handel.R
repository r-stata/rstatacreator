#' @title 用于处理 Stata-Markdown 文件编译的后续程序
#' @param file filename
#' @param delete_pic 是否删除中间生成的图片
#' @import stringr
#' @import dplyr
#' @import fs
#' @importFrom rmarkdown render
#' @importFrom tools file_path_sans_ext
#' @export
logfile_handel <- function(file = "main.smd",
                           delete_pic = T) {
  stopifnot(file.exists("logfile_blocks"))

  md_content <- readLines(file)
  code_blocks <- str_extract_all(paste(md_content, collapse = "\n"), "```[\\s\\S]*?```")[[1]]
  code_blocks[str_detect(code_blocks, "\\{stata")] -> code_blocks

  new_md_content <- paste0(md_content, collapse = "\n")
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
  rmarkdown::render(paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
  if (delete_pic == T) {
    fs::dir_delete("asis_blocks")
    fs::dir_delete("logfile_blocks")
    fs::dir_delete("picture_blocks")
    file.remove("temp_smd_dofile.do")
    file.remove(paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
  }
  if (delete_pic == F) {
    fs::dir_delete("asis_blocks")
    fs::dir_delete("logfile_blocks")
    file.remove("temp_smd_dofile.do")
    file.remove(paste0(tools::file_path_sans_ext(basename(file)), ".Rmd"))
  }
}
