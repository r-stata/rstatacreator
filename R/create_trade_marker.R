#' @title create_trade_marker: 在文件夹的每个子文件夹里面都创建版权标志
#' @param dir directory
#' @export
create_trade_marker <- function(dir) {
  file.copy("/Users/ac/Documents/rstata.png", paste0(dir, "/rstata.png"))
  file.copy("/Users/ac/Documents/版权声明.pdf", paste0(dir, "/版权声明.pdf"))

  lapply(fs::dir_ls(dir, recurse = T, type = "directory"), function(f){
    file.copy("/Users/ac/Documents/rstata.png", paste0(f, "/rstata.png"))
    file.copy("/Users/ac/Documents/版权声明.pdf", paste0(f, "/版权声明.pdf"))
  })
}
