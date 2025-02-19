#' @title create_readme: 创建 readme.txt 模板文件
#' @export
create_readme <- function(){
  writeLines("scheme:
fulltitle:
title:
subtitle:
titlepic:
coverpic:
-

data-preview:
-

data-source:
", "readme.txt")
  file.edit("readme.txt")
  message(paste0("在 ", getwd(), " 文件夹下创建了 readme.txt 文件!"))
}
