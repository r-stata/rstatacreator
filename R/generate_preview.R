#' @title generate_preview: 针对含有 readme.txt 的文件夹生成所需的全部图片截图
#' @param dir directory path
#' @param addwm if or not add RStata water mark
#' @param decimal_places decimal_places
#' @importFrom haven read_dta
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom readr read_rds
#' @importFrom tools file_ext
#' @importFrom tools file_path_sans_ext
#' @importFrom rstatatools addrswm
#' @importFrom reticulate py_run_string
#' @importFrom dplyr as_tibble
#' @export
generate_preview <- function(dir, addwm = F, decimal_places = 0) {
  stopifnot(file.exists(paste0(dir, "/readme.txt")))
  yaml::read_yaml(paste0(dir, "/readme.txt")) -> lst

  # 添加水印
  if (addwm == T & !file.exists(paste0(dir, "/test.txt"))) {
    lapply(fs::dir_ls(dir, regexp = "png", recurse = T), rstatatools::addrswm, mode = lst$scheme) -> tempres
    writeLines("水印已添加！", paste0(dir, "/test.txt"))
  }

  # 创建文件夹保存
  dir.create(paste0(dir, "/Preview"))

  # 创建封面图
  if (!file.exists(paste0(dir, "/Preview/duanshu_title.png"))) {
    create_coverpage(img = paste0(dir, "/", lst$coverpic),
                     title = lst$title,
                     subtitle = lst$subtitle)

    # 拷贝到目标文件夹
    file.copy("wechat_title.png", paste0(dir, "/Preview/wechat_title.png"), overwrite = T)
    file.copy("duanshu_title.png", paste0(dir, "/Preview/duanshu_title.png"), overwrite = T)
  }

  # 创建标题图
  if(!file.exists(paste0(dir, "/Preview/titlepic.png"))) {
    if (lst$scheme == "light") {
      create_titlepic(img = paste0(dir, "/", lst$titlepic), title = lst$fulltitle)
    }
    if (lst$scheme == "dark") {
      create_titlepic_dark(img = paste0(dir, "/", lst$titlepic), title = lst$fulltitle)
    }

    file.copy(paste0("titlepic_", basename(lst$titlepic)), paste0(dir, "/Preview/titlepic.png"), overwrite = T)
  }

  # 创建数据预览图
  for(datapath in lst$`data-preview`) {
    if (!file.exists(paste0(dir, "/Preview/数据预览_", tools::file_path_sans_ext(basename(datapath)), ".png"))) {
      if (tools::file_ext(datapath) == "dta") {
        if (file.size(paste0(dir, "/", datapath)) <= 1024000000) {
          haven::read_dta(paste0(dir, "/", datapath)) -> mydata
        } else {
          reticulate::py_run_string(paste0("import pandas as pd
data = pd.read_stata('", paste0(dir, "/", datapath), "')
")) -> py
          as_tibble(py$data) -> mydata
        }
      }
      if (tools::file_ext(datapath) %in% c("xlsx", "xls")) {
        readxl::read_excel(paste0(dir, "/", datapath)) -> mydata
      }
      if (tools::file_ext(datapath) == "csv") {
        readr::read_csv(paste0(dir, "/", datapath)) -> mydata
      }
      if (tools::file_ext(datapath) == "rds") {
        readr::read_rds(paste0(dir, "/", datapath)) -> mydata
      }
      create_tablepre(mydata, title = tools::file_path_sans_ext(basename(datapath)),
                      data_source = lst$`data-source`, decimal_places = decimal_places,
                      output_file = paste0(tools::file_path_sans_ext(basename(datapath)), ".png"), addwm = T)
      file.copy(paste0(tools::file_path_sans_ext(basename(datapath)), ".png"), paste0(dir, "/Preview/数据预览_", tools::file_path_sans_ext(basename(datapath)), ".png"), overwrite = T)
    }
  }

  # 添加 RStata 标记
  create_trade_marker(dir)
}
