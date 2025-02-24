#' @title create_tablepre：生成数据预览图
#' @param data a tibble data frame
#' @param decimal_places decimal
#' @param output_file output file
#' @param slice slice x lines, default 20
#' @param select select x columns, default 12
#' @param title title
#' @param caption caption
#' @param fontsize fontsize of text
#' @param addwm if or not add RStata water mark
#' @param data_source data source
#' @importFrom servr httd
#' @importFrom pagedown chrome_print
#' @importFrom pdftools pdf_convert
#' @importFrom rstatatools addrswm
#' @importFrom knitr plot_crop
#' @import ggplot2
#' @import dplyr
#' @import magick
#' @importFrom htmlTable htmlTable
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom ggthemes theme_map
#' @export
create_tablepre <- function(data, decimal_places = 2,
                      output_file = "rstataoutput.png",
                      slice = 20, select = 12,
                      title = "数据概览",
                      caption = "数据处理：微信公众号 RStata",
                      fontsize = 18, addwm = F,
                      data_source = "见介绍") {
  allnames <- colnames(data)
  newselect <- ifelse(ncol(data) < 12, ncol(data), select)
  data <- dplyr::select(data, 1:newselect)
  # 检测数值型变量
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  non_numeric_vars <- names(data)[!sapply(data, is.numeric)]
  subdata <- dplyr::slice_sample(data, n = ifelse(nrow(data) >= 20, 20, nrow(data)))
  dplyr::mutate_at(subdata, non_numeric_vars, as.character) -> subdata
  dplyr::mutate_at(data, non_numeric_vars, as.character) -> data

  # 保存直方图并生成 HTML 图像标签
  histograms_html <- sapply(names(data), function(var) {
    filename <- paste0("rstatahist_", var, ".png")
    if (var %in% numeric_vars) {
      p <- ggplot(data, aes(x = !!sym(var))) +
        geom_histogram(fill = "#66c2a5", color = "#66c2a5") +
        ggthemes::theme_map()
      ggsave(filename, p, width = 3, height = 1)
      return(paste0('<img src="', filename, '" width="150" height="50">'))
    }
    if (var %in% non_numeric_vars) {
      p <- ggplot(subdata, aes(label = !!sym(var), size = sample(1:10, nrow(subdata), replace = T), color = !!sym(var))) +
        ggwordcloud::geom_text_wordcloud(family = cnfont) +
        scale_size_area(max_size = 10) +
        ggthemes::theme_map() +
        scale_color_manual(values = rep(c("#FED90FFF", "#424F46FF", "#D1B271FF", "#FB4122FF",
                                          "#ABC67DFF", "#0363C3FF", "#7A491EFF", "#000000FF", "#FC0209FF",
                                          "#46732EFF"), 4))
      ggsave(filename, p, width = 3, height = 1)
      return(paste0('<img src="', filename, '" width="150" height="50">'))
    }
  })

  # 格式化数值型变量的小数位数
  data_formatted <- dplyr::slice(data, 1:slice)
  data_formatted[numeric_vars] <- lapply(data_formatted[numeric_vars], function(x) round(x, decimal_places))

  # 创建 HTML 表格
  html_table <- htmlTable(
    rbind(
      c(histograms_html, rep("", length(non_numeric_vars))),  # 第一行：直方图
      data_formatted                                         # 其他行：数据
    ),
    header = names(data_formatted),
    rnames = c("", 1:nrow(data_formatted)),
    caption = caption,
    tfoot = paste0("总观测值数：", nrow(data), "; 总变量数：", ncol(data), "<br>除了展示的变量，还包含这些变量：",
                   paste0(allnames, collapse = "、"), "<br> 数据来源：", data_source)
  )
  paste0("<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Table with Histograms</title><style>@font-face {font-family: 'LXGWWenKai';src: url('https://mdniceczx.oss-cn-beijing.aliyuncs.com/LXGWWenKai-Regular.ttf') format('truetype');font-weight: normal;font-style: normal;}table {width: 100%;border-collapse: collapse;font-family: LXGWWenKai;font-size: ", fontsize, "px;}th, td {padding: 10px;text-align: left;border: 1px solid #ddd;}th {background-color: #fff5e3;color: #9b6e23;}tr:nth-child(even) {background-color: #f2f2f2;}tr:nth-child(odd) {background-color: #ffffff;}td:first-child {background-color: #d2af81; font-weight: bold;}h1 {font-family: LXGWWenKai;text-align: center;}</style></head><body><h1> ", title, " </h1>",
         html_table, "</body></html>") -> html_table
  # 将 HTML 表格保存为文件
  writeLines(html_table, "rstataoutput.html")

  # 转换成 PDF 文件
  servr::httd() -> p
  pagedown::chrome_print("rstataoutput.html",
                         "rstataoutput.pdf",
                         options = list(paperWidth = 16, paperHeight = 12))
  p$stop_server()

  # 删除文件
  lapply(fs::dir_ls(regexp = "png"), file.remove) -> tempres
  rm(tempres)

  # 转换成 png 文件
  pdftools::pdf_convert("rstataoutput.pdf",
                        dpi = 300, pages = 1,
                        filenames = output_file)

  # 去除边缘空白
  knitr::plot_crop(output_file)
  
  # 添加水印
  if (addwm == T) {
    rstatatools::addrswm(output_file)
  }

  # 删除文件
  file.remove("rstataoutput.pdf")
  file.remove("rstataoutput.html")

  # 添加白色边框

  # 读取图片
  img <- magick::image_read(output_file)

  # 添加 50 像素宽的白色边框
  img_with_border <- image_border(img, color = "white", geometry = "50x50")

  # 保存图片
  image_write(img_with_border, path = output_file)
}
