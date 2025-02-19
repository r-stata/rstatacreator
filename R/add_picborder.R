#' @title add_picborder：添加 RStata 图表边框
#' @param img image path
#' @param output_file output file
#' @param title title
#' @param subtitle subtitle
#' @param caption caption
#' @param addwm if or not add RStata water mark
#' @importFrom servr httd
#' @importFrom pagedown chrome_print
#' @importFrom pdftools pdf_convert
#' @importFrom rstatatools addrswm
#' @importFrom knitr plot_crop
#' @import magick
#' @export
add_picborder <- function(img = "https://mdniceczx.oss-cn-beijing.aliyuncs.com/20230421232743.png", output_file = paste0("withborder_", basename(img)),
                         title = "标题",
                         subtitle = "副标题",
                         caption = "数据来源",
                         addwm = F) {
  if(!stringr::str_detect(img, "https://")) {
    file.copy(img, paste0(getwd(), "/", basename(img)))
    img <- basename(img)
  }
  htmlstr <- paste0("<!DOCTYPE html><html lang=\"zh-CN\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\"content=\"width=device-width, initial-scale=1.0\"><title>RStata图表边框</title><style>@font-face{font-family:'LXGWWenKai';src:url('https://mdniceczx.oss-cn-beijing.aliyuncs.com/LXGWWenKai-Regular.ttf')format('truetype');font-weight:normal;font-style:normal}body{font-family:LXGWWenKai;display:flex;justify-content:center;align-items:center;height:100vh;margin:0;background-color:#ffffff}.box{width: 400px;background-color: #ffffff;border-color: #f0f0f0;border-width: 3px;border-style: solid;border-radius: 15px;padding: 20px;position: relative;}.title{font-size:24px;font-weight:bold;margin-bottom:10px}.subtitle{font-size:16px;color:#666666;margin-bottom:30px}.qr-code{position:absolute;top:20px;right:20px;width:80px;height:80px;background-color:#e0e0e0;border-radius:10px}.imageraw {width: auto;height: auto;}.image{width: auto;height: auto;vertical-align: center;border-radius: 5px;border-color: #f0f0f0;border-width: 1px;border-style: solid;background-color: #ffffff;max-width: 100%;}.imageqr {width: auto;height: auto;vertical-align: center;border-radius: 0px;border-color: #ffffff;border-width: 1px;border-style: solid;max-width: 100%;}.footer{font-size:14px;color:#888888;margin-bottom:10px}.footer:last-child{margin-bottom:0}.bottom-text{text-align:right;horizontal-align:center;font-size:12px;color:#aaaaaa;align-items:stretch;display:flex;margin-top:10px}.dashed-line{flex-grow:1;border-top:3px dotted#cccccc;margin-right:10px;margin-top:5px}</style></head><body><div class=\"box\"><div class=\"title\">", title, "</div><div class=\"subtitle\">", subtitle, "</div><div class=\"qr-code\"><img class=\"imageqr\"src=\"https://mdniceczx.oss-cn-beijing.aliyuncs.com/%E7%9F%AD%E4%B9%A6%E5%B9%B3%E5%8F%B0.png\"/></div><div class=\"imageraw\"><img class=\"image\"src=\"", img, "\"/></div><div class=\"bottom-text\"><div class=\"dashed-line\"></div><span style=\"color:#f05c3b\">R</span><span style=\"color:#fed439\">S</span><span style=\"color:#fd7446\">ta</span><span style=\"color:#71d0f5\">ta</span><span style=\"color:#8a9197\">&nbsp;学院</span></div><div class=\"footer\">数据来源：", caption, "</div></div></body></html>")
  # 将 HTML 表格保存为文件
  writeLines(htmlstr, "rstataoutputpic.html")

  # 转换成 PDF 文件
  servr::httd() -> p
  pagedown::chrome_print("rstataoutputpic.html",
                         "rstataoutputpic.pdf",
                         options = list(paperWidth = 16, paperHeight = 12))
  p$stop_server()

  # 转换成 png 文件
  pdftools::pdf_convert("rstataoutputpic.pdf",
                        dpi = 300,
                        filenames = output_file)

  # 去除边缘空白
  knitr::plot_crop(output_file)

  # 添加水印
  if (addwm == T) {
    rstatatools::addrswm(output_file)
  }

  # 删除文件
  file.remove("rstataoutputpic.pdf")
  file.remove("rstataoutputpic.html")
}
