#' @title create_titlepic_dark: 创建暗黑风格的标题图
#' @param img image path
#' @param output_file output file
#' @param title title
#' @param addwm if or not add RStata water mark
#' @importFrom servr httd
#' @importFrom pagedown chrome_print
#' @importFrom pdftools pdf_convert
#' @importFrom rstatatools addrswm
#' @importFrom knitr plot_crop
#' @import ggplot2
#' @import dplyr
#' @import magick
#' @export
create_titlepic_dark <- function(img = "https://mdniceczx.oss-cn-beijing.aliyuncs.com/20220925175906.png", title = "推文标题", output_file = paste0("titlepic_", basename(img)), addwm = F) {
  if(!stringr::str_detect(img, "https://")) {
    file.copy(img, paste0(getwd(), "/", basename(img)))
    img <- basename(img)
  }
  if(stringr::str_detect(img, "https://")) {
    download.file(img, paste0(getwd(), "/", basename(img)))
    img <- basename(img)
  }
  # 创建 crop 文件
  file.copy(img, paste0("cropped_", img))
  knitr::plot_crop(paste0("cropped_", img))
  extract_edge_colors(img) -> colorlist
  htmlstr <- paste0("<!DOCTYPE html><html lang=\"zh-CN\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\"content=\"width=device-width, initial-scale=1.0\"><title>示例页面</title><style>@font-face {font-family: 'LXGWWenKai';src: url('https://mdniceczx.oss-cn-beijing.aliyuncs.com/LXGWWenKai-Regular.ttf') format('truetype');font-weight: normal;font-style: normal;}.container{width:fit-content;margin:0 auto;padding:10px;border:0px solid#fff;font-family: LXGWWenKai;}.content{display:flex;width:fit-content;margin:0 auto;padding:10px;border:4px solid#DCDCDC;min-height:500px;height:500px;background-color:", substr(colorlist$bottom_left, 1, 7), ";}.left{flex:2.35;margin:10px;position:relative}.left::after{content:'';position:absolute;top:0;right:-20px;width:4px;height:100%;background-color:#DCDCDC}.left img{width:auto;height:480px;aspect-ratio:attr(width)/attr(height);display:block;margin:0 auto}.right{flex:1;display:flex;flex-direction:column;width:225px;max-width:225px}.right-top{text-align:right;font-size:18px;margin-bottom:10px;margin-left:30px;margin-top:10px;margin-right:10px;color:#D7D7D7;height:30px;}.right-middle{font-size:30px;margin-bottom:5px;text-align:right;margin-left:20px;margin-right:10px;margin-top:160px}.right-bottom img{width:180px;height:auto;display:block;margin-left:auto;margin-right:0px}.right-bottom-text{text-align:right;font-size:18px;margin-top:10px;margin-right:10px;margin-bottom:10px}.dotted-line{width:100%;border-bottom:6px dotted#DCDCDC;margin-top:30px}</style></head><body><div class=\"container\"><!--内容区域--><div class=\"content\"><div class=\"left\"><img class=\"image\"src=\"", paste0("cropped_", img), "\"/></div><div class=\"right\"><div class=\"right-top\">", title, "</div><div class=\"right-middle\"><span style=\"color:#f05c3b\">R</span><span style=\"color:#fed439\">S</span><span style=\"color:#fd7446\">ta</span><span style=\"color:#71d0f5\">ta</span><span style=\"color:#D7D7D7\">&nbsp;学院</span></div><div class=\"right-bottom\"><img class=\"imageqr\"src=\"https://mdniceczx.oss-cn-beijing.aliyuncs.com/%E6%B7%B1%E8%89%B2%E8%83%8C%E6%99%AF%E5%BE%AE%E4%BF%A1%E5%85%AC%E4%BC%97%E5%8F%B7%E4%BA%8C%E7%BB%B4%E7%A0%81.png\"/><div class=\"right-bottom-text\"style=\"color:#f05c3b\">👆扫码进入<br>RStata学院～</div></div></div></div><!--点状虚线--><div class=\"dotted-line\"></div></div></body></html>")
  # 将 HTML 表格保存为文件
  writeLines(htmlstr, "rstata_titlepic.html")

  # 转换成 PDF 文件
  servr::httd() -> p
  pagedown::chrome_print("rstata_titlepic.html",
                         "rstata_titlepic.pdf",
                         options = list(paperWidth = 16, paperHeight = 12))
  p$stop_server()

  # 转换成 png 文件
  pdftools::pdf_convert("rstata_titlepic.pdf",
                        dpi = 300,
                        filenames = output_file)

  # 去除边缘空白
  knitr::plot_crop(output_file)

  # 添加水印
  if (addwm == T) {
    rstatatools::addrswm(output_file)
  }

  # 删除文件
  file.remove("rstata_titlepic.pdf")
  file.remove("rstata_titlepic.html")
  file.remove(paste0("cropped_", img))
}
