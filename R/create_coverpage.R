#' @title create_coverpage: 创建 RStata 推文的封面图，16:9 和 2.35:1 的两种结果
#' @param img image paths list, less then 3
#' @param title title
#' @param subtitle subtitle
#' @param backgroup backgroup image
#' @param addwm if or not add RStata water mark
#' @param output_file output file
#' @importFrom servr httd
#' @importFrom pagedown chrome_print
#' @importFrom pdftools pdf_convert
#' @importFrom rstatatools addrswm
#' @importFrom knitr plot_crop
#' @import magick
#' @export
create_coverpage <- function(img = NULL, title = NULL,
                             subtitle = NULL, background = img[1],
                             addwm = F,
                             output_file = "title.png"){
  stopifnot(length(img) <= 3)
  classtab <- c("one", "two", "three")

  # 将文件拷贝到工作目录下
  lapply(img, function(f){
    file.copy(f, paste0(getwd(), "/", basename(f)))
  }) -> tempres
  img <- basename(img)

  text <- ifelse(length(img) == 1, paste0("<div class=\"text\"><h1>", title, "</h1><p>", subtitle, "</p></div>"), "")
  imgtext <- paste0(paste0("<div class=\"image-wrapper\"><img src=\"", img, "\"></div>"), collapse = "")
  for (c in c("wechat", "duanshu")) {
    classtab2 <- c("wechat" = 2.35, "duanshu" = 1.7778)
    htmlstr <- paste0("<!DOCTYPE html><html lang=\"zh-CN\"><head><meta charset=\"UTF-8\"><meta name=\"viewport\"content=\"width=device-width, initial-scale=1.0\"><style>@font-face{font-family:'LXGWWenKai';src:url('https://mdniceczx.oss-cn-beijing.aliyuncs.com/LXGWWenKai-Regular.ttf')format('truetype');font-weight:normal;font-style:normal}*{margin:0;padding:0;box-sizing:border-box}body{padding:20px;background:#ffffff;font-family:LXGWWenKai}.container{width:100%;max-width:800px;margin:20px auto;background:#fff;overflow:hidden}.aspect-ratio-box{position:relative;width:100%;padding-top:calc(100%/", classtab2[c], ");overflow:hidden}.backdrop-layer{position:absolute;top:0;left:0;width:100%;height:100%;z-index:1;display:grid;gap:10px;padding:10px}.backdrop-image{position:absolute;top:-50px;left:-50px;right:-50px;bottom:-50px;background-size:cover;background-position:center;filter:blur(30px)brightness(0.9);z-index:1}.image-grid{position:absolute;top:0;left:0;width:100%;height:100%;padding:10px;display:grid;gap:10px;z-index:2}.image-wrapper{width:100%;height:100%;display:flex;justify-content:center;align-items:center;background:rgba(255,255,255,0.1);backdrop-filter:blur(5px);border-radius:4px;overflow:hidden}.image-wrapper img{max-width:100%;max-height:100%;object-fit:contain;transition:transform 0.3s ease}.image-grid.one{grid-template-columns:1fr 1fr}.image-grid.two{grid-template-columns:1fr 1fr}.image-grid.three{grid-template-columns:repeat(3,1fr)}.image-grid.four{grid-template-columns:repeat(2,1fr);grid-template-rows:repeat(2,1fr)}.image-grid.five{grid-template-columns:repeat(3,1fr);grid-template-rows:repeat(2,1fr)}.image-grid.five.image-wrapper:first-child{grid-column:span 2;grid-row:span 2}.image-grid.six{grid-template-columns:repeat(3,1fr);grid-template-rows:repeat(2,1fr)}.text{flex:1;text-align:center;padding-right:20px;justify-content:center;align-items:center;color:#f0f0f0;margin-top:", ifelse(c == "wechat", 130, 180), "px;text-shadow: -1px -1px 1px rgba(0,0,0,0),1px 1px 1px rgba(0,0,0,0.8)!important;}</style></head><body><div class=\"container\"><div class=\"aspect-ratio-box\"><div class=\"backdrop-layer\"><div class=\"backdrop-image\"style=\"background-image: url('", background, "');\"></div></div><div class=\"image-grid ", classtab[length(img)], "\">", text,  imgtext, "</div></div></div></body></html>")

    # 将 HTML 表格保存为文件
    writeLines(htmlstr, paste0(c, "_rstata_coverpic.html"))

    # 转换成 PDF 文件
    servr::httd() -> p
    pagedown::chrome_print(paste0(c, "_rstata_coverpic.html"),
                           paste0(c, "_rstata_coverpic.pdf"),
                           options = list(paperWidth = 23.5, paperHeight = 10))
    p$stop_server()

    # 转换成 png 文件
    pdftools::pdf_convert(paste0(c, "_rstata_coverpic.pdf"),
                          dpi = 300,
                          filenames = paste0(c, "_", output_file))

    # 去除边缘空白
    knitr::plot_crop(paste0(c, "_", output_file))

    # 添加水印
    if (addwm == T) {
      rstatatools::addrswm(paste0(c, "_", output_file))
    }

    # 删除文件
    file.remove(paste0(c, "_rstata_coverpic.pdf"))
    file.remove(paste0(c, "_rstata_coverpic.html"))
  }
}
