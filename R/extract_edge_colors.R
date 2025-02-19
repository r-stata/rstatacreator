#' @title extract_edge_colors: 提取图片边缘的颜色
#' @param image_path image_path
#' @param edge_width edge_width
#' @import magick
#' @export
extract_edge_colors <- function(image_path, edge_width = 1) {
  # 读取图片
  img <- image_read(image_path)

  # 获取图片的宽度和高度
  info <- image_info(img)
  width <- info$width
  height <- info$height

  # 提取左上角的颜色
  top_left <- image_crop(img, geometry_area(edge_width, edge_width, 0, 0))
  top_left_color <- as.raster(top_left)[1, 1]  # 获取第一个像素的颜色

  # 提取右上角的颜色
  top_right <- image_crop(img, geometry_area(edge_width, edge_width, width - edge_width, 0))
  top_right_color <- as.raster(top_right)[1, 1]

  # 提取左下角的颜色
  bottom_left <- image_crop(img, geometry_area(edge_width, edge_width, 0, height - edge_width))
  bottom_left_color <- as.raster(bottom_left)[1, 1]

  # 提取右下角的颜色
  bottom_right <- image_crop(img, geometry_area(edge_width, edge_width, width - edge_width, height - edge_width))
  bottom_right_color <- as.raster(bottom_right)[1, 1]

  # 将颜色组成列表
  edge_colors <- list(
    top_left = top_left_color,
    top_right = top_right_color,
    bottom_left = bottom_left_color,
    bottom_right = bottom_right_color
  )

  return(edge_colors)
}
