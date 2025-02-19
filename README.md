
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rstatacreator

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

快速创建微信公众号 RStata 的微信推文～

自用 R
包，里面有大量代码仅能在我自己的电脑上运行，不过代码还是很值得参考的！

欢迎大家关注微信公众号“RStata” 和 “Stata 中文社区” 获取最新资讯和动态！

|                                             RStata                                              |                                          Stata中文社区                                          |
|:-----------------------------------------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
| <img src="https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20201120143454.png" width="50%"/> | <img src="https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20201120143508.png" width="50%"/> |

## 安装方法

你可以从 GitHub 上安装这个 R 包：

``` r
devtools::install_github('r-stata/rstatacreator')
```

## 使用介绍

1.  generate_preview: 针对含有 readme.txt 的文件夹生成所需的全部图片截图
2.  add_picborder：添加 RStata 图表边框
3.  create_trade_marker: 在文件夹的每个子文件夹里面都创建版权标志
4.  extract_edge_colors: 提取图片边缘的颜色
5.  create_titlepic: 创建白底风格的标题图
6.  create_titlepic_dark: 创建暗黑风格的标题图
7.  create_readme: 创建 readme.txt 模板文件
8.  create_tablepre：生成数据预览图
9.  create_coverpage: 创建 RStata 推文的封面图，16:9 和 2.35:1
    的两种结果

------------------------------------------------------------------------

<h4 align="center">
License
</h4>
<h6 align="center">
MIT © 微信公众号 RStata
</h6>
