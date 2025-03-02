% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_tablepre.R
\name{create_tablepre}
\alias{create_tablepre}
\title{create_tablepre：生成数据预览图}
\usage{
create_tablepre(
  data,
  decimal_places = 2,
  output_file = "rstataoutput.png",
  slice = 20,
  select = 12,
  title = "数据概览",
  caption = "数据处理：微信公众号 RStata",
  fontsize = 18,
  addwm = F,
  data_source = "见介绍"
)
}
\arguments{
\item{data}{a tibble data frame}

\item{decimal_places}{decimal}

\item{output_file}{output file}

\item{slice}{slice x lines, default 20}

\item{select}{select x columns, default 12}

\item{title}{title}

\item{caption}{caption}

\item{fontsize}{fontsize of text}

\item{addwm}{if or not add RStata water mark}

\item{data_source}{data source}
}
\description{
create_tablepre：生成数据预览图
}
