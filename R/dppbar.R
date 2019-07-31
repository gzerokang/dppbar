#' \pkg{dppbar}: Data Processing and Plotting for Business Analysis in R
#'
#' The \pkg{dppbar} assembles functions from commonly used R package in explanatory data
#' analysis to build a toolbox that can be easily implemented even for beginners in this field.
#' It has a designed emphasis on business analysis, so it is especially useful in cross-sectional
#' data or panel data analysis. The package tries to balance between simplicity and flexibility.
#' Most functions can be easily implemented while offering further control parameters
#' to customerize result.
#'
#' The \pkg{dppbar} package contains functions to
#' \itemize{
#' \item Processing Data: transfer between numerical and categorical data, impute missing
#' values, and get special summary information
#' \item Data Visualization: common plots used to show patterns of data
#' \item Basic Modeling:  a stepwise model selection for panel data regression to get basic
#' idea about how to build regression models
#' }
#'
#' @section Functions:
#'
#' The main functions are:
#' \tabular{ll}{
#'    \strong{Data Processing Functions} \tab \cr
#'    \code{column_class()} \tab Separate features by categorical or numerical\cr
#'    \code{num2ctg()} \tab Numerical variable to categorical variabel\cr
#'    \code{ord_ctg2num()} \tab Ordinal categorical variable to numerical variable\cr
#'    \code{nom_ctg2num()} \tab Nominal categorical variable to numerical variable by dummy\cr
#'    \code{impute_missing()} \tab impute missing value by mice\cr
#'    \strong{Data Visualization Functions} \tab \cr
#'    \code{bar_plot()} \tab Stacked bar plot for multiple categories\cr
#'    \code{bubble_plot()} \tab Bubble plot with color and size showing more information\cr
#'    \code{corr_check()} \tab Pairs plot to check correlation between variables\cr
#'    \code{distribution_plot()} \tab Three plotting types to show distribution\cr
#'    \code{donut_plot()} \tab Donut plot to show percentage\cr
#'    \code{double_axis()} \tab Combine bar plot and line plot with double axis\cr
#'    \code{facet_bar()} \tab Separate bar plots in small facet for multiple categories\cr
#'    \code{horizontal_bar()} \tab Horizontal bar plot showing percentage\cr
#'    \code{label_bar_plot()} \tab Bar plot with a small label\cr
#'    \code{lines_plot()} \tab Basic lines plot with options\cr
#'    \code{lines_split_plot()} \tab Using subplot to show series with different range\cr
#'    \code{line_ann_plot()} \tab Line plot with information on turning points\cr
#'    \code{polar_cahrts()} \tab Drawing radar plot\cr
#'    \code{rank_plot()} \tab Rank change plot of categories over index\cr
#'    \strong{Data Analysis Functions} \tab \cr
#'    \code{cal_pct()} \tab Calculate percentage of each category\cr
#'    \code{get_rank()} \tab Get rank of each category\cr
#'    \code{lin_predict()} \tab Linear extrapolation with trend DLM\cr
#'    \code{plm_basic()} \tab Stepwise model selection for plm\cr
#' }
#'
#'
#'
#' @author Jizhou Kang \email{jizhou_kang@@hotmail.com}
#'
#'
#' @docType package
#' @name dppbar
#' @keywords package
NULL
