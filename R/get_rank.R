#' Get Rank of Each Category
#'
#' \code{get_rank} calculate the rank of each category, and can be based on some criteria
#' about selecting data, like the rank of each company's income in a certain year.
#'
#' This function uses dplyr package which offers a fast computation of rank. It can take
#' either column name or column index as the input to specify the categorical and numerical
#' variable, as well as using some condition to subset data. The function can detect the
#' column in which the condition is in automatically, so you are not required to input it.
#'
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' categorical variable
#' @param num.idx: a character or an integer, indicate which column will be selected as the
#' numerical variable
#' @param condition: a character or a numeric, indicate whether to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' Should be given some value only if the condition appears in multiple columns in the dataframe
#' @export
#' @return A new dataframe contains three columns. The first is a categorical variable,
#' the second is a numerical variable, and the third is the rank of each level in
#' the categorical variable with respect to the numerical variable.
#' @usage
#' get_rank(dataframe,ctg.idx,num.idx,condition,condition.idx)
#' @examples
#' data("dairy_fin_charts")
#' get_rank(dairy_fin_charts,ctg.idx = 'name',num.idx = "income",condition=2017)
#' ## another way to get the same result
#' get_rank(dairy_fin_charts[dairy_fin_charts$Year==2017,],ctg.idx = 1,num.idx = 2)
#' ## when two categories have same value
#' dairy_fin_charts[97,2]=12.40
#' get_rank(dairy_fin_charts,ctg.idx = 'name',num.idx = "income",condition=2017)
#' ## when have missing value
#' get_rank(dairy_fin_charts,ctg.idx = 1,num.idx = 2,condition=2010)

get_rank = function(dataframe,ctg.idx,num.idx,condition,condition.idx){

  if(ctg.idx%in%colnames(dataframe)){
    ctg.idx=ctg.idx
  }else{
    ctg.idx=colnames(dataframe)[ctg.idx]
  }
  ctg.idx=enquo(ctg.idx)

  if(num.idx%in%colnames(dataframe)){
    num.idx=num.idx
  }else{
    num.idx=colnames(dataframe)[num.idx]
  }
  num.idx=enquo(num.idx)

  if(!missing(condition)){
    if(missing(condition.idx)){
      select.vec=apply(dataframe,MARGIN = 2,function(x){condition%in%x})
      condition.idx=colnames(dataframe)[select.vec]
    }else if(condition.idx%in%colnames(dataframe)){
      condition.idx=condition.idx
    }else{
      condition.idx=colnames(dataframe)[condition.idx]
    }

    condition.idx=enquo(condition.idx)

    dataframe=dataframe %>%
      select(!!ctg.idx,!!num.idx,!!condition.idx)
    dataframe=dataframe[dataframe[,3]==condition,]
    dataframe=dataframe[,-3]

  }else{
    dataframe=dataframe %>%
      select(!!ctg.idx,!!num.idx)
  }

  dataframe$rank=rank(as.numeric(-dataframe[,2]),na.last = "keep", ties.method = 'min')

  return(dataframe)
}
