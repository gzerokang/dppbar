#' Transform ordinal categorical variable into numerical variable
#'
#' Transform a ordinal categorical column into a numerical column, based on
#' customer specified rule.
#'
#' The function will match each element in a categorical variable with a numeric value
#' by the mapping rule specified in the function input.
#'
#' @param dataframe: a dataframe-like object
#' @param col.id: an integer, specify which column in the dataframe that you need to transfer
#' @param col.name: a character, another way to specify the to-be-transformed column,
#' will be disregarded if col.id is specified
#' @param permutation: a vector of string which is a permutation of unique element in
#' your categorical variable. It will serve as the rule for assign numeric values.
#' The first element in this permutation will be assigned the smallest numeric value
#' @param numeric_levels: a numeric vector which determine the numeric values to be
#' assigned to the categorical variable, default to consecutive sequence of length
#' equal to permutation and start from 1
#' @export
#' @return Numerical vairable which is transformed from the assigned categorical variable
#' based on the specified order.
#' @usage
#' ord_ctg2num(dataframe,col.id,col.name=NA,permutation,
#' numeric_levels=seq(1,length(permutation),1))
#' @examples
#' data(tmall_milk_sales)
#' test_df=tmall_milk_sales
#' test_df$label[12:15]=NA
#' ord_ctg2num(test_df,col.name = 'label',permutation = c('P','O','H'))
#' ord_ctg2num(test_df,col.id = 11,permutation = c('P','O','H'))
#' ord_ctg2num(test_df,col.id = 11,permutation = c('P','O','H'),
#' numeric_levels=c(1,5,10))
#' \dontrun{
#'   # The following code will generate error,
#'   # because permutation not match
#'   ord_ctg2num(test_df,col.id = 11,permutation = c('p','o','h'))
#' }


ord_ctg2num=function(dataframe,col.id,col.name=NA,permutation,
                     numeric_levels=seq(1,length(permutation),1)){

  if(missing(col.id)){
    col.id=match(col.name,colnames(dataframe))
  }

  vec_copy=dataframe[,col.id]
  if(setequal(unique(vec_copy)[is.na(unique(vec_copy))==F],permutation)){
    order.dataframe=data.frame(numeric_levels)
    row.names(order.dataframe)=permutation
    num_vec=order.dataframe[vec_copy,]
  }else{
    stop('Error: Permutation does not match.')
  }

  return(num_vec)
}

