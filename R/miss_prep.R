#' Missing Value Pattern Checking and Processing
#'
#' This function will detect the pattern of missing values and output the pattern as
#' a dataframe. It can also process the dataframe by eliminating the missed columns or
#' rows when needed.
#'
#' The output dataframe "pattern" gives you a clear image about the pattern of missing value
#' in your dataframe when considering combinations of features. The output dataframe "df" is
#' the processed dataframe. By default, it will automatically drop columns with more than 30%
#' missing while keep rows. This is just some usual way suggested by data scientist community.
#' You can change the remove.row to TRUE to let the function helps you drop samples with
#' more than 50% features missing, or set remove.column to FALSE to ignore drop columns.
#'
#' @param dataframe: a dataframe-like object
#' @param drop.column: a logical indicating whether to automatically drop columns.
#' Default to TRUE. See "Details" for more information
#' @param drop.row: a logical indicating whether to automatically drop rows.
#' Default to FALSE. See "Details" for more information
#' @export
#' @return Two dataframes, the dataframe called "pattern" contains the pattern of missings in
#' the original dataframe, while the dataframe called "df" contains the processed dataframe,
#' which maybe used for further processing such as imputation.
#' @usage miss_prep(dataframe,remove.column=TRUE,remove.row=FALSE)
#' @seealso \code{\link{aggr}}
#' @references Michy Alice, Imputing Missing Data with R: mice Package.
#' \url{https://datascienceplus.com/imputing-missing-data-with-r-mice-package/}
#' @examples
#' ## Generate some NAs in the dataframe for illustration
#' data("tmall_milk_sales")
#' test_df=tmall_milk_sales
#' test_df[sample(1:nrow(test_df),200),'promotion'] <- NA
#' test_df[sample(1:nrow(test_df),50),'feature'] <- NA
#' test_df[sample(1:nrow(test_df),20),'units'] <- NA
#' test_df[sample(1:nrow(test_df),5),'unit_price'] <- NA
#' ## test function
#' miss=miss_prep(test_df)
#' miss_pattern=miss$pattern
#' df_processed=miss$df

miss_prep = function(dataframe,remove.column=TRUE,remove.row=FALSE){

  miss_pct=function(x){sum(is.na(x))/length(x)*100}
  col.miss=apply(dataframe,MARGIN = 2,miss_pct)
  row.miss=apply(dataframe,MARGIN = 1,miss_pct)

  miss_pattern=aggr(dataframe,plot=FALSE)
  miss_pattern_table=as.data.frame(cbind(miss_pattern$tabcomb,
                                   miss_pattern$count,miss_pattern$percent))
  colnames(miss_pattern_table)=c(colnames(dataframe),'Count','Percent')

  if(remove.column){
    dataframe.prepared=dataframe[,col.miss<30]
  }else{
    dataframe.prepared=dataframe
  }

  if(remove.row){
    dataframe.prepared=dataframe.prepared[col.miss<50,]
  }else{
    dataframe.prepared=dataframe.prepared
  }

  output=list()
  output$pattern=miss_pattern_table
  output$df=dataframe.prepared

  return(output)
}
