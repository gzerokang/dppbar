#' Separate features by categorical or numerical
#'
#' \code{column_class} get the class information for each column vector of a dataframe.
#'
#' This function receives a dataframe and returns a list which contains two dataframe:
#' numerical and categorical, which contain the column name and location
#' of numerical and categorical columns of the input dataframe, respectively.
#'
#' @param dataframe: a dataframe object
#' @export
#' @return A list with two element, both of them are dataframe, which stores the
#' column name and column index of numerical and categorical variables.
#' @usage
#' column_class(dataframe)
#' @examples
#' machinery_fin_charts_class=column_class(machinery_fin_charts)
#' machinery_fin_charts_class$numerical
#' machinery_fin_charts_class$categorical


column_class <- function(dataframe){

  class_df_generator=function(dataframe,class_vec,categorical=TRUE){
    if(categorical){
      class_df=data.frame('col.names'=colnames(dataframe)[class_vec=='character'],
                          'col.ids'=match(colnames(dataframe)[class_vec=='character'],
                                          colnames(dataframe)))
    }else{
      class_df=data.frame('col.names'=colnames(dataframe)[class_vec!='character'],
                          'col.ids'=match(colnames(dataframe)[class_vec!='character'],
                                          colnames(dataframe)))
    }
  }

  class_of_col <- sapply(dataframe,class)

  dataframe_class <- list(numerical=class_df_generator(dataframe,class_of_col,FALSE),
                         categorical=class_df_generator(dataframe,class_of_col))
  return(dataframe_class)
}
