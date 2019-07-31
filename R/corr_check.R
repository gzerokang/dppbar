#' Correlation Checking Plot
#'
#' \code{corr_check} gives a nice plot for checking correlation between numerical variables.
#'
#' The upper panel shows the correlation coefficients of numerical columns. The lower
#' panel shows the points and regression line of corresponding numerical columns, where
#' one can get some basic sense about how those variables are correlated.
#' The color displayed is an easy way to know the correlation between variables.
#' If the color is dark blue, then their are strong negative correlation (less than -0.6),
#' if it is light blue, then some negative correlation exist (-0.4 to -0.6),
#' if it is green, then you may think the two variables have little correlation (-0.4 to 0.4)
#' if it is pink, then there is relatively strong positive correlation (0.4 to 0.6)
#' and if it is red, then they have string positive correlation (bigger than 0.6).
#'
#' @param dataframe: a dataframe object
#' @param eliminate: a vector of numeric or character, indicate the column names or column
#' index of those columns that you do not want to include in correlation plot. Usually id
#' @export
#' @return A correlation plot with correlation coefficients shown in the upper panel and
#' points and linear regression line plot shown in the lower panel, for choosen
#' numerical columns.
#' @usage
#' corr_check(dataframe,eliminate)
#' @seealso \code{\link[graphics]{pairs}}
#' @examples
#' data("macro_data_chn")
#' corr_check(dataframe=macro_data_chn,eliminate = 'year')
#' ## another example, add a categorical variable,
#' ## see if the function eliminate it automatically
#' test_df=macro_data_chn
#' test_df$ctg='cat'
#' corr_check(dataframe = test_df,eliminate=c(1,2))


corr_check = function(dataframe,eliminate){

  if(!missing(eliminate)){
    if(eliminate[1]%in%colnames(dataframe)){
      dataframe=dataframe[,!(colnames(dataframe)%in%eliminate)]
    }else{
      eliminate=colnames(dataframe)[eliminate]
      dataframe=dataframe[,!(colnames(dataframe)%in%eliminate)]
    }
  }else{
    dataframe=dataframe
  }

  numeric_columns = column_class(dataframe)$numerical$col.names
  plot_df = dataframe[,colnames(dataframe)%in%numeric_columns]

  ## define panel to show correlation
  mypanel <- function(x, y, ...){
    cvec.border <- c("#00008B", "#4169E1","#00AB00","#FF6A6A","#DC143C")
    r <- cor(x, y)
    i.color <- 1+(r>-0.6)+(r>-0.4)+(r>0.4)+(r>0.6)
    ll <- par("usr")
    rect(ll[1], ll[3], ll[2], ll[4], border=cvec.border[i.color], lwd=3)
    points(x, y, ... )
    ok <- is.finite(x) & is.finite(y)
    if( any(ok) ) { abline(lm(y[ok] ~ x[ok]), col="red", lty=2, ...) }
  }

  panel.cor2 <- function(x, y, digits = 2, prefix = "", cex.cor, color.bg=FALSE, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    cvec.bg <- c('#1E90FF' , '#87CEFA', '#CCFFCC',"#FFE4E1","#FFCCE6")
    cvec.text <- c("#00008B", "#4169E1","#00AB00","#FF6A6A","#DC143C")
    i.color <- 1+(r>-0.6)+(r>-0.4)+(r>0.4)+(r>0.6)
    if( color.bg ) {
      ll <- par("usr")
      rect(ll[1], ll[3], ll[2], ll[4], col=cvec.bg[i.color])
      text(0.5, 0.5, txt, cex = cex.cor, col=cvec.text[i.color])
    } else {
      text(0.5, 0.5, txt, cex = cex.cor, col=cvec.text[i.color])
    }
  }

  textPanel=function(x=0.5,y=0.5,txt,cex,font){
    text(x,y,txt,cex=cex,family='STKaiti')
  }

  pairs(plot_df, gap=0.5,  las=1,
        pch=21, bg=rgb(0,0,1,0.25), text.panel = textPanel,
        panel=mypanel, upper.panel=function(...) panel.cor2(..., color.bg=TRUE), main="")

}



