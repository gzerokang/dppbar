#' Plot Lines with Different Range
#'
#' \code{lines_split} uses subplot to plot numeric variables with various range
#' but share same index.
#'
#' When plotting different variables that shares same index but have different range, using
#' one plot may generate big problem since some of the variable may just be a flat line. To
#' avoid this problem, one possible solution is to use subplot. This function is designed for
#' this purpose. The three dots structure inside the inner function lines_split are
#' options for the general settings of layout, see \code{\link[plotly]{layout}} for
#' more information.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param num.idx: a vector of character or an integer, indicate which column(s) will
#' be selected as the variable to show in y axis
#' @param condition: a vector, indicate how to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' Should be given some value if the condition appears in multiple columns in the dataframe
#' @param colors: a vector, the i-th element represent the color that will be used to draw
#' the i-th line, which refers to the numeric value of i-th condition.
#' @param mode: how should each line displayed, usually be set as 'lines' or 'lines+markers',
#' see \url{https://plot.ly/r/reference/#scatter} for more information
#' @param yaxis_name: name of y axis
#' @param linewidth: the width of each line, default to 2
#' @param title: the title of the plot
#' @param ... other parameters for plotting, see "Details" and \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @seealso \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{layout}},
#' \code{\link{lines_split_plot}}
#' @usage lines_split(dataframe,ctg.idx,num.idx,condition,condition.idx,
#'                    colors,mode='lines',yaxis_name="",linewidth=2,
#'                    title,...)
#' @examples
#' data("macro_data_chn")
#' lines_split(dataframe=macro_data_chn,ctg.idx = 'year',
#'             num.idx = c(3,5,6,9),
#'             colors=c("#00526d","#de6e6e","#32ab60","#ff8000"),
#'             title="一些宏观经济指标走势",
#'             xaxis=list(showgrid=F,ticklen=4,nticks=3,title="年份"),
#'             legend=list(x=0.5,y=1.05,orientation='h',bgcolor='transparent'),
#'             paper_bgcolor='#ccece6',margin=list(t=32,l=32,r=32))

lines_split = function(dataframe,ctg.idx,num.idx,condition,condition.idx,
                       colors,mode='lines',yaxis_name="",linewidth=2,
                       title,...){

  dots=list(...)

  if(ctg.idx%in%colnames(dataframe)){
    ctg.idx=ctg.idx
  }else{
    ctg.idx=colnames(dataframe)[ctg.idx]
  }
  ctg.idx=enquo(ctg.idx)

  if(num.idx[1]%in%colnames(dataframe)){
    num.idx=num.idx
  }else{
    num.idx=colnames(dataframe)[num.idx]
  }
  num.idx=enquo(num.idx)

  if(missing(condition)){
    sub_dataframe = dataframe%>%
      select(!!ctg.idx,!!num.idx)%>%
      gather(name,value,-!!ctg.idx)
    sub_dataframe=sub_dataframe[order(sub_dataframe[,1]),]
    row.names(sub_dataframe)=seq(1,nrow(sub_dataframe))
    colnames(sub_dataframe)[1]='index'
  }else{
    if(missing(condition.idx)){
      select.vec=apply(dataframe,MARGIN = 2,function(x){condition[1]%in%x})
      condition.idx=colnames(dataframe)[select.vec]
    }else if(condition.idx%in%colnames(dataframe)){
      condition.idx=condition.idx
    }else{
      condition.idx=colnames(dataframe)[condition.idx]
    }

    condition.idx=enquo(condition.idx)

    sub_dataframe=dataframe %>%
      select(!!ctg.idx,!!condition.idx,!!num.idx)
    sub_dataframe=sub_dataframe[sub_dataframe[,2]%in%condition,]
    sub_dataframe=sub_dataframe[order(sub_dataframe[,1]),]
    row.names(sub_dataframe)=seq(1,nrow(sub_dataframe))
    colnames(sub_dataframe)[1]='index'
  }

  ## start plot
  p=sub_dataframe%>%
    split(sub_dataframe[,2]) %>%
    lapply(function(d) plot_ly(d,x=~index,y=~value,type='scatter',mode=mode,
                               name=~name,line=list(width=linewidth)))%>%
    subplot(nrows = NROW(.),shareX=TRUE)

  if('xaxis'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['xaxis'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,xaxis=list(showgrid=T,nticks=10,ticklen=4,
                                             ticks='outside',tickmode="array",
                                             type='category',title=""))))
  }



  if('legend'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['legend'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,legend=list(x=0,y=-0.1,orientation='h',
                                              font=list(size=10),bgcolor="transparent"))))
  }

  p = p%>%layout(annotations = list(
    list(xref='paper',yref='paper',xanchor='left',yanchor='right',
         x=0,y=1.05,showarrow = F,text=title,
         font=list(size=20,family='heiti'))),...)

  return(p)

}
