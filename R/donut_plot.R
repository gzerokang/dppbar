#' Donut Plot to Show Percentage
#'
#' Percentage of certain categorical variable is essential in data analysis. \code{donut_plot}
#' makes a fancy plot based on pie plot that can clearly show percentage.
#'
#' If "legendOn" is set to TRUE, then there will be a legend indicate the map between
#' categorical variable's level and color. Otherwise the name of each categorical variable
#' will be shown as annotations on the plot. You can specify margin to avoid lose of
#' information because of the boundary of graph.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' categorical variable for each part of the pie
#' @param num.idx: a character or an integer, indicate which column will be selected as the
#' numerical variable to determine percentage
#' @param condition: a vector, indicate how to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' @param colors: a vector, the Palette that will be used to draw the graph
#' @param hole_size: a numeric, the size of the hole on the center of the original pie plot,
#' default to 0.5
#' @param title: a string, title of the plot
#' @param legendOn: a logical, how should the name of each category displayed, default to TRUE.
#' See "Details" for more information
#' @param ... other parameters to draw the plot, usually "paper_bgcolor" or "margin" in
#' \code{\link[plotly]{layout}}. See "Details" for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a donut plot (pie plot with a hole on the center).
#' @usage donut_plot(dataframe,ctg.idx,num.idx,condition,condition.idx,
#'                   colors,hole_size=0.5,title,legendOn=TRUE,...)
#' @seealso \code{\link[plotly]{plot_ly}},\code{\link[plotly]{add_pie}},
#' \code{\link[plotly]{layout}}
#' @examples
#' data("machinery_fin_charts")
#' donut_plot(dataframe=machinery_fin_charts,ctg.idx="分类",num.idx='主营业务收入',
#'            condition=2017,condition.idx='年份',
#'            colors=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
#'                     "#80B1D3" ,"#FDB462", "#B3DE69"),
#'            title='机械行业营收构成（2017）',legendOn=FALSE,paper_bgcolor='#ccece6')
#' ## another kind, here we use margin to make annotations showed fully
#' data("dairy_fin_charts")
#' donut_plot(dataframe=dairy_fin_charts,ctg.idx="name",num.idx='income',
#'            condition=2017,condition.idx='Year',colors = "",
#'            title='乳制品企业市场份额（2017）',legendOn=TRUE,
#'            paper_bgcolor='#ccece6',margin=list(t=30,b=72))


donut_plot = function(dataframe,ctg.idx,num.idx,condition,condition.idx,
                      colors,hole_size=0.5,title,legendOn=TRUE,...){

  plot_df=cal_pct(dataframe=dataframe,ctg.idx=ctg.idx,num.idx=num.idx,
                  condition=condition,condition.idx=condition.idx)

  if(legendOn){
    donut_plot = plot_df %>%
      plot_ly(labels = ~index, values = ~percent, textinfo = 'percent',
              insidetextfont = list(size=14,color='rbg686868'),
              outsidetextfont = list(size=14,color='rbg686868'),
              hoverinfo = 'text',
              text = ~paste(index,'</br>',percent,'%'),
              domain=list(y=c(0,0.9)),
              marker = list(line=list(color='#FFFFFF',width=1),colors = colors))%>%
      add_pie(hole=hole_size)
  }else{
    donut_plot = plot_df %>%
      plot_ly() %>%
      add_pie(hole = hole_size,
              labels = ~index, values = ~percent,textinfo='label+percent',
              insidetextfont = list(size=14,color='rbg686868'),
              outsidetextfont = list(size=14,color='rbg686868'),
              hoverinfo = 'text',
              text = ~paste(index,'</br>',percent,'%'),
              domain=list(y=c(0,0.9)),
              marker = list(line=list(color='#FFFFFF',width=1),colors = colors))
  }

  donut_plot=donut_plot%>%
    layout(showlegend=legendOn,
           annotations = list(
             list(xref='paper',yref='paper',xanchor='left',yanchor='right',
                  x=0,y=1.05,showarrow=F,text=title,
                  font=list(size='20',family='heiti'))),...)

  return(donut_plot)
}
