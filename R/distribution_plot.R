#' Multiple Type Distribution Plot
#'
#' \code{distribution_plot} shows the distribution of a particular numerical variable.
#' One can also specify a categorical variable to make box plot or violin plot to display
#' the distribution in different category.
#'
#' If type is 'histogram', then a histogram together with a kernel estimation will be
#' displayed. If type is 'violin', a violin plot with box will be displayed, and if
#' type is 'box' then a box plot with outliers will be displayed.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' categorical variable that will be used to split the numerical variable in box or
#' violin plot, default to NA
#' @param num.idx: a vector of character or an integer, indicate which column
#' will be selected as the numerical variable to show distribution with
#' @param type: a string of either 'histogram', 'violin' or 'box', decide the way to show
#' distribution, default to 'histogram'. See "Details" for more information
#' @param xaxis_name: name of x axis
#' @param tick_text: a vector of string, tick text that will be applied to box or violin plot
#' @param yaxis_name: name of y axis
#' @param title: the the title of the plot
#' @param ... other layout parameters, usually paper_bgcolor or margin.
#' See \code{\link[plotly]{layout}}
#' @export
#' @return This function will return an object of class plotly. By calling this function you
#' will get either a histogram plus kernel, a violin plot or a box plot, based on
#' customer choice.
#' @usage distribution_plot(dataframe,ctg.idx=NA,num.idx,type='histogram',
#'                          xaxis_name,labels=NA,yaxis_name,title,...)
#' @seealso \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{layout}}
#' @examples
#' data("tmall_milk_sales")
#' distribution_plot(dataframe=tmall_milk_sales,ctg.idx=NA,num.idx='unit_weight',
#'                   type='histogram',xaxis_name='单位净含量（克）',
#'                   yaxis_name='产品数量（件）',title='线上销售乳制品单位净含量分布')
#' distribution_plot(dataframe=tmall_milk_sales,ctg.idx=11,num.idx='unit_weight',
#'                   type='violin',xaxis_name='销量情况',
#'                   tick_text=c('热销产品','普通产品','滞销产品'),
#'                   yaxis_name='单位净含量（克）',
#'                   title='线上销售乳制品单位净含量按销量分布',paper_bgcolor='#ccece6')
#' distribution_plot(dataframe=tmall_milk_sales,ctg.idx=2,num.idx='unit_weight',
#'                   type='box',xaxis_name='销量情况',
#'                   tick_text=c('酸奶产品','牛奶产品'),
#'                   yaxis_name='单位净含量（克）',
#'                   title='线上销售乳制品单位净含量按产品线分布',
#'                   paper_bgcolor='#ccece6',margin=list(t=36,l=36,b=36,l=10))


distribution_plot=function(dataframe,ctg.idx=NA,num.idx,type='histogram',
                           xaxis_name,tick_text=NA,yaxis_name,title,...){

  if(type=='histogram'){
    density=density(dataframe[,num.idx])
    p = plot_ly()%>%
      add_histogram(x=dataframe[,num.idx],type='histogram',showlegend=F)%>%
      add_trace(x = density$x,y=density$y,type='scatter',mode='lines',
                fill = "tozeroy",yaxis='y2',showlegend=F)%>%
      layout(yaxis=list(side = 'left',title=yaxis_name,ticklen=2,gridwidth=2),
             xaxis=list(showgrid=F,ticklen=2,ticks="outside",tickangle=0,title=xaxis_name),
             yaxis2=list(side='right',title='',showgrid=F,ticklen=0,overlaying="y",
                         zeroline=FALSE,showticklabels=FALSE))
  }else{
    if(type=='violin'){
      p = dataframe %>%
        plot_ly(x=~dataframe[,ctg.idx],y=~dataframe[,num.idx],split=~dataframe[,ctg.idx],
                type='violin',box=list(visible=T),meanline=list(visible=T))
    }else{
      p = dataframe %>%
        plot_ly(x=~dataframe[,ctg.idx],y=~dataframe[,num.idx],split=~dataframe[,ctg.idx],
                type='box',boxpoints='outliers')
    }
    p=p%>%layout(showlegend=F,
                 xaxis=list(title=xaxis_name,tickvals=unique(dataframe[,ctg.idx]),
                            tickmode='array',ticktext=tick_text),
                 yaxis = list(title=yaxis_name,zeroline=F))
  }

  p=p%>%layout(annotations = list(
    list(xref='paper',yref='paper',xanchor='left',yanchor='right',
         x=0,y=1.05,showarrow = F,text=title,
         font=list(size=20,family='heiti'))),...)

  return(p)
}
