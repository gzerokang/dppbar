#' Bubble Plot with Choice on Color Size and Text
#'
#' \code{bubble_plot} makes bubble plot where each bubble represent a sample, with customer
#' choiced X axis, Y axis, size, color and text
#'
#' The column of size must be a numerical column, otherwise the mapping of size will generate
#' error.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param num.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in y axis
#' @param size.idx: a character or an integer, indicate which column will be selected as the
#' variable to be assigned to the bubble's size, should be a numeric column
#' @param color.idx: a character or and integer, indicate which column will be selected as the
#' variable to be assigned to the bubble's color, perfer categorical column
#' @param text.idx: a character or an integer, indicate which column will be selected as the
#' variable shown in the hover label
#' @param colors: a vector, the colors that to be used in making bubbles
#' @param xaxis_name: name of x axis
#' @param xaxis_format: format of x axis. See \code{\link[plotly]{layout}}
#' @param yaxis_name: name of y axis
#' @param yaxis_format: format of y axis. See \code{\link[plotly]{layout}}
#' @param legend_pos_y: a numeric, indicate the postion of the title of legend on vertical
#' axis, default to 1
#' @param title: the title of the plot
#' @param ... other parameters for plotting. See \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @importFrom magrittr %>%
#' @return This function will return an object of class plotly. By calling the function you
#' will get a bubble plot where X axis value, Y axis value, color, size and text is your choice.
#' @usage bubble_plot(dataframe,ctg.idx,num.idx,size.idx,color.idx,
#'                    text.idx,colors,xaxis_name,xaxis_format,yaxis_name,
#'                    yaxis_format,legend_pos_y=1,title,...)
#' @seealso \code{\link[plotly]{plot_ly}},\code{\link[plotly]{layout}}
#' @examples
#' data("machinery_fin_charts")
#' test_df=machinery_fin_charts%>%
#'   filter(年份==2017 & 分类=='通用设备')
#' bubble_plot(dataframe=test_df,ctg.idx=15,num.idx='毛利率',size.idx='主营业务收入',
#'             color.idx='经济区划分',text.idx=7,colors=RColorBrewer::brewer.pal(8,'Set1'),
#'             xaxis_name='营收CAGR5',yaxis_name='毛利率',xaxis_format='%',yaxis_format='%',
#'             title='通用设备企业实力气泡图（2017）',paper_bgcolor='#ccece6')
#' test_df=machinery_fin_charts%>%
#'   filter(年份==2017 & 分类=='电力设备')
#' bubble_plot(dataframe=test_df,ctg.idx=10,num.idx='毛利率',size.idx='主营业务收入',
#'             color.idx=15,text.idx=7,
#'             colors='Reds',xaxis_name='员工总数（人）',yaxis_name='毛利率',
#'             xaxis_format='',yaxis_format='%',legend_pos_y = 1.02,
#'             title='电力设备企业实力气泡图（2017）',paper_bgcolor='#ccece6')

bubble_plot=function(dataframe,ctg.idx,num.idx,size.idx,color.idx,
                     text.idx,colors,xaxis_name,xaxis_format,yaxis_name,
                     yaxis_format,legend_pos_y=1,title,...){

  ## treating NAs, removing rows containing NA
  format_idx=function(idx){
    if(idx%in%colnames(dataframe)){
      idx=idx
    }else{
      idx=colnames(dataframe)[idx]
    }
    idx=rlang::enquo(idx)
    return(idx)
  }

  select_column=sapply(list(ctg.idx,num.idx,size.idx,color.idx,text.idx),format_idx)
  dataframe=dataframe%>%
    dplyr::select(!!select_column[[1]],!!select_column[[2]],!!select_column[[3]],
           !!select_column[[4]],!!select_column[[5]])
  dataframe=dataframe[complete.cases(dataframe),]

  ## set size of bubbles
  desired_max_size=50
  list_size_values = dataframe[,3]
  sizeref = 2.0*max(list_size_values)/(desired_max_size**2)

  p = plot_ly(dataframe,
              x=dataframe[,1],y=dataframe[,2],
              color=dataframe[,4],colors=colors,sizes=c(10,50),
              text=dataframe[,5],hoverinfo='text+size',
              type='scatter',mode='markers',
              marker=list(symbol='circle',opacity=0.5,sizemode='area',
                          size=list_size_values,sizeref=sizeref,
                          line=list(width=2,color='#f2f2f2')))%>%
    layout(legend=list(x=1,y=0.95,bgcolor='transparent'),
           xaxis=list(title=xaxis_name,showgrid=T,ticklen=4,zeroline=F,showticklabels=T,
                      tickformat=xaxis_format,ticks='outside'),
           yaxis=list(title=yaxis_name,showgrid=T,showticklabels=T,ticklen=4,
                      tickformat=yaxis_format,zeroline=F),
           annotations=list(
             list(xref='paper',yref='paper',xanchor='left',yanchor='right',
                  x=0,y=1.05,showarrow = F,text=title,
                  font=list(size=20,family='heiti')),
             list(xref='paper',yref='paper',xanchor='left',yanchor='right',
                  x=1,y=legend_pos_y,showarrow = F,text=colnames(dataframe)[4],
                  font=list(size=16,family='heiti'))),...)

  return(p)
}


