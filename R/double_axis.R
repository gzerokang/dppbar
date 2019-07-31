#' Stack Bar Plot and Line Plot with Double Axises
#'
#' \code{double_axis} makes plot with two y axises, one for display line plot, and the other
#' for bar plot
#'
#' This function is useful when the numerical variable you want to display have
#' various range, like some are percentage while others are values. The three dots
#' structure makes it easy for advanced users to set other graphing parameters, such
#' as legend, paper_bgcolor and margin. This function can be combined with
#' \code{\link[plotly]{add_trace}} to add more traces, such as points, to the plot.
#' Note that one should carefully choose how many variables to display and
#' whether to add annotations to make the plot clear and fancy.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param lines.idx: a vector of characters or integers, but not both, indicate which
#' column(s) will be selected to make line plot
#' @param bars.idx: a vector of characters or integers, but not both, indicate which
#' column(s) will be selected to make bar plot
#' @param condition: a vector, indicate how to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' Should be given some value if the condition appears in multiple columns in the dataframe
#' @param lines.mode: a string indicating the mode of line plot, can be 'lines' or 'lines+markers'
#' @param lines.colors: a vector, the palette for line plot
#' @param lines.width: a numeric, the width of the line plot, default to 2
#' @param lines.names: a vector of strings, indicating the name of line variables in legend
#' @param bars.colors: a vector, the palette for bar plot
#' @param bars.names: a vector of strings, indicating the name of bar variables in legend
#' @param xaxis_name: a string, the name of x axis
#' @param line.axis_format: a string, the format of y axis of line plot.
#' See \code{\link[plotly]{layout}} for more information, default to ""
#' @param line.axis_name: a string, the name of y axis for line plot, will be displayed
#' on the left side of the plot
#' @param bars.axis_name: a string, the name of y axis for bar plot, will be displayed
#' on the right side of the plot
#' @param title: title of the plot
#' @param annOn: a logical, decide whether to add annotations for line plot.
#' See "Details" for more information
#' @param ... other parameters for plotting, see "Details" and \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will plot with double y axis, one for display lines and the other for the bars.
#' @usage double_axis(dataframe,ctg.idx,lines.idx,bars.idx,condition,condition.idx,
#'                    lines.mode,lines.colors,lines.width=2,lines.names,
#'                    bars.colors,bars.names,xaxis_name,line.axis_format="",
#'                    line.axis_name,bar.axis_name,title,annOn,...)
#' @seealso \code{\link[plotly]{add_trace}},\code{\link[plotly]{layout}}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' ## a warm up example
#' data("dairy_fin_charts")
#' test_df=dairy_fin_charts%>%
#'   filter(name=='伊利股份')
#' double_axis(dataframe=test_df,ctg.idx='Year',lines.idx='profit',bars.idx=2,
#'             lines.mode='lines+markers',lines.colors='rgb(128,0,128)',
#'             lines.names = '营业利润',bars.colors = 'rgba(55,128,192,0.7)',
#'             bars.names = '营业收入',xaxis_name = '年份',
#'             line.axis_name = '营业利润（亿元）',bar.axis_name = '营业收入（亿元）',
#'             title='伊利股份营收及利润',annOn=T,margin=list(r=40))
#' ## a more complex example
#' data("estate_fin_charts")
#' double_axis(dataframe=estate_fin_charts,ctg.idx='Year',lines.idx=c(5,6),
#'             bars.idx=c('asset','liability'),condition='万科A',lines.mode = 'lines',
#'             lines.colors = c("rgb(128, 0, 128)",'rgb(255,140,0)'),
#'             lines.width = 4,lines.names=c('总资产收益率','净资产收益率'),
#'             bars.colors = c('rgba(55,128,192,0.7)','rgba(219, 64, 82,0.7)'),
#'             bars.names = c('总资产','总负债'),xaxis_name = '年份',
#'             line.axis_format = '',line.axis_name='百分比',bar.axis_name = '单位：亿元',
#'             title='财务分析（万科A）',annOn=F,
#'             legend=list(x=0.45,y=1.03,orientation='h',
#'                         font=list(size=10),bgcolor="transparent"),
#'             margin=list(r=54),paper_bgcolor='#ccece6')
#' \dontrun{
#' ## be careful when set annOn when have more than one lines
#' ## the plot may not be clear
#' double_axis(dataframe=estate_fin_charts,ctg.idx='Year',lines.idx=c(5,6),
#'             bars.idx=c('asset','liability'),condition='万科A',lines.mode = 'lines+markers',
#'             lines.colors = c("rgb(128, 0, 128)",'rgb(255,140,0)'),
#'             lines.width = 4,lines.names=c('总资产收益率','净资产收益率'),
#'             bars.colors = c('rgba(55,128,192,0.7)','rgba(219, 64, 82,0.7)'),
#'             bars.names = c('总资产','总负债'),xaxis_name = '年份',
#'             line.axis_format = '',line.axis_name='百分比',bar.axis_name = '单位：亿元',
#'             title='财务分析（万科A）',annOn=T,
#'             legend=list(x=0.45,y=1.03,orientation='h',
#'                         font=list(size=10),bgcolor="transparent"),
#'             margin=list(r=54),paper_bgcolor='#ccece6')
#' }


double_axis = function(dataframe,ctg.idx,lines.idx,bars.idx,condition,condition.idx,
                       lines.mode,lines.colors,lines.width=2,lines.names,
                       bars.colors,bars.names,xaxis_name,line.axis_format="",
                       line.axis_name,bar.axis_name,title,annOn,...){

  dots = list(...)

  num.line=length(lines.idx)
  num.bar=length(bars.idx)

  ## select data
  if(ctg.idx%in%colnames(dataframe)){
    ctg.idx=ctg.idx
  }else{
    ctg.idx=colnames(dataframe)[ctg.idx]
  }
  ctg.idx=enquo(ctg.idx)

  if(lines.idx[1]%in%colnames(dataframe)){
    lines.idx=lines.idx
  }else{
    lines.idx=colnames(dataframe)[lines.idx]
  }
  lines.idx=enquos(lines.idx)

  if(bars.idx[1]%in%colnames(dataframe)){
    bars.idx=bars.idx
  }else{
    bars.idx=colnames(dataframe)[bars.idx]
  }
  bars.idx=enquos(bars.idx)

  if(!missing(condition)){
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
      select(!!condition.idx,!!ctg.idx,!!!lines.idx,!!!bars.idx)
    sub_dataframe=sub_dataframe[sub_dataframe[,1]%in%condition,]
    sub_dataframe=sub_dataframe[order(sub_dataframe[,2]),]
    row.names(sub_dataframe)=seq(1,nrow(sub_dataframe))
  }else{
    sub_dataframe=dataframe %>%
      select(!!ctg.idx,!!!lines.idx,!!!bars.idx)
    index=rep(1,nrow(dataframe))
    sub_dataframe=cbind(index,sub_dataframe)
    sub_dataframe=sub_dataframe[order(sub_dataframe[,2]),]
    row.names(sub_dataframe)=seq(1,nrow(sub_dataframe))
  }

  ## proceed to plot
  p = plot_ly(data = sub_dataframe)

  for(i in 1:num.line){
    p=add_trace(p,x=sub_dataframe[,2],y=sub_dataframe[,2+i],
                type='scatter',mode=lines.mode,
                line=list(color=lines.colors[i],width=lines.width),
                name=lines.names[i],showlegend=T)
  }
  for(j in 1:num.bar){
    p=add_bars(p,x=sub_dataframe[,2],y=sub_dataframe[,2+num.line+j],
               showlegend=T,marker=list(color=bars.colors[j]),yaxis='y2',
               name=bars.names[j])
  }

  if('legend'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['legend'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,legend=list(x=0,y=0.95,orientation='h',
                                              font=list(size=10),bgcolor="transparent"))))
  }

  p = p%>%layout(yaxis2=list(side="right",title=bar.axis_name,ticklen=0,
                             showgrid=F,overlaying='y',zeroline=FALSE),
                 xaxis = list(showgrid=F,ticklen=4,ticks='outside',tickmode='array',
                              ticktext=unique(sub_dataframe[,2]),
                              tickvals=unique(sub_dataframe[,2]),
                              tickangle=-45,title=xaxis_name),
                 yaxis = list(side='left',title=line.axis_name,ticklen=2,
                              tickformat=line.axis_format,gridwidth=2,
                              zeroline=FALSE),
                 annotations = list(list(xref='paper',yref='paper',xanchor='left',
                                         yanchor='right',x=0,y=1.05,showarrow = F,
                                         text=title,font=list(size=20,family='heiti'))),...)
  if(annOn){
    for(i in 1:num.line){
      p=add_annotations(p,xref='x',yref='y',
                        x=sub_dataframe[,2],y=sub_dataframe[,2+i],
                        text=round(sub_dataframe[,2+i],2),
                        font=list(family='Arial',size=12,color='orange'),
                        showarrow=TRUE,arrowcolor='orange',arrowhead=2,
                        startarrowsize=0.4)
    }
  }else{
    p=p
  }

  return(p)

}

