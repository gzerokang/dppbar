#' Line Plots with Information about Turning Points
#'
#' \code{line_ann_plot} will add user specified turning points and corresponding explanations
#' to a regular line plots.
#'
#' This function uses the package \code{\link{plotly}} to make fancy line plots with annotations
#' of the turning points of the dataframe. To make the plot, you will need a dataframe
#' containing at least three columns, the column for x axis and y axis of the plot, as well
#' as a categorical column which will be used to select data or display different lines.
#' The function already have default settings for plot which makes it handy to users with
#' little experience in using plotly. For those advanced users, more plotting parameters can
#' be used because of the three dots structure. Such options may include: xaxis, legend,
#' and paper_bgcolor. See \code{\link[plotly]{layout}} for more information.
#'
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param num.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in y axis
#' @param condition: a vector, indicate how to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' Should be given some value if the condition appears in multiple columns in the dataframe or
#' condition is missing
#' @param colors: a vector, the i-th element represent the color that will be used to draw
#' the i-th line, which refers to the numeric value of i-th condition.
#' @param events: a vector of strings, explanation of each turning point
#' @param marker_pos_x: a vector, should be the same length as events, indicate the x axis
#' position of turning points markers
#' @param ann_pos_x: a numeric, indicate the x axis position of markers in annotation
#' @param text_pos_x: a numeric, indicate the x axis position of explanations in annotation
#' @param ann_pos_y: a vector of numeric, indicate the y axis position of markers in annotation
#' @param marker_refer: which line should be referred to when putting turning points markers
#' @param marker_pos_adj: should the marker in the plot have some position adjusting in y axis
#' @param yaxis_name: name of y axis
#' @param title: the title of the plot
#' @param marker_color: the color of marker, default to 'rgb(246,78,139)'(alike purple)
#' @param ... other parameters for plotting, see "Details" and \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @return This function will return an object of class plotly. By calling the function you
#' will get a plot which is a series plot with markers and annotations marking turning points.
#' @usage line_ann_plot(dataframe,ctg.idx,num.idx,condition,condition.idx,colors,
#'                      events, marker_pos_x,ann_pos_x,text_pos_x,ann_pos_y,
#'                      marker_refer,marker_pos_adj,yaxis_name,title,
#'                      marker_color='rgb(246,78,139)',...)
#' @seealso \code{\link[plotly]{add_trace}},\code{\link[plotly]{layout}}
#' @references Time Series Charts By the Economist in R Using Plotly
#' Published by Roddhiman in Dashboards, Data Visualization, R
#' \url{https://moderndata.plot.ly/time-series-charts-by-the-economist-in-r-using-plotly/}
#' @examples
#' data(dairy_fin_charts)
#' line_ann_plot(dataframe=dairy_fin_charts,ctg.idx='Year',num.idx='profit',
#'               condition=c('伊利股份','蒙牛股份','光明乳业'),
#'               colors = c("#00526d","#de6e6e","rgb(50,171,96)"),
#'               events = c("中国奶制品污染事件&金融危机","公布和实施经济刺激计划",
#'                          "“互联网+”：传统行业进入电商时代","中央一号文件：全面振兴奶业"),
#'               marker_pos_x = c(2008,2009,2012,2017),
#'               ann_pos_x = 2006, text_pos_x = 0.1, ann_pos_y = c(78,74,70,66),
#'               marker_refer = c('光明乳业','蒙牛股份','伊利股份','伊利股份'),
#'               yaxis_name = '利润总额（亿元）',title = '乳制品企业利润变动与行业重要事件')
#' ## customize some advanced parameters
#' line_ann_plot(dataframe=dairy_fin_charts,ctg.idx='Year',num.idx='profit',
#'               condition=c('伊利股份','蒙牛股份','光明乳业'),
#'               colors = c("#00526d","#de6e6e","rgb(50,171,96)"),
#'               events = c("中国奶制品污染事件&金融危机","公布和实施经济刺激计划",
#'                          "“互联网+”：传统行业进入电商时代","中央一号文件：全面振兴奶业"),
#'               marker_pos_x = c(2008,2009,2012,2017),
#'               ann_pos_x = 2006, text_pos_x = 0.1, ann_pos_y = c(78,74,70,66),
#'               marker_refer = c('光明乳业','蒙牛股份','伊利股份','伊利股份'),
#'               marker_pos_adj=3,
#'               yaxis_name = '利润总额（亿元）',
#'               title = '乳制品企业利润变动与行业重要事件',
#'               legend = list(x=0.5,y=0.1,orientation='h',
#'                             font=list(size=10),bgcolor='transparent'),
#'               xaxis = list(showgrid=T,nticks=12,ticks="outside",title="年份"),
#'               paper_bgcolor='#ccece6')




line_ann_plot=function(dataframe,ctg.idx,num.idx,condition,condition.idx,colors,
                       events, marker_pos_x,ann_pos_x,text_pos_x,ann_pos_y,
                       marker_refer,marker_pos_adj,yaxis_name,title,
                       marker_color='rgb(246,78,139)',...){

  dots=list(...)

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

  if(missing(condition.idx)){
    select.vec=apply(dataframe,MARGIN = 2,function(x){condition[1]%in%x})
    condition.idx=colnames(dataframe)[select.vec]
  }else if(condition.idx%in%colnames(dataframe)){
    condition.idx=condition.idx
  }else{
    condition.idx=colnames(dataframe)[condition.idx]
  }

  condition.idx=enquo(condition.idx)


  if(!missing(condition)){
    sub_dataframe = dataframe%>%
      select(!!ctg.idx,!!num.idx,!!condition.idx)
    sub_dataframe=sub_dataframe[sub_dataframe[,3]%in%condition,]
  }else{
    sub_dataframe=dataframe %>%
      select(!!ctg.idx,!!num.idx,!!condition.idx)
  }

  marker_pos_y = marker_refer
  for(i in 1:length(marker_refer)){
    marker_pos_y[i] = sub_dataframe[sub_dataframe[,1]==marker_pos_x[i] &
                                      sub_dataframe[,3]==marker_refer[i],2]
  }

  sub_dataframe=sub_dataframe%>%spread(!!condition.idx,!!num.idx)
  ## sub_dataframe contains all data for plot
  ## now take care of annotations

  if(!missing(marker_pos_adj)){
    marker_pos_y = as.numeric(marker_pos_y)+marker_pos_adj
  }else{
    marker_pos_y = as.numeric(marker_pos_y)
  }

  p = plot_ly(data=sub_dataframe,x = sub_dataframe[,1])

  for(i in 2:ncol(sub_dataframe)){
    p=add_lines(p,y=sub_dataframe[,i],
                line=list(color=colors[(i-1)],width=4),
                name=colnames(sub_dataframe)[i],showlegend=T)
  }

  marker_text = sapply(seq(1,length(events)),function(x){paste('<b>',x,'</b>')})
  events = as.vector(sapply(events,function(x){paste('<b>',x,'</b>')}))

  Annotations = list()
  for(i in 1:length(events)){
    Annotations[[i]] = list(xref='paper',yref='plot',xanchor="left",yanchor="right",
                            x = text_pos_x, y = ann_pos_y[i], showarrow=F,
                            align='left',text = events[i],
                            font = list(size=12,family='SimSun'))

  }

  Annotations[[(length(events)+1)]] = list(xref="paper",yref="paper",
                                           xanchor="left",yanchor="right",
                                           x=0,y=1.05,showarrow=F,align='left',
                                           text=title,
                                           font=list(size=20,family='heiti'))

  # continue plotting
  p = p%>%add_markers(x = marker_pos_x, y = marker_pos_y,
                      marker = list(size=15,color=marker_color),
                      showlegend=F)%>%
    add_text(x = marker_pos_x, y = marker_pos_y,
             text = marker_text,
             textfont = list(color="white",size=8),
             showlegend=F)%>%
    add_markers(x = rep(ann_pos_x,length(events)), y=ann_pos_y,
                marker = list(size=15,color=marker_color),
                showlegend=F)%>%
    add_text(x = rep(ann_pos_x,length(events)), y=ann_pos_y,
             text = marker_text,
             textfont = list(color="white",size=8),
             showlegend=F)

  ## layout
  if('legend'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['legend'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,legend=list(x=0,y=-0.1,orientation='h',
                                              font=list(size=10),bgcolor="transparent"))))
  }

  if('xaxis'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['xaxis'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,xaxis=list(showgrid=T,nticks=20,
                                             ticks='outside',
                                             title=colnames(sub_dataframe)[1]))))
  }

  p = p%>%layout(yaxis = list(title=yaxis_name,
                              ticklen=0,showgrid=T,
                              zeroline=F,gridwidth=2,
                              showticklabels=T),
                 annotations=Annotations,
                 ...)


  return(p)
}
