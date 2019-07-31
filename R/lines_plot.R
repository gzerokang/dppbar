#' Making Lines Plot with Multiple Options
#'
#' Line plots are the most basic and commonly-used plots in data visualization.
#' \code{lines_plot} makes line plot with multiple choice on the display.
#'
#' This function makes it easier to draw line plot no matter which format your data have.
#' For beginners, it takes relatively little arguments and provided a fancy look line plot.
#' For sophisticated users, flexibility can be achieved by the three dots structure.
#' Some unlisted option can be take into the function, such as legend, paper_bgcolor and
#' margin. See \code{\link[plotly]{layout}} for more information.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param num.idx: a character or an integer, indicate which column(s) will be selected as the
#' variable to show in y axis
#' @param condition: a vector, indicate how to select part of the dataframe
#' @param condition.idx: a character or an integer, indicate which column is the condition in.
#' Should be given some value if the condition appears in multiple columns in the dataframe
#' @param colors: a vector, the i-th element represent the color that will be used to draw
#' the i-th line, which refers to the numeric value of i-th condition
#' @param mode: how should each line displayed, usually be set as 'lines' or 'lines+markers',
#' see \url{https://plot.ly/r/reference/#scatter} for more information
#' @param yaxis_name: name of y axis
#' @param linewidth: the width of each line, default to 2
#' @param title: the title of the plot
#' @param ... other parameters for plotting, see "Details" and \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a lines plot which is displayed as you sepcified in the function.
#' @usage lines_plot(dataframe,ctg.idx,num.idx,condition,condition.idx,
#'                   colors,mode='lines',yaxis_name="",linewidth=2,
#'                   title,...)
#' @seealso \code{\link[plotly]{plot_ly}},\code{\link[plotly]{layout}}
#' @examples
#' data("dairy_fin_charts")
#' lines_plot(dataframe=dairy_fin_charts,ctg.idx = 5,num.idx = 2,
#'            condition=c('伊利股份','蒙牛股份','光明乳业'),
#'            colors=c("#00526d","#de6e6e","#32ab60"),
#'           yaxis_name = '营业收入',linewidth = 4,
#'            title='乳制品企业营业收入图')
#' lines_plot(dataframe=dairy_fin_charts,ctg.idx = 5,num.idx = 2,
#'            condition=c('伊利股份','蒙牛股份','光明乳业'),
#'            colors=c("#00526d","#de6e6e","#32ab60"),
#'            mode='lines+markers',linewidth = 2,
#'           title='乳制品企业营业收入图',
#'           yaxis_name='营业收入',
#'           xaxis=list(showgrid=F,nticks=10,ticklen=4,tickangle=-45,
#'                      ticks='outside',tickmode="array",
#'                      type='category',title="年份"),
#'           yaxis=list(visible=F),
#'           legend=list(x=0.5,y=0.1,orientation='h',
#'                       font=list(size=10),bgcolor="transparent"),
#'           paper_bgcolor='#ccece6',margin=list(t=32,l=32,r=32))



lines_plot = function(dataframe,ctg.idx,num.idx,condition,condition.idx,
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
  }

  p=plot_ly(sub_dataframe,x=sub_dataframe[,1],y=sub_dataframe[,3],type='scatter',mode=mode,
            split=sub_dataframe[,2],
            color=sub_dataframe[,2],colors=colors,name=sub_dataframe[,2],
            line=list(width=linewidth))


  if('xaxis'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['xaxis'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,xaxis=list(showgrid=T,nticks=10,ticklen=4,
                                             ticks='outside',tickmode="array",
                                             type='category',title=""))))
  }

  if('yaxis'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['yaxis'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,yaxis=list(showgrid=T,ticklen=0,
                                             zeroline=F,gridwidth=2,
                                             showticklabels=T,title=yaxis_name))))
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
