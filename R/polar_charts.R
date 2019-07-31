#' Drawing Radar Plot
#'
#' \code{polar_charts} provides polar charts of the dataframe, each row will be a trace in
#' the polar charts.
#'
#' The row name will be served as the name of each trace that will also be shown in the legend.
#' The column name of the dataframe will be used as the tick text of the angular axis of
#' the polar charts. Note that if choose 'fills' as 'toself', then the relative 'fillcolors'
#' should be somewhat transparent, which can be done by setting colors using 'rgba'.
#'
#' @param dataframe: a dataframe object
#' @param colors: a vector, the palette used to draw lines
#' @param fills: a categorical string of either 'none' or 'toself', whether to fill the polygon
#' created by the line
#' @param fillcolors: a vector, the palette used to fill polygons, will be ignored if related
#' 'fills' is set to 'none'
#' @param title: the name of the plot
#' @param ... other plot parameters. See \code{\link[plotly]{layout}} for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a polar plot for your dataframe.
#' @usage polar_charts(dataframe,colors,fills,fillcolors,title,...)
#' @seealso \code{\link[plotly]{plot_ly}},\code{\link[plotly]{layout}}
#' @examples
#' ## prepare data, it's generate from analysis of NASDAQ: MDLZ
#' MDLZ=c(6,5,2,6,1,5,1,1,3,1,2,2,10,10,9,9)
#' others=c(7,9,6,6,3,2,2,2,3,2,4,4,10,10,9,9)
#' polar.dataframe=rbind(MDLZ,others)
#' row.names(polar.dataframe)=c('亿滋国际','市场同业竞争者均值')
#' colnames(polar.dataframe)=c('股息收益','净资产收益率','资产回报率','息税前利润',
#'                             '销售增长率','净收入增长率','营收增长期望','每股盈余期望',
#'                             '市盈率','市售率','企业价值倍数','市现率',
#'                             '总市值','成交量','波动性','风险系数')
#' ## a simple example
#' polar_charts(dataframe=polar.dataframe,colors=c('#FF7F00','#33A02C'),fills=c('toself','none'),
#'              fillcolors=c('#FDBF6F','#B2DF8A'),title='亿滋国际和同行竞争对手股票指标对比',
#'              margin=list(t=56))
#' ## another way
#' polar_charts(dataframe=polar.dataframe,colors=c('#FF7F00','#33A02C'),fills=c('toself','toself'),
#'              fillcolors=c('rgba(253,191,111,0.3)','rgba(178,223,138,0.3)'),
#'              title='亿滋国际和同行竞争对手股票指标对比',
#'              legend=list(x=1,y=0,bgcolor='transparent',font=list(size=14,family='heiti')),
#'              margin=list(t=56),paper_bgcolor='#ccece6')

polar_charts = function(dataframe,colors,fills,fillcolors,title,...){

  dots=list(...)

  dataframe=cbind(dataframe,dataframe[,1])
  colnames(dataframe)[length(colnames(dataframe))]=colnames(dataframe)[1]

  p=plot_ly(type='scatterpolar',mode='lines')

  for(i in 1:nrow(dataframe)){
    p=add_trace(p,r=dataframe[i,],theta=colnames(dataframe),
                name=row.names(dataframe)[i],line=list(width=3,color=colors[i]),
                fill=fills[i],fillcolor=fillcolors[i])
  }

  if('legend'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['legend'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,legend=list(x=1,y=1,
                                              font=list(size=10),bgcolor="transparent"))))
  }

  p=p%>%layout(font=list(family='STKaiti',size=16),
               grid=list(roworder='bottom to top'),
               polar=list(radialaxis=list(showticklabels=F,ticklen=0,visible=T,
                                          zeroline=F,
                                          range=seq(min(dataframe),max(dataframe),1)),
                          angularaxis=list(showline=T)),
               annotations = list(
                 list(xref='paper',yref='paper',xanchor='left',yanchor='right',
                      x=-0.06,y=1.12,showarrow = F,text=title,
                      font=list(size=20,family='heiti'))),...)

  return(p)
}
