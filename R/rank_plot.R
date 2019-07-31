#' Plotting Change of Ranks of a Categorical Variable to an Index
#'
#' \code{rank_plot} gives a plot of ranks for a categorical variable. The function makes
#' it clear to see the trend of each category and how their relations change.
#'
#' This function plot ranks of different category, no matter which format your data is.
#' If you have dataframe with a column serves as index, one as the numeric to display,
#' and another one as categories. Then plug them into this function, you will generate
#' a line plot with each line represent the rank change of a category over index.
#' Or if you dataframe contains multiple numeric columns, just leave condition.idx missing
#' and the function will give a line plot with each line represent the rank change of each
#' numeric column you selected. "criteria" is used to decide at which particular index
#' will the top_N selection been done. While if there are too many categories
#' you can use "top_N" to subset only the top N categories with biggest numerical value
#' with respect to criteria to be used in the plot. Some unlisted option is also available,
#' such as xaxis, paper_bgcolor and margin.
#' See \code{\link[plotly]{layout}} for more information.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in x axis
#' @param num.idx: a vector of character or an integer, indicate which column(s)
#' will be selected as the variable to show in y axis
#' @param condition.idx: a character or an integre, indicate which column will be treated as
#' the legend name. See "Details" for more information
#' @param criteria: a character or a numeric, depend on the class of element of
#' the column specified by ctg.idx. See "Details" for more information
#' @param top_N: a integer, if there are too many categories in the legend,
#' use this argument to choose the top n levels with respect to criteria.
#' See "Details" for more information
#' @param colors: a vector, the palette used for plotting
#' @param yaxis_name: name of y axis
#' @param title: the the title of the plot
#' @param ... other parameters for plotting, mainly layout options.
#' See "Details" and \code{\link[plotly]{layout}} for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a line plot of multiple categorical variables' rank to an specified index.
#' @usage rank_plot(dataframe,ctg.idx,num.idx,condition.idx,criteria,top_N,colors,
#'                  yaxis_name,title,...)
#' @seealso \code{\link[plotly]{plot_ly}},\code{\link[plotly]{layout}}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' data("estate_fin_charts")
#' rank_plot(dataframe=estate_fin_charts,ctg.idx = 'Year',num.idx = 'margin',
#'           condition.idx = '证券简称',criteria=2016,top_N=5,
#'           colors=brewer.pal(5,'Set1'),
#'           yaxis_name='利润排名',
#'           title='2016年利润排名前5的房地产企业历年排名变化',
#'           paper_bgcolor='#ccece6',margin=list(t=36,l=24))
#' \dontrun{
#' ## the following code will generate warnings saying that there are only 6 linetypes
#' ## but the function have taken good care of linetypes by sample with replacement
#' ## Don't worry, just ignore those warnings
#' rank_plot(dataframe=estate_fin_charts,ctg.idx = 'Year',num.idx = 'income',
#'           condition.idx = '证券简称',criteria=2016,top_N=12,
#'           colors=brewer.pal(12,'Set3'),
#'           yaxis_name='营收排名',
#'           title='2016年营收排名前5的房地产企业历年排名变化',
#'           paper_bgcolor='#ccece6',margin=list(t=36,l=24))
#' }
#' data("macro_data_chn")
#' test_df = macro_data_chn
#' test_df[1:3,9] <- NA
#' rank_plot(dataframe=test_df,ctg.idx='year',num.idx=c(9:12),
#'           criteria = 2016,colors = brewer.pal(4,'Set1'),
#'           yaxis_name = '商品价格排名',
#'           title='一些大宗商品的历年价格排名',
#'           xaxis = list(showgrid=T,nticks=5,ticklen=4,tickangle=-45,
#'                        ticks='outside',tickmode="auto",
#'                        type='category',title="年份"),
#'           paper_bgcolor='#ccece6',margin=list(t=36,l=24))
#'

rank_plot=function(dataframe,ctg.idx,num.idx,condition.idx,criteria,top_N,colors,
                   yaxis_name,title,...){

  dots = list(...)

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

  if(!missing(condition.idx)){
    if(condition.idx%in%colnames(dataframe)){
      condition.idx=condition.idx
    }else{
      condition.idx=colnames(dataframe)[condition.idx]
    }
    condition.idx=enquo(condition.idx)
    sub_dataframe = dataframe%>%select(!!ctg.idx,!!condition.idx,!!num.idx)
    colnames(sub_dataframe)=c('index','name','value')
  }else{
    sub_dataframe = dataframe%>%
      select(!!ctg.idx,!!num.idx)%>%
      gather(name,value,-!!ctg.idx)
    colnames(sub_dataframe)[1]='index'
  }

  sub_dataframe_rank=get_rank(dataframe=sub_dataframe,ctg.idx='name',
                              num.idx='value',condition=criteria,condition.idx = 'index')

  if(missing(top_N)){
    top_N=nrow(sub_dataframe_rank)
  }else{
    top_N=top_N
  }

  top_N_name=sub_dataframe_rank%>%
    filter(rank<=top_N)%>%
    select(name,rank)
  top_N_name=top_N_name[order(top_N_name$rank),1]

  filter_dataframe=sub_dataframe%>%filter(name%in%top_N_name)

  rank_level=function(i){
    rank_dataframe=get_rank(dataframe = filter_dataframe,ctg.idx = 'name',
                            num.idx = 'value',condition = i,condition.idx = 'index')
    rank_dataframe$value=i
    return(rank_dataframe)
  }

  plot_df=lapply(unique(filter_dataframe$index),rank_level)
  plot_df=bind_rows(plot_df)


  sample_pch=function(n){
    possible_pch=c("0","circle","100","circle-open","200",
                   "circle-dot","300","circle-open-dot","1","square","101",
                   "square-open","201","square-dot","301","square-open-dot",
                   "2","diamond","102","diamond-open","202")
    sample_pch=sample(possible_pch,n,replace=TRUE)
  }

  sample_lty=function(n){
    possible_lty=c('solid','dot',"dash","longdash","dashdot","longdashdot")
    sample_lty=sample(possible_lty,n,replace = TRUE)
  }

  p = plot_ly(data=plot_df,x=plot_df[,2],y=plot_df[,3],
                               type='scatter',mode='lines+markers',
              split=plot_df[,1],color=plot_df[,1],colors=colors,name=plot_df[,1],
              symbol=plot_df[,1],symbols = sample_pch(top_N),
              linetype=plot_df[,1],linetypes = sample_lty(top_N))%>%
      layout(legend=list(bgcolor='transparent'),
             yaxis = list(showgrid=T,showticklabels=T,ticklen=4,
                          tickmode='array',tickvals=seq(1,top_N),ticktext=seq(1,top_N),
                          zeroline=F,gridwidth=2,title=yaxis_name))

  if('xaxis'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['xaxis'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,xaxis=list(showgrid=T,nticks=20,ticklen=4,tickangle=-45,
                                             ticks='outside',tickmode="array",
                                             type='category',title=""))))
  }

  p = p%>%layout(annotations = list(
    list(xref='paper',yref='paper',xanchor='left',yanchor='right',
         x=0,y=1.05,showarrow = F,text=title,
         font=list(size=20,family='heiti'))),...)


  return(p)
}
