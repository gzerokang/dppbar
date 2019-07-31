#' Bar Plot of Different Categories
#'
#' Bar plots is very common in data visualization. \code{bar_plot} makes grouped bar plot
#' with manipulated data.
#'
#' This function makes it easier to draw bar plot no matter which format your data have.
#' If you have dataframe with a column serves as index, one as the numeric to display,
#' and another one as categories. Then by plug in this function, you will generate
#' a bar plot with categories serves as legend. Or if you dataframe contains multiple
#' numeric columns, then just leave condition.idx missing and the function will use column
#' names of your numeric columns as legend. "criteria" is used to decide the order of bars
#' for each categories and if there are too many of them you can use "top_N" which
#' select the top N categories with biggest numerical value with respect to criteria.
#' Some unlisted option can be take into the function, such as paper_bgcolor and
#' margin. See \code{\link[plotly]{layout}} for more information.
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
#' @param xaxis_name: name of x axis
#' @param yaxis_name: name of y axis
#' @param title: the the title of the plot
#' @param ... other parameters for plotting, mainly layout options.
#' See "Details" and \code{\link[plotly]{layout}} for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a bar plot of multiple categorical variables with respect to an specified index.
#' @usage bar_plot(dataframe,ctg.idx,num.idx,condition.idx,criteria,top_N,colors,
#'                  xaxis_name,yaxis_name,title,...)
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{ggplot}},
#' \code{\link[plotly]{layout}}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' data("estate_fin_charts")
#' bar_plot(dataframe=estate_fin_charts,ctg.idx = 'Year',num.idx = 'income',
#'          condition.idx = '证券简称',criteria=2016,top_N=12,
#'          colors=brewer.pal(12,'Set3'),
#'          xaxis_name='年份',yaxis_name='营业收入（亿元）',
#'          title='2016年营业收入前12名房地产企业历年营收变化',
#'          paper_bgcolor='#ccece6',margin=list(t=36,l=24))
#' ## another example, where condition.idx is missing
#' data("macro_data_chn")
#' bar_plot(dataframe=macro_data_chn,ctg.idx='year',num.idx=c(9:12),
#'          criteria = 2016,colors = brewer.pal(4,'Set1'),
#'          xaxis_name = '年份',yaxis_name = '商品价格（元/吨）',
#'          title='一些大宗商品的历年价格变化',
#'          paper_bgcolor='#ccece6',margin=list(t=36,l=24))

bar_plot = function(dataframe,ctg.idx,num.idx,condition.idx,criteria,top_N,colors,
                    xaxis_name,yaxis_name,title,...){

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

  ## just in case there are missing values
  sub_dataframe[is.na(sub_dataframe)]=0


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

  plot_df=sub_dataframe%>%
    filter(name%in%top_N_name)
  plot_df$name=factor(plot_df$name,levels = top_N_name)
  plot_df = plot_df[order(plot_df$name),]

  bar_plot = ggplot(plot_df,aes(x=plot_df[,1],y=plot_df[,3],
                                fill=plot_df[,2])) +
             geom_bar(alpha=0.8,stat='identity') +
             scale_x_discrete(limits = plot_df[,1]) +
             scale_fill_manual(values=colors) +
             ylab(yaxis_name) + xlab(xaxis_name) +
             guides(fill=guide_legend(title = NULL))

  bar_plot = ggplotly(bar_plot)%>%
    layout(legend=list(bgcolor='transparent'),
      annotations = list(
      list(xref='paper',yref='paper',xanchor='left',yanchor='right',
           x=0,y=1.05,showarrow = F,text=title,
           font=list(size=20,family='heiti'))),...)

  return(bar_plot)
}
