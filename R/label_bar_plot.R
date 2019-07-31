#' Labeled Bar Plot
#'
#' \code{label_bar_plot} plot a categorical variable verses a numerical variable, with a
#' small tag on the right side of each bar indiciting the value of each bar.
#'
#' This function makes simple but fancy bar plot by adding a small tag to the right side
#' of the bar. It can deal with multiple type of data, no matter how numerical variables are
#' organized. If there are to much to show, it also enables you to select part of the data
#' by specify criteria. If you have a cross-section data, which means that there's no
#' variable that can be choosen as the ctg.idx and criteria. Then you can simply add
#' a column to the dataframe, with the same element, and set ctg.idx as the column index or
#' column name of the new column, while set criteria as the unique element of that column.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' criteria to select part of the data
#' @param num.idx: a vector of character or an integer, indicate which column(s)
#' will be selected as the numerical variable (x axis in the plot)
#' @param condition.idx: a character or an integer, indicate which column will be treated as
#' the categorical variable (y axis in the plot)
#' @param criteria: a character or a numeric, depend on the class of element of
#' the column specified by ctg.idx. See "Details" for more information
#' @param top_N: a integer, if there are too many categories in the legend,
#' use this argument to choose the top n levels with respect to criteria.
#' See "Details" for more information
#' @param colors: a vector or a character, the palette used for plotting
#' @param xaxis_name: name of x axis
#' @param title: name of the plot
#' @param paper_bgcolor: background color of the whole plot, default to "#f2f2f2"
#' @export
#' @return This function will return an object of class ggplot. By calling the function you
#' will get a bar plot of a numerical variables with respect to an categorical variable.
#' @usage static_bar_plot(dataframe,ctg.idx,num.idx,condition.idx,
#'                        criteria,top_N,colors,xaxis_name,title,
#'                        paper_bgcolor='#f2f2f2')
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{theme}}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' data("estate_fin_charts")
#' label_bar_plot(dataframe=estate_fin_charts,ctg.idx='Year',num.idx='roa',
#'                 condition.idx = '证券简称',criteria=2016,top_N=10,colors='#377EB8',
#'                 xaxis_name = 'ROA',title='房地产企业2016年ROA排名前十企业')
#' data("macro_data_chn")
#' label_bar_plot(dataframe=macro_data_chn,ctg.idx='year',num.idx=c(9:12),
#'                 criteria=2016,colors=brewer.pal(4,'Set1'),
#'                 xaxis_name = '价格（元/吨）',title='大宗商品商品2016年价格',
#'                 paper_bgcolor = '#ccece6')
#' ## deal with cross-section data
#' ## a silly example but illustrate the idea
#' test_df=estate_fin_charts%>%filter(Year==2016)%>%select(证券简称,income)
#' ## if this test_df is the data you have
#' ## you can generate a labeled bar plot by the following method
#' test_df$Year=2016
#' label_bar_plot(dataframe=estate_fin_charts,ctg.idx='Year',num.idx='roa',
#'                condition.idx = '证券简称',criteria=2016,top_N=10,colors='#377EB8',
#'                xaxis_name = 'ROA',title='房地产企业2016年ROA排名前十企业',
#'                paper_bgcolor='#ccece6')

label_bar_plot = function(dataframe,ctg.idx,num.idx,condition.idx,
                           criteria,top_N,colors,xaxis_name,title,
                           paper_bgcolor='#f2f2f2'){

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

  plot_df=sub_dataframe%>%
    filter(name%in%top_N_name & index==criteria)
  plot_df$name=factor(plot_df$name,levels = top_N_name)
  plot_df = plot_df[order(plot_df$name),]

  mytheme = theme(axis.text.y=element_text(size=rel(1.5),colour='black',family='STKaiti'),
                  axis.title.x = element_text(size=rel(1.4),family = 'STKaiti'),
                  plot.title = element_text(size=rel(2),family='STKaiti'),
                  plot.background = element_rect(fill=paper_bgcolor),
                  panel.background = element_rect(fill='#ffffff'),
                  panel.grid.major = element_line(colour='#f2f2f2'))

  p=ggplot(plot_df,aes(x=plot_df[,2],y=plot_df[,3]))+
    geom_bar(stat = 'identity',fill=colors)+coord_flip()+
    geom_label(aes(label=round(plot_df[,3],2)))+
    labs(x="",y=xaxis_name,title=title)+mytheme

  return(p)

}
