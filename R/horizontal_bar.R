#' Horizontal Bar Plot Showing Percentage
#'
#' \code{horizontal_bar} plot a horizontal bar plot, with each small bar represent the
#' percentage of a categorical variable with respect to another categorical variable.
#'
#' The function will first count the number of samples based on the two categorical variables
#' specified by 'h.idx' and 'v.idx', then calculate the percentage of 'h.idx' variable
#' to 'v.idx' variable. When do plotting, one should take good care of the order of 'v_name'
#' and 'h_name'.
#'
#' @param dataframe: a dataframe object
#' @param h.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in horizontal axis, should be a categorical variable
#' @param v.idx: a character or an integer, indicate which column will be selected as the
#' variable to show in vertical axis, should be a categorical variable
#' @param h_name: a vector, indicate the label shows in horizontal axis
#' @param v_name: a vector, indicate the label shows in vertical axis
#' @param colors: a vector, the palette used to plot the bar
#' @param xaxis_name: a character, the name of x axis
#' @param title: a character, the name of the plot
#' @param ... other parameters for plotting, see "Details" and \code{\link[plotly]{layout}}
#' for more information
#' @export
#' @return This function will return an object of class plotly. By calling the function you
#' will get a horizontal bar plot where each bar shows the percentage of samples belong to
#' a category with respect to another cateogry.
#' @usage horizontal_bar(dataframe,h.idx,v.idx,h_name,v_name,colors,
#'                       xaxis_name,title,...)
#' @seealso \code{\link[plotly]{add_trace}},\code{\link[plotly]{layout}}
#' @examples
#' data("tmall_milk_sales")
#' horizontal_bar(dataframe=tmall_milk_sales,h.idx='label',v.idx='pack',
#'                h_name=c('热销产品','普通产品','滞销产品'),
#'                v_name=c('爱克林包装','杯装','袋装','盒装','瓶装'),
#'                colors = brewer.pal(3,'Set1'),xaxis_name='百分比',
#'                title='电商乳制品产品线分类统计')
#' horizontal_bar(dataframe=tmall_milk_sales,h.idx='feature',v.idx='label',
#'                h_name=c('儿童牛奶','养生牛奶','新出产品','其他产品','牛奶饮料','主推产品'),
#'                v_name=c('热销产品','普通产品','滞销产品'),
#'                colors = c('#E41A1C','#377EB8','#4DAF4A','#984EA3',
#'                           '#FF7FF0','#FFD92F'),xaxis_name='百分比',
#'                title='电商乳制品产品线分类统计',paper_bgcolor='#ccece6',
#'                plot_bgcolor='#ccece6')


horizontal_bar = function(dataframe,h.idx,v.idx,h_name,v_name,colors,
                          xaxis_name,title,...){

  if(h.idx%in%colnames(dataframe)){
    h.idx=h.idx
  }else{
    h.idx=colnames(dataframe)[h.idx]
  }


  if(v.idx%in%colnames(dataframe)){
    v.idx=v.idx
  }else{
    v.idx=colnames(dataframe)[v.idx]
  }

  dataframe$count=1
  formula_expression=paste('count ~ ',v.idx,'+',h.idx,sep="")
  formula_expression=as.formula(formula_expression)
  count_df=as.data.frame(xtabs(formula = formula_expression,data=dataframe))
  colnames(count_df)=c('v.col','h.col','n.col')

  get_percent = function(v){
    percent_vec=cal_pct(dataframe=count_df,ctg.idx='h.col',num.idx='n.col',
                        condition=v,condition.idx = 'v.col')$percent
    percent_vec=round(percent_vec,2)
    return(percent_vec)
  }

  percent_df=sapply(unique(count_df$v.col),get_percent)

  label=as.character(unique(count_df[,'h.col']))
  class=as.character(unique(count_df[,'v.col']))
  class=rev(class)
  percent_df=as.data.frame(t(percent_df))
  class_name=factor(v_name,levels=v_name)
  percent_df=cbind(class_name,percent_df)
  colnames(percent_df)=c('class',label)
  percent_df$class = factor(percent_df$class,levels=class_name)

  n.h=length(unique(count_df$h.col))
  n.v=length(unique(count_df$v.col))

  x_position_lst=list()
  x_position_lst[[1]]=percent_df[n.v,2]/2
  for (i in 2:n.h) {
    x_position_lst[[i]]=sum(percent_df[n.v,2:i])+percent_df[n.v,(i+1)]/2
  }
  x_position=unlist(x_position_lst)

  ##plot
  p = plot_ly(percent_df)
  for(i in 1:n.h){
    p=add_trace(p,x=percent_df[,i+1],y=percent_df[,1],type='bar',
                orientation='h',marker=list(color=colors[i],
                line=list(color='rgb(248,248,249)',width=2)))
  }

  p=p%>%layout(xaxis=list(title=xaxis_name,showgrid=F,showline=T,
                          showticklabels=T,zeroline=F,range=c(0,100),
                          domain=c(0.05,1)),
               yaxis=list(title="",showgrid=F,showline=F,type='category',
                          showticklabels=F,zeroline=F),
               annotations = list(
                 list(xref='paper',yref='paper',xanchor='left',yanchor='right',
                      x=0,y=1.1,showarrow = F,text=title,
                      font=list(size=20,family='heiti'))),
               barmode='stack',margin=list(t=64,l=40),
               showlegend=FALSE,...)%>%
    add_annotations(xref='paper',yref='y',x=0.04,y=percent_df$class,
                    xanchor='right',text=percent_df$class,font=list(size=12,color='black'),
                    showarrow=FALSE,align='right')%>%
    add_annotations(xref='x',yref='paper',x=x_position,y=1.03,text=h_name,
                    font=list(size=12,color='black'),showarrow=FALSE)

  return(p)

}
