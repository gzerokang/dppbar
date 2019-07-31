#' Bar Plots in Small Facet
#'
#' \code{facet_bar} separates a whole bar plot into smaller bar plots displayed in small
#' facets. It is used to show multiple (up to three) categorical information. A similar
#' plot can be found in package \code{"lattice"} where it is called \code{\link{barchart}}
#'
#' If "num.idx" is missing, then only histogram can be plot, where the height of each bar
#' equals to the count of that specific category, selected by "ctg.idx", "condition.idx",
#' and "label.idx". On the other hand, if a num.idx is choosen, then you need to switch "type"
#' to "bar" where the total of the numerical variables you selected that belong to a particular
#' category will be used as the height of bar.
#'
#' @param dataframe: a dataframe object
#' @param ctg.idx: a character or an integer, indicate which column will be selected as the
#' categorical variable shown in x axis
#' @param num.idx: a vector of character or an integer, indicate which column
#' will be selected as the numerical variable shown in y axis,
#' only valid when "type" is bar
#' @param condition.idx: a character or an integer, indicate which column will be treated as
#' the legend
#' @param label.idx: a character or an integer, indicate to which column should the split of
#' facets been done
#' @param legend_name: title of the legend
#' @param legend_label: a vector of string, name of legend ticks
#' @param colors: a vector or a character, the palette used for plotting
#' @param xaxis_name: name of x axis
#' @param xaxis_label: name of ticks of the x axis
#' @param yaxis_name: name of y axis
#' @param title: name of the plot
#' @param type: a character of either 'histogram' or 'bar', indicating the way for drawing
#' bars, default to 'histogram'. See "Details" for more information
#' @param stack: a logical indicate whether the bar should be stack or dodged,
#' default to TURE which means stack
#' @param paper_bgcolor: background color of the whole plot, default to "#f2f2f2"
#' @export
#' @return This function will return an object of class ggplot. By calling the function you
#' will get a plot with multiple facet where each of them displays a bar chart or histogram.
#' @usage facet_bar(dataframe,ctg.idx,num.idx,condition.idx,label.idx,
#'                  legend_name,legend_label,colors,
#'                  xaxis_name,xaxis_label,yaxis_name,title,
#'                  type='histogram',stack=F,paper_bgcolor="#f2f2f2")
#' @seealso \code{\link[ggplot2]{facet_wrap}}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' data("tmall_milk_sales")
#' facet_bar(dataframe=tmall_milk_sales,ctg.idx='label',num.idx=9,
#'           condition.idx='promotion',label.idx='brand',legend_name='是否促销',
#'           legend_label=c('不促销','促销'),
#'           colors=c('#A6CEE3','#1F78B4'),xaxis_name='产品类型',
#'           xaxis_label=c('热销产品','普通产品','滞销产品'),
#'           yaxis_name='价格（元）',title='线上乳制品分类销售情况',
#'           type='bar',stack=T,paper_bgcolor='#ccece6')
#' ## another type of plot
#' facet_bar(dataframe=tmall_milk_sales,ctg.idx='class',num.idx=NA,
#'           condition.idx='label',label.idx='brand',legend_name='产品类型',
#'           legend_label=c('热销产品','普通产品','滞销产品'),
#'           colors=brewer.pal(3,'Set2'),xaxis_name='分类',xaxis_label=c('酸奶','牛奶'),
#'           yaxis_name='产品数（件）',title='线上乳制品分类销售情况',
#'           paper_bgcolor='#ccece6')


facet_bar = function(dataframe,ctg.idx,num.idx,condition.idx,label.idx,
                     legend_name,legend_label,colors,
                     xaxis_name,xaxis_label,yaxis_name,title,
                     type='histogram',stack=F,paper_bgcolor="#f2f2f2"){

  if(stack){
    position='stack'
  }else{
    position='dodge'
  }

  if(type=='histogram'){
    p = ggplot(dataframe,aes(x=dataframe[,ctg.idx])) +
      geom_bar(aes(fill=dataframe[,condition.idx]),stat='count',position=position)
  }else{
    p = ggplot(dataframe,aes(x=dataframe[,ctg.idx],y=dataframe[,num.idx],
                             fill=dataframe[,condition.idx])) +
      geom_bar(stat='identity',position=position)
  }

  nlabels=length(unique(dataframe[,label.idx]))

  p = p + facet_wrap(~dataframe[,label.idx],ncol=nlabels/2)

  p = p +
    scale_fill_manual(values = colors,labels=legend_label) +
    scale_x_discrete(labels=xaxis_label) +
    theme(text = element_text(family = 'STKaiti'),
          plot.background = element_rect(fill=paper_bgcolor),
          plot.title = element_text(size=rel(2)),
          panel.background = element_rect(fill='#ffffff'),
          panel.grid.major = element_line(colour='#f2f2f2'),
          legend.background = element_rect(fill=paper_bgcolor),
          strip.background = element_rect(fill='#f5f5f5'),
          axis.text.x = element_text(angle=45,hjust=1,vjust=1)) +
    xlab(xaxis_name)+ylab(yaxis_name)+labs(fill=legend_name,title=title)


  return(p)

}
