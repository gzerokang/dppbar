#' Transform numerical variable into categorical variable
#'
#' Transform a numerical column into a categorical column, based on specified conditions.
#'
#' If partition is an interger, "type" should be set to equal,
#' which means the numerical variable will be separated into equal groups.
#' If partition is a vector and sums up to 1,then type should be set to "quantile",
#' which means the cut will be based on those specified quantiles.
#' Otherwise set type to "criteria", which means groupping will be based on whether
#' the element of numerical variable is less than or equal to criterias.
#'
#' @param dataframe: a dataframe-like object
#' @param col.id: an integer, specify which column in the dataframe that you need to transfer
#' @param col.name: a character, another way to specify the to-be-transformed column,
#' will be disregarded if col.id is specified
#' @param partition: an interger or a vectorof numerics, based on the type, specified how
#' to transfer.
#' @param level.name: a vactor of strings, the category that will be assigned to each group of
#' numerical variable
#' @param type: a character that can take values of "quantile", "equal", and "criteria", see
#' "Details", default "quantile"
#' @export
#' @return Categorical vairable which is transformed from the assigned numerical variable
#' based on the specified conditions.
#' @usage
#' num2ctg(dataframe,col.id,col.name=NA,partition,level.name,type='quantile')
#' @examples
#' data(macro_data_chn)
#' test_df=macro_data_chn
#' test_df$PPI[2:4]=NA
#' test_df$PPI
#' num2ctg(test_df,col.name = 'PPI',partition = c(0.2,0.5,0.3))
#' num2ctg(test_df,col.name = 'PPI',partition = c(98,100),
#' level.name = c('low','medium','high'),type='criteria')
#' num2ctg(test_df,col.name = 'PPI',partition = 3,
#' level.name = c('low','medium','high'),type='equal')
#' num2ctg(test_df,col.id = 4,partition = 3,
#' level.name = c('low','medium','high'),type='equal')

num2ctg = function(dataframe,col.id,col.name=NA,partition,level.name,type='quantile'){
  if(missing(col.id)){
    col.id=match(col.name,colnames(dataframe))
  }

  vec_copy=dataframe[,col.id]
  vec_copy[is.na(vec_copy)]=max(vec_copy,na.rm=T)+max(partition)+1
  sort_vec=sort(vec_copy,index.return=TRUE)
  n.total=nrow(dataframe)
  if(sum(is.na(dataframe[,col.id]))>0){
    n.na=sum(vec_copy==max(vec_copy))
  }else{
    n.na=0
  }
  n=n.total-n.na
  n.partition=length(partition)

  if(type=='equal'){
    partition=rep(1/partition,partition)
  }else if(type=='criteria'){
    partition=sort(partition)
    vec_copy_new=c(vec_copy,partition)
    rank_vec=c(0,rank(vec_copy_new,ties.method='first')[-(1:n.total)])
    partition=c(partition,1)
    for(i in 1:n.partition){
      partition[i]=(rank_vec[i+1]-rank_vec[i]-1)/n
    }
    partition[n.partition+1]=1-sum(partition[1:n.partition])
  }else{
    partition=partition
  }

  if(missing(level.name)){
    if(type=='criteria'){
      level.name=sapply(1:(n.partition+1),function(i){paste0('L',i)})
    }else{
      level.name=sapply(1:n.partition,function(i){paste0('L',i)})
    }
  }

  p=0
  ctg_vec=dataframe[,col.id]
  for(i in 1:length(partition)){
    start=max(1,ceiling(p*n+0.1))
    p=p+partition[i]
    end=min(n,floor(p*n))
    ctg_vec[sort_vec$ix[start:end]]=level.name[i]
  }

  return(ctg_vec)
}
