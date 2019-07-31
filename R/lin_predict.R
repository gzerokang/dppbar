#' Linear Extrapolation Using Dynamic Linear Model
#'
#' \code{lin_predict} generate extrapolation of a numeric variable based on dynamic linear
#' model. The result is shown as a dataframe containing predict mean and confidence interval
#' as well as a plot for visualization
#'
#' It's really often to do prediction or extrapolation of a numerical variable, especially
#' when it is a time series. This function accomplish this task by using dynamic linear
#' model, more specifically, random walk plus trend model. The explicit formula of the
#' model can be found in Petris' book Dynamic Linear Model with R. The procedure is also
#' suggested in the book. The function will first build a random walk plus trend model
#' using \code{\link[dlm]{dlm}}, then estiamte parameters in dlm by \code{\link[dlm]{dlmMLE}}.
#' After that, \code{\link[dlm]{dlmFilter}} will be used to filter and get one-step-ahead
#' forecast for model checking. Finally using \code{\link[dlm]{dlmForecast}}, one can get
#' the forecast. The plot is a nice way to see whether the model works, by judging the gap
#' between model fitting and real data. When setting x axis of the plot, if 'ctg.idx' in
#' the function is specified, 'extra_names' should also be specified based on the index
#' choosen by 'ctg.idx', or just miss both of them, then a sequence start from 1 with length
#' equal to the number of real data plus extrapolations will be applied as tick text of x
#' axis. Only given value to either one of them will generate error. Note that since the
#' extrapolation is done using a random walk plus trend model, you must have some
#' confidence that the series you want to forecast do have some trend as a priori,
#' (it is the case for many macroeconmic series), otherwise the model may not converge
#' and the result can be badly wrong or the prediciton may have really large variance that
#' results in the confidence interval making no sense.
#'
#' @param dataframe: a dataframe object
#' @param ts.idx: a character or a numeric, indicate the column in the dataframe that you
#' want to get prediction with
#' @param t_ahead: an integer, how many predictions whould be made
#' @param addCI: a logical, whether to add confidence interval to the visualization
#' @param ctg.idx: a character or a numeric, indicate the column in the dataframe that will
#' be used as the x axis index in the plot. See "Details" for more information
#' @param extra_names: a vector of strings, will be used as the x axis tick text for
#' prediction points. See "Details" for more information
#' @param xaxis_name: the name of x axis for the plot
#' @param yaxis_name: the name of y axis for the plot
#' @param title: the name of the plot
#' @param ... other plot settings. See \code{\link[plotly]{layout}} for more information
#' @export
#' @return A list with two elements called pred.mtx and pred.plot respectively.
#' The pred.mtx is a dataframe containing the predict mean, 95% confidence interval for
#' the series and period you specified. The pred.plot is a plotly object which is a
#' visualization of how the dynamic linear model with trend fits original data and
#' the trend of future prediction and confidence intervals.
#' @usage lin_predict(dataframe,ts.idx,t_ahead,addCI,ctg.idx,extra_names,xaxis_name,
#'                    yaxis_name,title,...)
#' @seealso \code{\link[dlm]{dlm}},\code{\link[dlm]{dlmMLE}}
#' \code{\link[dlm]{dlmFilter}},\code{\link[dlm]{dlmForecast}}
#' @references Petris, Giovanni and Petrone, Sonia and Campagnoli, Patrizia(2009)
#' Dynamic Linear Models with R. \emph{Journal of Applied Statistics}
#' Volumn 38, pages 31-84, DOI: 10.1007/b135794_2
#' Petris, Giovanni and Petrone, Sonia and Campagnoli, Patrizia(2009)
#' Dynamic Linear Models with R. Book published by Springer.
#' ISBN 978-0-387-77237-0
#' Giovanni Petris, dlm: an R package for Bayesian analysis of Dynamic Linear Models
#' Vignette of R package dlm. Available at:
#' \url{https://cran.r-project.org/web/packages/dlm/vignettes/dlm.pdf}
#' @author Jizhou Kang <jizhou_kang@hotmail.com>
#' @examples
#' data("macro_data_chn")
#' GDP_predict=lin_predict(dataframe=macro_data_chn,ts.idx='GDP',t_ahead=3,
#'                         addCI=T,xaxis_name='Time',yaxis_name='GDP(元)',
#'                         title='GDP未来三年预测')
#' GDP_predict$pred.mtx
#' GDP_predict$pred.plot
#' GDP_predict=lin_predict(dataframe=macro_data_chn,ts.idx='GDP',t_ahead=3,addCI=T,
#'                         ctg.idx=1,extra_names = c(2017,2018,2019),
#'                         xaxis_name='年份',yaxis_name='GDP(元)',title='GDP未来三年预测',
#'                         legend=list(x=0.72,y=0.1,bgcolor='transparent'),
#'                         margin=list(t=45,l=45,r=18),paper_bgcolor='#ccece6')
#' GDP_predict$pred.mtx
#' GDP_predict$pred.plot

lin_predict = function(dataframe,ts.idx,t_ahead,addCI,ctg.idx,extra_names,xaxis_name,
                       yaxis_name,title,...){

  dots=list(...)

  n=nrow(dataframe)
  time_vec=seq(1,n,1)

  time_series=ts(as.matrix(dataframe)[,ts.idx],start=c(1,1))

  ## parameter estimation
  build=function(param){
    dlm(FF=matrix(c(1,0),nr=1),GG=matrix(c(1,0,1,1),nrow=2),
        V=param[1],W=matrix(c(param[2],0,0,param[3]),nrow=2),
        m0=c(param[4],param[5]),C0=1*diag(2))
  }

  fit_model=dlmMLE(time_series,c(1,1,1,0,0),build,method='L-BFGS-B',lower=c(1e-6,0))

  ## building dlm
  dlmModel=dlm(FF=matrix(c(1,0),nr=1),GG=matrix(c(1,0,1,1),nrow=2),
               V=fit_model$par[1],W=matrix(c(fit_model$par[2],0,0,fit_model$par[3]),nrow=2),
               m0=c(fit_model$par[4],fit_model$par[5]),C0=1*diag(2))

  ## filtering
  Filt=dlmFilter(time_series,dlmModel)

  ## forecast
  fore=dlmForecast(Filt,nAhead=t_ahead)
  lower95_forecast_y=fore$f-qnorm(0.025,lower=FALSE)*sqrt(unlist(fore$Q))
  upper95_forecast_y=fore$f+qnorm(0.025,lower=FALSE)*sqrt(unlist(fore$Q))

  pred=matrix(0,t_ahead,4)
  pred[,1]=seq(1,t_ahead,1)
  pred[,2]=round(as.vector(fore$f),2)
  pred[,3]=round(as.vector(lower95_forecast_y),2)
  pred[,4]=round(as.vector(upper95_forecast_y),2)
  pred=as.data.frame(pred)
  colnames(pred)=c('t_ahead','Model Prediction','Lower 95% C.I.','Upper 95% C.I.')

  ## plot
  p=plot_ly()%>%
    add_trace(x = time_vec, y = time_series,mode = 'lines+markers',
              marker=list(symbol='circle',size=10),type='scatter',
              color = I("black"), name = "Real data") %>%
    add_trace(x = time_vec, y = Filt$f, mode = 'lines+markers',
              marker=list(symbol='x',size=10),type='scatter',
              color = I("red"), name = "Fitted by model")
  if(addCI){
    p=p%>%
      add_ribbons(x = seq(max(time_vec)+1,max(time_vec)+t_ahead,1),
                  ymin = as.vector(lower95_forecast_y),
                  ymax = as.vector(upper95_forecast_y),
                  color = I("darkgrey"), name = "95% C.I.")
  }else{
    p=p
  }



  p=p%>%
    add_trace(x = seq(max(time_vec)+1,max(time_vec)+t_ahead,1), y = fore$f,
              color=I('blue'),mode='lines+markers',
              marker=list(symbol='10',size=10),type='scatter',
              name = "Model forecast")%>%
    add_lines(x=c(max(time_vec),max(time_vec)+1),
              y=c(Filt$m[dim(Filt$m)[1],1],fore$f[1]),
              line=list(color=I('black'),dash = 'dash'),showlegend = FALSE)

  if('legend'%in%names(dots)){
    p = do.call(layout,c(list(p=p),dots['legend'%in%names(dots)]))
  }else{
    p = do.call(layout,c(list(p=p,legend=list(x=0.05,y=0.96,
                                              font=list(size=10),bgcolor="transparent"))))
  }

  p=p%>%
    layout(annotations = list(list(xref = "paper", yref = "paper",
                                   xanchor = "left", yanchor = "right",
                                   x =0, y = 1.05, showarrow = F,
                                   text = title,
                                   font = list(size = 20,family='heiti'))),
           yaxis = list(side = "left", title=yaxis_name, ticklen = 2,
                        gridwidth = 2))

  if(missing(ctg.idx) & missing(extra_names)){
    p=p%>%
      layout(xaxis = list(showgrid = F, ticklen = 4, nticks = 15,zeroline=F,
                          ticks = "outside",tickmode = "auto",
                          tickangle = -45,title = xaxis_name),...)
  }else{
    tick_text=c(dataframe[,ctg.idx],extra_names)
    extra_value=seq(max(time_vec)+1,max(time_vec)+t_ahead,1)
    tick_value=c(time_vec,extra_value)
    p=p%>%layout(xaxis=list(showgrid=F,ticklen=4,zeroline=F,ticks='outside',
                            tickmode='array',tickvals=tick_value,ticktext=tick_text,
                            tickangle = -45, title=xaxis_name),...)
  }


  result=list(pred.mtx=pred,pred.plot=p)
  return(result)

}
