## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo=TRUE,fig.width=6,fig.height=4)

## ----message=FALSE-------------------------------------------------------
library(dppbar)

## ----echo=FALSE----------------------------------------------------------
data("dairy_fin_charts")
head(dairy_fin_charts,5)

## ----echo=FALSE----------------------------------------------------------
data("estate_fin_charts")
head(estate_fin_charts[,1:5],5)

## ----echo=FALSE----------------------------------------------------------
data("machinery_fin_charts")
head(machinery_fin_charts[,1:5],5)

## ----echo=FALSE----------------------------------------------------------
data("macro_data_chn")
head(macro_data_chn[,1:5],5)

## ----echo=FALSE----------------------------------------------------------
data("tmall_milk_sales")
head(tmall_milk_sales[,1:5],5)

## ------------------------------------------------------------------------
machinery_fin_charts_class=column_class(machinery_fin_charts)
machinery_fin_charts_class$numerical
machinery_fin_charts_class$categorical

## ------------------------------------------------------------------------
test_df=macro_data_chn
test_df$PPI[2:4]=NA
test_df$PPI
num2ctg(test_df,col.name = 'PPI',partition = c(0.2,0.5,0.3))
num2ctg(test_df,col.name = 'PPI',partition = c(98,100),
level.name = c('low','medium','high'),type='criteria')
num2ctg(test_df,col.name = 'PPI',partition = 3,
level.name = c('low','medium','high'),type='equal')
num2ctg(test_df,col.id = 4,partition = 3,
level.name = c('low','medium','high'),type='equal')

## ------------------------------------------------------------------------
test_df=tmall_milk_sales
test_df$label[12:15]=NA
ord_ctg2num(test_df,col.name = 'label',permutation = c('P','O','H'))[1:10]
ord_ctg2num(test_df,col.id = 11,permutation = c('P','O','H'))[1:10]
ord_ctg2num(test_df,col.id = 11,permutation = c('P','O','H'),
numeric_levels=c(1,5,10))[1:10]

## ------------------------------------------------------------------------
test_df=tmall_milk_sales
nom_ctg2num(test_df,col.id=c(1,2,3,4,5))[1:5,]
nom_ctg2num(test_df,col.name=c('label','promotion'),drop=c('H','N'))[1:5,]

## ------------------------------------------------------------------------
test_df=tmall_milk_sales
test_df[sample(1:nrow(test_df),200),'promotion'] <- NA
test_df[sample(1:nrow(test_df),50),'feature'] <- NA
test_df[sample(1:nrow(test_df),20),'units'] <- NA
test_df[sample(1:nrow(test_df),5),'unit_price'] <- NA

miss=miss_prep(test_df)
miss$pattern

miss_prep(miss$df,remove.row = TRUE)$pattern

## ----message=FALSE-------------------------------------------------------
test_df=tmall_milk_sales
test_df[sample(1:nrow(test_df),200),'promotion'] <- NA
test_df[sample(1:nrow(test_df),50),'feature'] <- NA
test_df[sample(1:nrow(test_df),20),'units'] <- NA
test_df[sample(1:nrow(test_df),5),'unit_price'] <- NA
test_df[sample(1:nrow(test_df),20),'label'] <- NA

df_imputed1=impute_missing(test_df,ord.col="label")
sapply(df_imputed1$impute,function(x) sum(is.na(x)))

df_imputed2=impute_missing(test_df,ord.col="label",
                           ignore.imputation = "unit_price")
sapply(df_imputed2$impute,function(x) sum(is.na(x)))

## to check whether the imputation make sense
densityplot(df_imputed1$pool,scales=list(x=list(relation='free')))

## ------------------------------------------------------------------------
bar_plot(dataframe=estate_fin_charts,
         ctg.idx = 'Year',num.idx = 'income',
         condition.idx = '证券简称',criteria=2016,top_N=12,
         colors=brewer.pal(12,'Set3'),
         xaxis_name='年份',yaxis_name='营业收入（亿元）',
         title='2016年营业收入前12名房地产企业历年营收变化',
         paper_bgcolor='#ccece6',margin=list(t=36,l=24))

bar_plot(dataframe=macro_data_chn,
         ctg.idx='year',num.idx=c(9:12),
         criteria = 2016,colors = brewer.pal(4,'Set1'),
         xaxis_name = '年份',yaxis_name = '商品价格（元/吨）',
         title='一些大宗商品的历年价格变化',
         paper_bgcolor='#ccece6',margin=list(t=36,l=24))

## ------------------------------------------------------------------------
test_df=machinery_fin_charts%>%
  filter(年份==2017 & 分类=='通用设备')
bubble_plot(dataframe=test_df,ctg.idx=15,num.idx='毛利率',size.idx='主营业务收入',
            color.idx='经济区划分',text.idx=7,colors=brewer.pal(8,'Set1'),
            xaxis_name='营收CAGR5',yaxis_name='毛利率',xaxis_format='%',yaxis_format='%',
            title='通用设备企业实力气泡图（2017）',paper_bgcolor='#ccece6')

test_df=machinery_fin_charts%>%
  filter(年份==2017 & 分类=='电力设备')
bubble_plot(dataframe=test_df,ctg.idx=10,num.idx='毛利率',size.idx='主营业务收入',
            color.idx=15,text.idx=7,
            colors='Reds',xaxis_name='员工总数（人）',yaxis_name='毛利率',
            xaxis_format='',yaxis_format='%',legend_pos_y = 1.02,
            title='电力设备企业实力气泡图（2017）',paper_bgcolor='#ccece6')

## ------------------------------------------------------------------------
corr_check(dataframe=macro_data_chn,eliminate = 'year')

## ------------------------------------------------------------------------
## another example, add a categorical variable,
## see if the function eliminate it automatically
test_df=macro_data_chn
test_df$ctg='cat'
corr_check(dataframe = test_df,eliminate=c(1,2))

## ------------------------------------------------------------------------
distribution_plot(dataframe=tmall_milk_sales,
                  ctg.idx=NA,num.idx='unit_weight',
                  type='histogram',xaxis_name='单位净含量（克）',
                  yaxis_name='产品数量（件）',
                  title='线上销售乳制品单位净含量分布')
distribution_plot(dataframe=tmall_milk_sales,
                  ctg.idx=11,num.idx='unit_weight',
                  type='violin',xaxis_name='销量情况',
                  tick_text=c('热销产品','普通产品','滞销产品'),
                  yaxis_name='单位净含量（克）',
                  title='线上销售乳制品单位净含量按销量分布',
                  paper_bgcolor='#ccece6')
distribution_plot(dataframe=tmall_milk_sales,
                  ctg.idx=2,num.idx='unit_weight',
                  type='box',xaxis_name='销量情况',
                  tick_text=c('酸奶产品','牛奶产品'),
                  yaxis_name='单位净含量（克）',
                  title='线上销售乳制品单位净含量按产品线分布',
                  paper_bgcolor='#ccece6',
                  margin=list(t=36,l=36,b=36,l=10))

## ------------------------------------------------------------------------
donut_plot(dataframe=machinery_fin_charts,
           ctg.idx="分类",num.idx='主营业务收入',
           condition=2017,condition.idx='年份',
           colors=c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072",
                    "#80B1D3" ,"#FDB462", "#B3DE69"),
           title='机械行业营收构成（2017）',
           legendOn=FALSE,paper_bgcolor='#ccece6')

donut_plot(dataframe=dairy_fin_charts,
           ctg.idx="name",num.idx='income',
           condition=2017,condition.idx='Year',colors = "",
           title='乳制品企业市场份额（2017）',legendOn=TRUE,
           paper_bgcolor='#ccece6',margin=list(t=30,b=72))


## ------------------------------------------------------------------------
test_df=dairy_fin_charts%>%
  filter(name=='伊利股份')
double_axis(dataframe=test_df,
            ctg.idx='Year',lines.idx='profit',
            bars.idx=2,lines.mode='lines+markers',
            lines.colors='rgb(128,0,128)',lines.names = '营业利润',
            bars.colors = 'rgba(55,128,192,0.7)',
            bars.names = '营业收入',xaxis_name = '年份',
            line.axis_name = '营业利润（亿元）',
            bar.axis_name = '营业收入（亿元）',
            title='伊利股份营收及利润',annOn=T,
            margin=list(r=40))

double_axis(dataframe=estate_fin_charts,ctg.idx='Year',lines.idx=c(5,6),
            bars.idx=c('asset','liability'),condition='万科A',lines.mode = 'lines',
            lines.colors = c("rgb(128, 0, 128)",'rgb(255,140,0)'),
            lines.width = 4,lines.names=c('总资产收益率','净资产收益率'),
            bars.colors = c('rgba(55,128,192,0.7)','rgba(219, 64, 82,0.7)'),
            bars.names = c('总资产','总负债'),xaxis_name = '年份',
            line.axis_format = '',line.axis_name='百分比',bar.axis_name = '单位：亿元',
            title='财务分析（万科A）',annOn=F,
            legend=list(x=0.45,y=1.03,orientation='h',
                        font=list(size=10),bgcolor="transparent"),
            margin=list(r=54),paper_bgcolor='#ccece6')

## ------------------------------------------------------------------------
double_axis(dataframe=estate_fin_charts,
            ctg.idx='Year',lines.idx=c(5,6),
            bars.idx=c('asset','liability'),
            condition='万科A',lines.mode = 'lines+markers',
            lines.colors = c("rgb(128, 0, 128)",'rgb(255,140,0)'),
            lines.width = 4,lines.names=c('总资产收益率','净资产收益率'),
            bars.colors = c('rgba(55,128,192,0.7)','rgba(219, 64, 82,0.7)'),
            bars.names = c('总资产','总负债'),
            xaxis_name = '年份',line.axis_format = '',
            line.axis_name='百分比',bar.axis_name = '单位：亿元',
            title='财务分析（万科A）',annOn=T,
            legend=list(x=0.45,y=1.03,orientation='h',
                        font=list(size=10),bgcolor="transparent"),
            margin=list(r=54),paper_bgcolor='#ccece6')

## ------------------------------------------------------------------------
facet_bar(dataframe=tmall_milk_sales,
          ctg.idx='label',num.idx=9,
          condition.idx='promotion',label.idx='brand',
          legend_name='是否促销',
          legend_label=c('不促销','促销'),
          colors=c('#A6CEE3','#1F78B4'),xaxis_name='产品类型',
          xaxis_label=c('热销产品','普通产品','滞销产品'),
          yaxis_name='价格（元）',
          title='线上乳制品分类销售情况',
          type='bar',stack=T,
          paper_bgcolor='#ccece6')

facet_bar(dataframe=tmall_milk_sales,
          ctg.idx='class',num.idx=NA,
          condition.idx='label',label.idx='brand',
          legend_name='产品类型',
          legend_label=c('热销产品','普通产品','滞销产品'),
          colors=brewer.pal(3,'Set2'),xaxis_name='分类',
          xaxis_label=c('酸奶','牛奶'),
          yaxis_name='产品数（件）',title='线上乳制品分类销售情况',
          paper_bgcolor='#ccece6')

## ------------------------------------------------------------------------
horizontal_bar(dataframe=tmall_milk_sales,
               h.idx='label',v.idx='pack',
               h_name=c('热销产品','普通产品','滞销产品'),
               v_name=c('爱克林包装','杯装','袋装','盒装','瓶装'),
               colors = brewer.pal(3,'Set1'),
               xaxis_name='百分比',
               title='电商乳制品产品线分类统计')

horizontal_bar(dataframe=tmall_milk_sales,h.idx='feature',v.idx='label',
               h_name=c('儿童牛奶','养生牛奶','新出产品','其他产品',
                        '牛奶饮料','主推产品'),
               v_name=c('热销产品','普通产品','滞销产品'),
               colors = c('#E41A1C','#377EB8','#4DAF4A','#984EA3',
                          '#FF7FF0','#FFD92F'),
               xaxis_name='百分比',
               title='电商乳制品产品线分类统计',
               paper_bgcolor='#ccece6',
               plot_bgcolor='#ccece6')

## ------------------------------------------------------------------------
label_bar_plot(dataframe=estate_fin_charts,
               ctg.idx='Year',num.idx='roa',
               condition.idx = '证券简称',criteria=2016,
               top_N=10,colors='#377EB8',
               xaxis_name = 'ROA',
               title='房地产企业2016年ROA排名前十企业')

label_bar_plot(dataframe=macro_data_chn,
               ctg.idx='year',num.idx=c(9:12),
               criteria=2016,colors=brewer.pal(4,'Set1'),
               xaxis_name = '价格（元/吨）',
               title='大宗商品商品2016年价格',
               paper_bgcolor = '#ccece6')

## ------------------------------------------------------------------------
## cross-sectional data
test_df=estate_fin_charts%>%
  filter(Year==2016)%>%
  select(证券简称,income)
## make plot
test_df$Year=2016
label_bar_plot(dataframe=estate_fin_charts,
               ctg.idx='Year',num.idx='roa',
               condition.idx = '证券简称',
               criteria=2016,top_N=10,
               colors='#377EB8',
               xaxis_name = 'ROA',
               title='房地产企业2016年ROA排名前十企业',
               paper_bgcolor='#ccece6')


## ------------------------------------------------------------------------
lines_plot(dataframe=dairy_fin_charts,
           ctg.idx = 5,num.idx = 2,
           condition=c('伊利股份','蒙牛股份','光明乳业'),
           colors=c("#00526d","#de6e6e","#32ab60"),
           yaxis_name = '营业收入',linewidth = 4,
           title='乳制品企业营业收入图')

lines_plot(dataframe=dairy_fin_charts,
           ctg.idx = 5,num.idx = 2,
           condition=c('伊利股份','蒙牛股份','光明乳业'),
           colors=c("#00526d","#de6e6e","#32ab60"),
           mode='lines+markers',linewidth = 2,
           title='乳制品企业营业收入图',
           yaxis_name='营业收入',
           xaxis=list(showgrid=F,nticks=10,ticklen=4,tickangle=-45,
                     ticks='outside',tickmode="array",
                     type='category',title="年份"),
           yaxis=list(visible=F),
           legend=list(x=0.5,y=0.1,orientation='h',
                      font=list(size=10),bgcolor="transparent"),
           paper_bgcolor='#ccece6',
           margin=list(t=32,l=32,r=32))

## ------------------------------------------------------------------------
lines_split_plot(p=lines_split(dataframe=macro_data_chn,ctg.idx = 'year',
                 num.idx = c(3,5,6,9),
                 colors=c("#00526d","#de6e6e","#32ab60","#ff8000"),
                 title="一些宏观经济指标走势",
                 xaxis=list(showgrid=F,ticklen=4,nticks=3,title="年份"),
                 legend=list(x=0.5,y=1.05,orientation='h',bgcolor='transparent'),
                 paper_bgcolor='#ccece6',
                 margin=list(t=32,l=32,r=32)),
                 yaxis=list(visible=F),
                 yaxis2=list(visible=F),
                 yaxis3=list(visible=F),
                 yaxis4=list(visible=F))

lines_split_plot(p=lines_split(dataframe=macro_data_chn,ctg.idx = 'year',
                 num.idx = c(3,5,6,9),
                 colors=c("#00526d","#de6e6e","#32ab60","#ff8000"),
                 title="一些宏观经济指标走势",
                 xaxis=list(showgrid=F,ticklen=4,nticks=3,title="年份"),
                 legend=list(x=0.5,y=1.05,orientation='h',bgcolor='transparent'),
                 paper_bgcolor='#ccece6',
                 margin=list(t=32,l=32,r=32)))

## ------------------------------------------------------------------------
line_ann_plot(dataframe=dairy_fin_charts,
              ctg.idx='Year',num.idx='profit',
              condition=c('伊利股份','蒙牛股份','光明乳业'),
              colors = c("#00526d","#de6e6e","rgb(50,171,96)"),
              events = c("中国奶制品污染事件&金融危机",
                         "公布和实施经济刺激计划",
                         "“互联网+”：传统行业进入电商时代",
                         "中央一号文件：全面振兴奶业"),
              marker_pos_x = c(2008,2009,2012,2017),
              ann_pos_x = 2006, text_pos_x = 0.1, 
              ann_pos_y = c(78,74,70,66),
              marker_refer = c('光明乳业','蒙牛股份','伊利股份','伊利股份'),
              yaxis_name = '利润总额（亿元）',
              title = '乳制品企业利润变动与行业重要事件')

line_ann_plot(dataframe=dairy_fin_charts,
              ctg.idx='Year',num.idx='profit',
              condition=c('伊利股份','蒙牛股份','光明乳业'),
              colors = c("#00526d","#de6e6e","rgb(50,171,96)"),
              events = c("中国奶制品污染事件&金融危机",
                         "公布和实施经济刺激计划",
                         "“互联网+”：传统行业进入电商时代",
                         "中央一号文件：全面振兴奶业"),
              marker_pos_x = c(2008,2009,2012,2017),
              ann_pos_x = 2006, text_pos_x = 0.1, 
              ann_pos_y = c(78,74,70,66),
              marker_refer = c('光明乳业','蒙牛股份','伊利股份','伊利股份'),
              marker_pos_adj=3,
              yaxis_name = '利润总额（亿元）',
              title = '乳制品企业利润变动与行业重要事件',
              legend = list(x=0.5,y=0.1,orientation='h',
                            font=list(size=10),bgcolor='transparent'),
              xaxis = list(showgrid=T,nticks=12,
                           ticks="outside",title="年份"),
              paper_bgcolor='#ccece6')

## ------------------------------------------------------------------------
## prepare data, it's generate from analysis of NASDAQ: MDLZ
MDLZ=c(6,5,2,6,1,5,1,1,3,1,2,2,10,10,9,9)
others=c(7,9,6,6,3,2,2,2,3,2,4,4,10,10,9,9)
polar.dataframe=rbind(MDLZ,others)
row.names(polar.dataframe)=c('亿滋国际','市场同业竞争者均值')
colnames(polar.dataframe)=c('股息收益','净资产收益率',
                            '资产回报率','息税前利润',
                            '销售增长率','净收入增长率',
                            '营收增长期望','每股盈余期望',
                            '市盈率','市售率',
                            '企业价值倍数','市现率',
                            '总市值','成交量',
                            '波动性','风险系数')

## a simple example
polar_charts(dataframe=polar.dataframe,
             colors=c('#FF7F00','#33A02C'),
             fills=c('toself','none'),
             fillcolors=c('#FDBF6F','#B2DF8A'),
             title='亿滋国际和同行竞争对手股票指标对比',
             margin=list(t=56))

## another way
polar_charts(dataframe=polar.dataframe,
             colors=c('#FF7F00','#33A02C'),
             fills=c('toself','toself'),
             fillcolors=c('rgba(253,191,111,0.3)','rgba(178,223,138,0.3)'),
             title='亿滋国际和同行竞争对手股票指标对比',
             legend=list(x=1,y=0,bgcolor='transparent',
                         font=list(size=14,family='heiti')),
             margin=list(t=56),
             paper_bgcolor='#ccece6')

