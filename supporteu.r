## written by Chia Liu, March 2, 2022 ## 

library(essurvey)
set_email("liu@demogr.mpg.de")

dat <- import_rounds(6, format = 'spss') %>% filter(cntry %in% c('UA','PL','RU','HU')) %>% 
  select(cntry, yrbrn, gndr, eduyrs, implvdm, dmcntov, euftf,stfgov)

dat2<-dat %>% zap_labels() %>% 
  mutate_all(~replace(., is.na(.),99)) %>%
  mutate(cntry=ifelse(cntry=='HU','Hungary',
                      ifelse(cntry=='PL','Poland',
                             ifelse(cntry=='RU','Russia','Ukraine'))),
         yrbrn=case_when(
           yrbrn %in% c(1911:1950)~'1950',
           yrbrn %in% c(1951:1965)~'1965',
           yrbrn %in% c(1966:1980)~'1980',
           yrbrn %in% c(1981:1998)~'1998',
         TRUE~'NA'
         ),
         gndr=ifelse(gndr==1,'male','female'))

pal<-RColorBrewer::brewer.pal(11,'RdBu')
pal2<-append(pal,c('yellow'),after=length(pal)) ## use a different scheme for missing 

## create distribution plot for 4 dimensions on government/EU ## 

tiff(file="rplot.jpg",
     width=8, height=8, units="in", res=500)
par(mfrow=c(2,2)) 
table(dat2$cntry,dat2$euftf) %>% prop.table(1) %>% plot(col=pal2, main='EU gone too far or not far enough?',
                                                                    xlab='0=already gone too far; 10=should go farther, 99=missing')
table(dat2$cntry,dat2$dmcntov) %>% prop.table(1) %>% plot(col=pal2,main='How democratic is your country?',
                                                                      xlab='0=not at all; 10=completely, 99=missing')
table(dat2$cntry,dat2$implvdm) %>% prop.table(1) %>% plot(col=pal2,main='How important is it to live in a democracy?',
                                                                       xlab='0=not at all; 10=extremely important, 99=missing')
table(dat2$cntry,dat2$stfgov) %>% prop.table(1) %>% plot(col=pal2,main='Satisfaction with government',
                                                                       xlab='0=extremely dissatisfied; 10=extremely satisfied, 99=missing')
dev.off()

## regressions ## 
dat3<-dat2 %>% filter(euftf!=99 & yrbrn!='NA' & eduyrs!=99) ## exclude missing ## 

m1<-lm(euftf~cntry,data=dat3) 
m2<-lm(euftf~yrbrn+gndr+cntry,data=dat3) 
m3<-lm(euftf~yrbrn+gndr+cntry+eduyrs,data=dat3) 

library(dotwhisker)
dwplot(list(m1,m2,m3),
       dot_args = list(size = 2.2),
       whisker_args = list(size = 0.7),
       model_order = c("Model 1", "Model 2", "Model 3"),
  vline = geom_vline(
    xintercept = 0,
    colour = "grey60",
    linetype = 2
  )) %>% 
  relabel_predictors(
    c(
      cntryPoland = "Poland (ref:Hungary)",
      cntryRussia = "Russia",
      cntryUkraine = "Ukraine",
      gndrmale = "Male (ref:Female)",
      yrbrn1965='Cohort 1951-1965 (ref:<=1950)',
      yrbrn1980='Cohort 1966-1980',
      yrbrn1998='Cohort 1981-1998',
      eduyrs = "Yrs of education (ref:0)"
    ))+
  ggtitle("Support for EU (Hungary as reference)") +
  theme_pubr()
