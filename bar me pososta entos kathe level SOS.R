# diagrammata
library(ggthemes)
library(RColorBrewer)
library(corrplot)
library(rstatix)


display.brewer.all()
str(bank2)
bank2%>%ggplot(aes(x=age))+geom_density()+facet_grid(.~y) #density of age by yes and no
bank2%>%ggplot(aes(x=age,color=y))+geom_density()
bank2%>%ggplot(aes(x=duration,color=y))+geom_density() #density duration by yes and no

bank2%>%ggplot(aes(job,fill=y))+geom_bar(stat = "count",position = "dodge")+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))
bp<-data.frame(prop.table(table(bank2$job,bank2$y))) #each line percentage from the whole set
names(bp)<-c("job","y","prop")
bp%>%ggplot(aes(x=job,y=prop,fill=y))+geom_bar(stat = "identity",position = "dodge")+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))

bp1<-data.frame(prop.table(table(bank2$job,bank2$y),1)) #se kathe grammi deixnei to pososto yes kai no entos toy ekastote level tou job
names(bp1)<-c("job","y","prop")
bp1%>%ggplot(aes(x=job,y=prop,fill=y))+geom_bar(stat = "identity",position = "stack")+theme_solarized(light=FALSE)+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))+scale_fill_brewer(palette = "BrBG")

bp2<-data.frame(prop.table(table(bank2$month,bank2$y),1))#se kathe grammi deixnei to pososto yes kai no entos toy ekastote level tou month
names(bp2)<-c("month","y","prop")
bp2%>%ggplot(aes(x=month,y=prop,fill=y))+geom_bar(stat = "identity",position = "stack")+theme_solarized(light=FALSE)+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))+scale_fill_brewer(palette = "BrBG")

#bp4 y by day 
bp4<-data.frame(prop.table(table(bank2$day_of_week,bank2$y),1))
names(bp4)<-c("day_of_week","y","prop")
bp4%>%ggplot(aes(x=day_of_week,y=prop,fill=y))+geom_bar(stat = "identity",position = "stack")+theme_solarized(light=FALSE)+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))+scale_fill_brewer(palette = "BrBG")

#bp5 default by y
bp5<-data.frame(prop.table(table(bank2$default,bank2$y),1))
names(bp5)<-c("default","y","prop")  
bp5%>%ggplot(aes(x=default,y=prop,fill=y))+geom_bar(stat = "identity",position = "stack")+theme_solarized(light=FALSE)+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))+scale_fill_brewer(palette = "BrBG")

#more visualizations
bank2%>%ggplot(aes(x=cons.price.idx,color=y))+geom_density()
bank2%>%ggplot(aes(x=cons.conf.idx,color=y))+geom_density()
bank2%>%ggplot(aes(y=cons.price.idx))+geom_boxplot(aes(xmin=-25,xmax=25))
bank2%>%ggplot(aes(y=cons.conf.idx))+geom_boxplot(aes(xmin=-25,xmax=25))

bp3<-data.frame(prop.table(table(bank2$poutcome,bank2$y)))# previous outcome by yes no
names(bp3)<-c("poutcome","y","prop")
bp3%>%ggplot(aes(x=poutcome,y=prop,fill=y))+geom_bar(stat = "identity", position="stack")+theme_solarized(light=FALSE)+theme(axis.text.x=element_text(angle = 45,hjust = 1,vjust = 1))+scale_fill_brewer(palette = "BrBG")

# y vs euribor3m
bank2%>%ggplot(aes(x=euribor3m,color=y))+geom_density()
bank2%>%ggplot(aes(x=cons.price.idx,y=cons.conf.idx,color=y,size=age))+geom_point(position = "jitter")+scale_color_hue(l=40,c=20)
?geom_point

#duration by y !!!!
bank2 %>% ggplot(aes(x=y,y=duration,colour=duration>mean(duration)))+geom_point()+geom_hline(yintercept=mean(bank2$duration),lty=5)+annotate(geom="text",y=mean(bank2$duration,na.rm = T)+100,x=2.5,label="AVG",fontface=2)+labs(title = "Duration VS Y",subtitle = "Thomas Goudelas" ,x = "Loan", color = "Duration bigger than mean duration",caption = "Bank statistic") +
scale_color_manual(labels = c("LESSER THAN AVG", "GREATER THAN AVG"), values = c("blue", "red"))
?annotate

#correlations
cor1<-cor(bank2[,c("duration","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])
corrplot(cor1) # correlations heatmap me orisma to cor
cor_pmat(bank2[,c("duration","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])
cor_plot(cor_mat(bank2[,c("duration","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])) #better correlations heatmap with insignificant 


