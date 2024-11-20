#'@name accuracy_plot
#'@title Produces a plot for the accuracy
#'@param sample sample
#'@param population population
#'@param method method. Default is 'higher'
#'@export
accuracy_plot<-function(sample=NULL,population,method="higher"){

data<-do.call("rbind",lapply(1:population,function(i){data.frame(x=i,y=artfish_accuracy(n=i,N=population,method=method))}))

plot<-ggplot(data=data, aes(x=x, y=y, group=1)) +
  geom_line()

if(!is.null(sample)){
  if(sample<=population){
  data$target<-ifelse(data$x==sample,"sample","")
  
  target<-subset(data,target=="sample")
  
 
  plot<-plot+
    geom_point(aes(x=target$x, y=target$y, group=1, color="red"),show.legend=FALSE)+
    geom_segment(aes(x = target$x, xend = target$x, y = min(data$y), yend = target$y),linetype = 2,color="red")+
    geom_segment(aes(x = min(data$x), xend = target$x, y = target$y, yend = target$y),linetype = 2,color="red")+
    geom_segment(aes(x = -Inf, xend = +Inf, y = 0.9, yend = 0.9),linetype = 1,color="red")+
    geom_text(aes(x=min(data$x),y=target$y,label = round(target$y,2), vjust = -1), size=3)+
    geom_text(aes(x=target$x,y=min(data$y),label = round(target$x,2), vjust = 0,hjust=-1), size=3)+
    labs(x="Sample size",y="Accuracy")
  }
}else{
  nearest<-min(subset(data,y>=0.9)$y)
  data$target<-ifelse(data$y==nearest,"sample","")
  
  target<-subset(data,target=="sample")
  
  plot<-plot+
    geom_point(aes(x=target$x, y=target$y, group=1, color="red"),show.legend=FALSE)+
    geom_segment(aes(x = target$x, xend = target$x, y = min(data$y), yend = target$y),linetype = 2,color="red")+
    geom_segment(aes(x = min(data$x), xend = target$x, y = target$y, yend = target$y),linetype = 2,color="red")+
    geom_segment(aes(x = -Inf, xend = +Inf, y = 0.9, yend = 0.9),linetype = 1,color="red")+
    geom_text(aes(x=min(data$x),y=target$y,label = round(target$y,2), vjust = -1), size=3)+
    geom_text(aes(x=target$x,y=min(data$y),label = round(target$x,2), vjust = 0,hjust=-1), size=3)+
    labs(x="Sample size",y="Accuracy")
}
  
  return(plot)

}

