#'@name sample_size_plot
#'@title Produces a plot for the sample size
#'@param sample sample
#'@param population population
#'@param sample.mean mean
#'@param sample.sd standard deviation
#'@param alpha pvalue of t distribution
#'@export
sample_size_plot<-function(sample=NULL,population, sample.mean,sample.sd,alpha=0.1){
  
  data<-do.call("rbind",lapply(2:population,function(i){
    
    freedom = i - 1
    t.score = qt(p=alpha/2, df=freedom,lower.tail=F)
    
    relative.error<-(t.score * sample.sd/(sqrt(i)*sample.mean))*100
    
    data.frame(x=i,y=relative.error)
    
    }))
  
  plot<-ggplot(data=data, aes(x=x, y=y, group=1)) +
    geom_line()
  
  if(!is.null(sample)){
    if(sample<=population){
      
      data$target<-ifelse(data$x==sample,"sample","")
      
      sub<-subset(data,target=="sample")
      
      plot<-plot+
        geom_point(aes(x=sub$x, y=sub$y, group=1, color="red"),show.legend=FALSE)+
        geom_segment(aes(x = sub$x, xend = sub$x, y = min(data$y), yend = sub$y),linetype = 2,color="red")+
        geom_segment(aes(x = min(data$x), xend = sub$x, y = sub$y, yend = sub$y),linetype = 2,color="red")+
        geom_segment(aes(x = -Inf, xend = +Inf, y = 10, yend = 10),linetype = 1,color="red")+
        geom_text(aes(x=min(data$x),y=sub$y,label = round(sub$y,2), vjust = -1), size=3)+
        geom_text(aes(x=sub$x,y=min(data$y),label = round(sub$x,2), vjust = 0,hjust=-1), size=3)+
        labs(x="Sample size",y="Accuracy")
      
    }
  }else{
    
    nearest<-min(subset(data,y>=10)$y)
    data$target<-ifelse(data$y==nearest,"sample","")
    
    sub<-subset(data,target=="sample")
    
    plot<-plot+
      geom_point(aes(x=sub$x, y=sub$y, group=1, color="red"),show.legend=FALSE)+
      geom_segment(aes(x = sub$x, xend = sub$x, y = min(data$y), yend = sub$y),linetype = 2,color="red")+
      geom_segment(aes(x = min(data$x), xend = sub$x, y = sub$y, yend = sub$y),linetype = 2,color="red")+
      geom_segment(aes(x = -Inf, xend = +Inf, y = 10, yend = 10),linetype = 1,color="red")+
      geom_text(aes(x=min(data$x),y=sub$y,label = round(sub$y,2), vjust = -1), size=3)+
      geom_text(aes(x=sub$x,y=min(data$y),label = round(sub$x,2), vjust = 0,hjust=-1), size=3)+
      labs(x="Sample size",y="Relative error")
    
  }
  
  return(plot)
  
}

