# A function to nicely plot a TukeyHSD objext
plot_TukeyHSD <- function(tuk, back=c("return","plot")){
  
  library(tidyverse)
  
  for(i in 1:length(tuk)){
    tmp <- 
      tuk[[i]]%>%
      as_tibble(rownames = "comparison",.name_repair = "universal")%>%
      mutate(signf=p.adj<0.05)%>%
      mutate(colors=if_else(signf,"red","black"))
    
    tuk.plot <- 
      tmp%>%
      ggplot(aes(x=comparison, y=diff))+
      geom_pointrange(aes(ymin=lwr, ymax=upr, color=signf))+
      geom_hline(yintercept = 0)+
      theme_cowplot()+
      coord_flip()+
      scale_color_manual(values=c("black","red"))+
      theme(legend.position = "none",
            axis.text.y = element_text(color = tmp$colors[order(tmp$comparison)]))
    
    if (back=="plot") print(tuk.plot)
    if (back=="return") return(tuk.plot)
  }
}