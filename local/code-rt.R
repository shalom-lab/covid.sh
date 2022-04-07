library(EpiEstim)
library(tidyverse)
## load data
# observation
SH_obs <- readxl::read_xlsx("SH_Rt_data.xlsx")
SH_obs <- SH_obs %>%
  mutate(
    date = as.Date(date),
    daily_all_infec = symptomatic + asymptomatic,
  )
SH_positive_obs <- SH_obs$daily_all_infec

t_start <- seq(5, length(SH_positive_obs) - 0)
t_end <- t_start + 0

Rt_obs <- estimate_R(SH_positive_obs,
                     method="parametric_si",
                     config = make_config(list(
                       mean_si = 4.5,#7.5
                       std_si = 2,#3.4
                       t_start = t_start,
                       t_end = t_end)))
results_obs <- data.frame(date=SH_obs$date[Rt_obs$R$t_end],
                          meanR=c(Rt_obs$R$`Mean(R)`),
                          lbd=c(Rt_obs$R$`Quantile.0.025(R)`),
                          ubd=c(Rt_obs$R$`Quantile.0.975(R)`))

ggplot(data = results_obs,aes(x=date,y=meanR))+
  geom_ribbon(aes(ymin=lbd,ymax=ubd),fill= "#AD002AFF",alpha=0.2)+
  geom_line(size=1,colour= "#AD002AFF")+
  geom_hline(yintercept = 1,size=1,lty=2)+
  scale_x_date(date_breaks = "2 days",date_labels = "%m/%d",expand = c(0,0))+
  labs(x="Date (2022)",y="Estimated Rt")+
  theme(legend.title = element_blank(),legend.position = c(0.85,0.95),
        legend.background = element_blank(),legend.key.size = unit(15,"pt"),
        legend.key = element_blank(),legend.text=element_text(size=15,hjust = 0),
        axis.text = element_text(size = 20,colour = "black"),
        axis.title = element_text(size=20,color = "black"),
        axis.line = element_line(colour = "black",size = 1),
        axis.ticks = element_line(color = "black",size = 1),
        panel.background = element_blank(),panel.grid = element_line(colour = "grey"),
        panel.border = element_rect(fill = NA,size = 1,colour = "black"),
        plot.margin=unit(rep(2,4),'lines'),
        strip.background = element_blank(),
        strip.text = element_text(size=20,colour = "black")
  )
ggsave(filename = "SH_Rt.png",width = 16,height = 9,dpi = 300)
