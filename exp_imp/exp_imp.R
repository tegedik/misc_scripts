library(tidyverse)
library(ggplot2)
library(hrbrthemes)

df %>% 
  ggplot(aes(x=year, y=ster_mil, group=type,  linetype=type)) + 
  geom_line() +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_point() +
  scale_x_date(breaks = seq.Date(from = min(as.Date("1830-04-08")), to = max(as.Date("1915-04-08")), by = "5 years"), date_labels="%Y",) +
  theme_ipsum() +
  labs(x="", y="Sterling in millions", title="Trade in Ottoman Empire", 
       subtitle = "Şevket Pamuk's Reconstruction (1995)",
       caption="Pamuk, Ş. (1995). Ottoman Foreign Trade in the 19th Century, Historical Statistics Series Vol. I, State Institute of Statistics, Prime Ministry. \n @tahirenesgedik") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("imp_exp.png", dpi=300, width = 30, height = 20, units = "cm", bg = "white")


df %>% 
  ggplot(aes(x=year, y=ster_mil, group=type,  linetype=type)) + 
  geom_line() +
  scale_linetype_manual(values=c("solid", "dashed")) +
  geom_point() +
  scale_x_date(breaks = seq.Date(from = min(as.Date("1830-04-08")), to = max(as.Date("1915-04-08")), by = "5 years"), date_labels="%Y",) +
  scale_y_continuous(trans='log', breaks=c(8,12,16,24,40)) +
  theme_ipsum() +
  labs(x="", y="Sterling in millions (log scale)", title="Trade in Ottoman Empire", 
       subtitle = "Şevket Pamuk's Reconstruction (1995)",
       caption="Pamuk, Ş. (1995). Ottoman Foreign Trade in the 19th Century, Historical Statistics Series Vol. I, State Institute of Statistics, Prime Ministry. \n @tahirenesgedik") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("imp_exp_log.png", dpi=300, width = 30, height = 20, units = "cm", bg = "white")
