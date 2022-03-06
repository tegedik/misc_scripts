library(tidyverse)
library(ggplot2)
library(hrbrthemes)

df_1897 <- tribble(
  ~age_grp, ~male,  ~female,
  "0-5", 883075, 791944,
  "6-10", 1001294, 924175,
  "11-15", 980320, 865899,
  "16-20", 905378, 797600,
  "21-25", 912623, 765947,
  "26-30", 873564, 755820,
  "31-35", 787972, 723318,
  "36-40", 698046, 640341,
  "41-45", 664305, 608797,
  "46-50", 577018, 482769,
  "51-55", 470458, 423118,
  "56-60", 360242, 296273,
  "61-65", 307332, 269863,
  "66-70", 230951, 196710,
  "71-75", 178526, 160549,
  "76-80", 120861, 104445,
  "81-85", 75335, 66240,
  "86-90", 43449, 36149,
  "91+", 33264, 36338,
  "total", 10104013, 8946295
)

df_1897 <- df_1897 %>% 
  mutate(tot = male + female,
         age_grp = factor(age_grp, levels=age_grp))

df_plot <- df_1897 %>% 
  slice(-20) %>% 
  pivot_longer(!c(age_grp,tot), names_to = "gndr", values_to = "count")

df_plot$count <- ifelse(df_plot$gndr == "male", -1*df_plot$count, df_plot$count)

ggplot(df_plot, aes(x = age_grp, y = count, fill = gndr)) + 
  geom_bar(data = subset(df_plot, gndr == "male"), stat = "identity", fill="#407fbf") + #TUIK colors
  geom_bar(data = subset(df_plot, gndr == "female"), stat = "identity", fill="#a02921") + #TUIK colors
  scale_y_continuous(labels = c("1m", "0.5m","0","0.5m","1m")) +
  coord_flip() +
  labs(x="", y="", title="Population pyramid of Ottoman Empire in 1897", 
       subtitle = "39 administrative units were covered (app. 60% of the population)",
       caption="Gürkan, T. (2017). Resmi İstatistiklere Göre Osmanlı Toplum ve Ekonomisi. Türkiye İş Bankası Kültür Yayınları.\n @tahirenesgedik") +
  annotate("text", x = 19, y = 500000, label = "Female", family="Arial Narrow", size=7) +
  annotate("text", x = 19, y = -500000, label = "Male", family="Arial Narrow", size=7) +
  theme_ipsum()


ggsave("pop_pyr.png", dpi=300, width = 25, height = 20, units = "cm", bg = "white")