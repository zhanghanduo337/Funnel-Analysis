setwd('/Users/zhanghanduo/Desktop/data/Funnel')
rm(list = ls())
library(dplyr)
library(ggplot2)
library(PRROC)
library(tidyverse)
library(plotly)
library(gtable)

################################################################### import data

df_home_page = read.csv('home_page_table.csv')
df_payment_confirmation = read.csv('payment_confirmation_table.csv')
df_payment_page = read.csv('payment_page_table.csv')
df_search_page = read.csv('search_page_table.csv')
df_user = read.csv('user_table.csv')

################################################################## merge tables
names(df_payment_confirmation)[2] = 'confirm'
names(df_home_page)[2] = 'home'
names(df_search_page)[2] = 'search'
names(df_payment_page)[2] = 'pay'
df_temp = merge(df_user,df_home_page,all.x = T,by.x = 'user_id', by.y = 'user_id')
df_temp = merge(df_temp,df_search_page,all.x = T,by.x = 'user_id', by.y = 'user_id')
df_temp = merge(df_temp,df_payment_page,all.x = T,by.x = 'user_id', by.y = 'user_id')
df_temp = merge(df_temp,df_payment_confirmation,all.x = T,by.x = 'user_id', by.y = 'user_id')
df = df_temp
head(df)
attach(df)
rm(df_temp)

####################################################################### EDA

summary(df)

(dim(df)[1]-sum(is.na(df$confirm)))/dim(df)[1] #overall conversion rate: 0.5%

conv_device = df %>% 
  group_by(device) %>%
  summarise(conv_rate= sum(is.na(confirm) == F)/n(),tot_num = n()) %>%
  arrange(desc(conv_rate))
conv_device

conv_device_sex = df %>% 
  group_by(device,sex) %>%
  summarise(conv_rate= sum(is.na(confirm) == F)/n(),tot_num = n()) %>%
  arrange(desc(conv_rate))
conv_device_sex 
#Females using mobile have an rather high conversion rate, over 1%

conv_home_page = df %>% 
  group_by(device) %>%
  summarise(home_page= sum(is.na(home) == F))
conv_search_page = df %>% 
  group_by(device) %>%
  summarise(search_page= sum(is.na(search) == F))
conv_payment = df %>% 
  group_by(device) %>%
  summarise(payment= sum(is.na(pay) == F))
conv_confirm = df %>% 
  group_by(device) %>%
  summarise(confirm= sum(is.na(confirm) == F))
conv_table = merge(conv_home_page,conv_search_page,by = 'device')
conv_table = merge(conv_table,conv_payment,by = 'device')
conv_table = merge(conv_table,conv_confirm,by = 'device')
conv_table = as.data.frame(t(conv_table)[-1,])
colnames(conv_table) = c( "Desktop","Mobile")
conv_table$Desktop = as.numeric(as.character(conv_table$Desktop))
conv_table$Mobile = as.numeric(as.character(conv_table$Mobile))
###########################################################Overall Funnel Plot

fig <- plot_ly() 
fig <- fig %>%
  add_trace(type = "funnel",
            y = c("home_page", "search_page", "payment", "confirm"),
            x = c(sum(conv_table[1,]),sum(conv_table[2,]),
                  sum(conv_table[3,]), sum(conv_table[4,])),
            textposition = "inside",
            textinfo = "value+percent initial",
            opacity = 0.65,
            marker = list(color = c("deepskyblue", "lightsalmon", "tan", "teal"),
                          line = list(width = rep(2,5))),
            connector = list(line = list(color = "royalblue", dash = "dot", width = 3))) 
fig <- fig %>%
  layout(title = list(text = 'Overall Funnel Plot',position = 'top right'),
    yaxis = list(categoryarray = c("home_page", "search_page", "payment", "confirm")),
    annotations = list(text = "302,1%",  x = 8000, y = 3,showarrow=FALSE ))

fig
 
############################################################back to back ggplot

# prepare data for plotting

conv_table$Mobile = -conv_table$Mobile
conv_table$page = rownames(conv_table)
temp = conv_table[,c(2,3)]
temp1 = conv_table[,c(1,3)]
names(temp)[1]='total'
names(temp1)[1]='total'
plotting_df = as.data.frame(rbind(temp,temp1))
plotting_df$device = if_else(plotting_df$total>0,'Desktop','Mobile')
temp_df <-
  plotting_df %>% 
  filter(device == "Desktop") %>% 
  arrange(total)
plotting_df$log_total = log(abs(plotting_df$total))
plotting_df$log_total[1:4] = -plotting_df$log_total[1:4]
the_order <- temp_df$page

# use total to plot to emphasis conversion rate 

p <- 
  plotting_df %>% 
  ggplot(aes(x = page, y = total, group = device, fill = device)) +
  geom_bar(stat = "identity", width = 0.85) +
  coord_flip() +
  scale_x_discrete(limits = the_order)+
  scale_y_continuous(breaks = seq(-60000,60000,10000), 
                     labels = abs(seq(-60000,60000,20000))) +
  labs(x = "Page", y = "Count", title = "Funnel chart(total)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values=c("dark red", "dark blue"),
                    name="",
                    breaks=c("Desktop", "Mobile"),
                    labels=c("Desktop", "Mobile")) 

print(p)

# log_total plot to emphasis mobile is doing better than desktop on conversion rate
p1 <- 
  plotting_df %>% 
  ggplot(aes(x = page, y = log_total, group = device, fill = device)) +
  geom_bar(stat = "identity", width = 0.85) +
  coord_flip() +
  scale_x_discrete(limits = the_order)+
  scale_y_continuous(breaks = seq(-12, 12, 2), 
                     labels = abs(seq(-12, 12, 2))) +
  labs(x = "Page", y = "Count", title = "Funnel chart(log_total)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
guides(fill = guide_legend(reverse = TRUE)) +
 scale_fill_manual(values=c("dark red", "dark blue"),
                    name="",
                    breaks=c("Desktop", "Mobile"),
                    labels=c("Desktop", "Mobile")) 

print(p1)

#2plots show in one
#multiplot from R cookbook
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
# multiplot function
multiplot(p,p1,cols = 2)

############################################################Funnel Chart on sex

conv_device_sex = df%>%
  group_by(sex,device)%>%
  summarise(home_page= sum(is.na(home)== F),
             search_page= sum(is.na(search)== F),
             payment= sum(is.na(pay)== F),
             confirm= sum(is.na(confirm)== F))
conv_device_sex = as.data.frame(conv_device_sex)

##Conclusion: big potential market on female using mobile device, rather strong conversion rate.
#product team should be working on the reason behind low conversion rate of desktop device and male
#customers, either from advertisement standpoint, or merchandise standpoint. We might also 
#consider conducting careful A/B testing to validate the conclusion or stratagy. 

