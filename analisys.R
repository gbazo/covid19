library(tidyverse)
library(reshape2)
library(DT)
library(sqldf)
library(prophet)
library(timelineS)

# import data
data <- read_csv("/home/gabriel/Documentos/corona-virus-brazil/brazil_covid19.csv")

data2 <- read_csv("/home/gabriel/Documentos/corona-virus-brazil/brazil_sice.csv")

data3 <- read_csv("/home/gabriel/Documentos/corona-virus-brazil/covid_new_c.csv")

# resumed table
casos2 <- data %>% 
  group_by(state) %>% 
  summarise(soma_suspeitos = sum(`suspects`),
            soma_negativos = sum(`refuses`),
            soma_positivos = sum(`cases`))

casos3 <- data %>% 
  group_by(date) %>% 
  summarise(soma_suspeitos = sum(`suspects`),
            soma_negativos = sum(`refuses`),
            soma_positivos = sum(`cases`))


data2 <- data2[data2$pos > 0, ]

########################### forecast model
  
casos_predict <- data.frame(data2$Data, data2$susp)

colnames(casos_predict) <- c("ds", "y")

m <- prophet(casos_predict, n.changepoints = 2, changepoint.range = 0.9, daily.seasonality = 'auto')

future <- make_future_dataframe(m, periods = 30)
forecast <- predict(m, future)

plot(m, forecast) + add_changepoints_to_plot(m)

prophet_plot_components(m, forecast)

###################################################################

casos_est <- data %>% 
  group_by(Data, Estado) %>% 
  summarise(soma_suspeitos = sum(`Casos Suspeitos`),
            soma_negativos = sum(`Casos Negativos`),
            soma_positivos = sum(`Casos Positivos`))

write.csv(casos_est, file = "/home/gabriel/Documentos/corona-virus-brazil/brazil_estados.csv")

###############################################################

plot(casos2$Data,casos2$soma_positivos, type = 'l')

# crate a new dataframe with information of only last date reported
last_data <- data.frame(data[data$date == '2020-03-16',])

# sum of covid19 cases - expected result: 234 cases
sum(last_data$Casos.Suspeitos)

# split data variable
data_split <- data %>%
  separate(Data, c("Y","M","D"),sep = "-")

###################################

estados <- last_data$state
data <- data.frame(data)

susp <- data.frame(data$date, data$state, data$cases)

input <- 0
for (i in estados){
  input[i] <- data.frame(susp[susp$data.state == i, ])
}

View(input$`São Paulo`)

############################################# plots

# plot of 3 values for brazil
ggplot(data2, aes(x=Data)) + 
  geom_line(aes(y = susp), color = "darkred") + 
  geom_line(aes(y = neg), color="steelblue", linetype="twodash") +
  geom_line(aes(y = pos), color = "green", linetype="longdash") + scale_y_continuous(breaks = seq(0, 2200, 200)) +
  scale_x_date(date_breaks = "2 day")

# plot of cases, new + incremental
ggplot(data3, aes(x=Data)) + 
  geom_line(aes(y = pos), color = "darkred") + 
  geom_line(aes(y = day), color="steelblue", linetype="twodash") +
  scale_y_continuous(breaks = seq(0, 240, 20)) +
  scale_x_date(date_breaks = "2 day")

#################################################### timeline

dt_tm <- data.frame(data3$day, data3$Data)

dt_dm <- dt_tm[dt_tm$data3.day > 0, ]

dt_tm_split <- dt_dm %>%
  separate(data3.Data, c("Y","M","D"),sep = "-")

timelineS(dt_dm, labels = dt_dm$data3.day, label.length = c(0.2, 0.2, 0.2, 0.3, 0.4, 0.4, 0.4, 0.5, 0.55, 0.7, 0.6, 0.65, 0.95, 0.8), 
          scale = "day", scale.format = "%D", scale.font = 1, label.direction = "up", label.cex = 1.2, label.position = 3, buffer.days = 1, main = "New Cases of Covid-19 Brazil (234 Cases)")



