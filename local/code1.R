####动力学模型在R语言中的实现####
####1 在R语言中构建动力学模型####
# if (!requireNamespace("deSolve", quietly = TRUE)) install.packages("deSolve")
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
# if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
# if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(deSolve)
library(ggplot2)
library(tidyverse)
library(reshape2)

####SIR####
####1.1 构建微分方程函数####
SIR <- function(time, initial, parameters) {
  with(as.list(c(initial, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

###1.2 定义时间点####
time_SIR <- seq(0,100,1) #设定为100天

###1.3 定义参数####
parameters_SIR <- c(
  beta  = 0.0002, # 感染概率(人/天)
  gamma = 0.05    # 治愈率 (/天)
)

###1.4 定义初始值####
initial_SIR <- c(
  S = 999,  # 时刻0的易感者数
  I =   1,  # 时刻0的感染者数
  R =   0   # 时刻0的愈合（免疫）数
)

###1.5 解微分方程####
sir_result <- ode(
  y=initial_SIR,
  times = time_SIR,
  func = SIR,
  parms = parameters_SIR
)

###1.6 可视化####
# ggplot可视化
sir_result1 <- data.frame(sir_result)%>%
  melt(id.vars = "time",
       variable.name = "group")

ggplot()+
  geom_line(data = sir_result1,aes(x=time,y=value,colour=group,group=group))+
  labs(x="Time (days)",y="Number of people")+
  scale_color_discrete(labels = c("Susceptiblea","Infectious","Recovered"))+
  theme_classic()

# 基础绘图可视化
sir_result1 <- data.frame(sir_result)
with(sir_result1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

########################################
####SEIR####
SEIR <- function(time, initial, parameters) {
  with(as.list(c(initial,parameters)),{
    dS <- -beta * I * S
    dE <- beta * I * S - alpha * E
    dI <- alpha * E - gamma * I
    dR <- gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}

time_SEIR <- seq(1,100,1)

initial_SEIR <- c(S = 990, E = 5, I = 1, R = 4)

parameters_SEIR <- c(beta = 0.0005, alpha = 1/4, gamma = 1/20)

SEIR_result <- ode(y = initial_SEIR, times = time_SEIR, func = SEIR, parms = parameters_SEIR)

SEIR_result1 <- data.frame(SEIR_result)%>%
  melt(id.vars = "time",variable.name = "group")

ggplot()+
  geom_line(data = SEIR_result1,aes(x=time,y=value,colour=group,group=group))+
  labs(x="Time (days)",y="Number of people")+
  theme_classic()

# 现存病例数、累计病例数、新增病例数
SEIR2 <- function(time, initial, parameters) {
  with(as.list(c(initial,parameters)),{
    dS <- -beta * I * S
    dE <- beta * I * S - alpha * E
    dI <- alpha * E - gamma * I   # 现存病例数
    dIc <- alpha * E   #累计病例数
    dR <- gamma * I
    return(list(c(dS, dE, dI, dIc, dR)))
  })
}
initial_SEIR2 <- c(S = 990, E = 5, I = 1, Ic = 1, R = 4)
SEIR2_result <- ode(y = initial_SEIR2,times = time_SEIR,func = SEIR2,parms = parameters_SEIR)

SEIR2_result1 <- data.frame(SEIR2_result)%>%
  mutate(Inew = c(1,diff(Ic)))%>%
  select(time,I,Ic,Inew)%>%
  melt(id.vars = "time",variable.name = "group")

ggplot()+
  geom_line(data = SEIR2_result1,aes(x=time,y=value,colour=group,group=group))+
  labs(x="Time (days)",y="Number of people")+
  scale_color_discrete(labels=c("现存病例","累计病例","每日新增病例"))+
  theme_classic()
