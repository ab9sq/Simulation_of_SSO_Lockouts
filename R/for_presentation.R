library(tidyverse)
source("./R/DC_LO_Sim.R")


sites <- 2:8
run <- 1000

for (index in sites) {
     lockOut <- sim(run = run,
                    sites = index)
     if(index == 2){
          results <- data.frame(center = index,
                                average = mean(lockOut),
                                StandDev = sd(lockOut),
                                median = median(lockOut),
                                max = max(lockOut) ,
                                min = min(lockOut),
                                TMax = (index * 9) + 1,
                                stringsAsFactors = FALSE)
     } else {
          temp <- data.frame(center = index,
                             average = mean(lockOut),
                             StandDev = sd(lockOut),
                             median = median(lockOut),
                             max = max(lockOut) ,
                             min = min(lockOut),
                             TMax = (index * 9) + 1,
                             stringsAsFactors = FALSE)
          results <- rbind(results, temp)
     }
}

ggplot(results, aes(x=center,
                    y=average)) +
     geom_point(shape = 3)+
     geom_line(aes(y=TMax),
               color = "red") +
     xlab("Data Centers") +
     ylab("Number of Failed Attempts to Lockout") +
     #theme_bw() +
     geom_smooth(method = lm,
                 se = TRUE,
                 level = 0.99,
                 size = 0.25,
                 color = "blue") +
     geom_hline(yintercept = 10,
                color = "green") +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"))

ggsave("./images/results.jpg")

sites <- 2:47
run <- 1000

for (index in sites) {
     lockOut <- sim(run = run,
                    sites = index)
     if(index == 2){
          results_47 <- data.frame(center = index,
                                average = mean(lockOut),
                                StandDev = sd(lockOut),
                                median = median(lockOut),
                                max = max(lockOut) ,
                                min = min(lockOut),
                                TMax = (index * 9) + 1,
                                stringsAsFactors = FALSE)
     } else {
          temp <- data.frame(center = index,
                             average = mean(lockOut),
                             StandDev = sd(lockOut),
                             median = median(lockOut),
                             max = max(lockOut) ,
                             min = min(lockOut),
                             TMax = (index * 9) + 1,
                             stringsAsFactors = FALSE)
          results_47 <- rbind(results_47, temp)
     }
}

ggplot(results_47, aes(x=center,
                    y=average)) +
     geom_point(shape = 3)+
     geom_line(aes(y=TMax),
               color = "red") +
     xlab("Data Centers") +
     ylab("Number of Failed Attempts to Lockout") +
     geom_smooth(method = lm,
                 se = TRUE,
                 level = 0.99,
                 size = 0.25,
                 color = "blue") +
     geom_hline(yintercept = 10,
                color = "green") +
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"))
ggsave("./images/results_47.jpg")
