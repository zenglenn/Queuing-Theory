install.packages('queueing')
library(queueing)

#Glendwood
i_mmck = NewInput.MM1(lambda=.02624, mu=.05108, n=0)#, c=6, k=13)

outputMMC = QueueingModel(i_mmck)

Report(outputMMC)


curve(dpois(x, i_mmck$lambda),
      from = 0, 
      to = 20, 
      type = "b", 
      lwd = 2,
      xlab = "Number of Patients",
      ylab = "Probability",
      main = "Poisson Distribution for Arrival Process",
      ylim = c(0, 0.75),
      n = 21)


curve(dexp(x, rate = 1/i_mmck$lambda),
      from = 0, 
      to = 10,
      type = "l", 
      lwd = 2,
      xlab = "Interarrival Time",
      ylab = "Probaility",
      main = "Exponential Distribution for Interarrival Time",
      ylim = c(0, 1))
abline(h = 0)


#Waiting Time for Service

curve(dexp(x, rate = i_mmck$mu),
      from = 0, 
      to = 5, 
      type = "l", 
      lwd = 2,
      xlab = "Service Waiting Time",
      ylab = "Probability",
      main = "Exponential Distribution for Service Process",
      ylim = c(0, 1))
abline(h = 0)


library(simmer)
library(simmer.plot)
set.seed(1234)

lambda <- .026240329
mu <- .051082625



mm1.trajectory <- create_trajectory() %>%
  seize("resource", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("resource", amount=1)

mm1.env <- simmer() %>%
  add_resource("resource", capacity=1, queue_size=Inf) %>%
  add_generator("arrival", mm1.trajectory, function() rexp(1, lambda)) %>%
  run(until=2000)

