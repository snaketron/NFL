require(rstan)
rstan_options(auto_write = TRUE)
# m.scores <- rstan::stan_model(file = "model_scores.stan")
# m.delta <- rstan::stan_model(file = "model_delta.stan")


getPmax <- function(x) {
  p <- sum(x>0)/length(x)
  return(2*max(c(p, 1-p))-1)
}


# NFL data
nfl <- read.csv(file = "nfl.csv", sep = ";", as.is = T)
x <- do.call(rbind, strsplit(x = nfl$Date, split = '-'))
nfl$Year <- as.numeric(x[, 1])
nfl$Month <- as.numeric(x[, 2])
nfl$Day <- as.numeric(x[, 3])
rm(x)
nfl <- nfl[nfl$Year == 2018, ]
nfl <- nfl[-which(nfl$Year == 2018 & nfl$Month == 1), ]
nfl <- nfl[-which(nfl$Year == 2018 & nfl$Month == 2), ]
nfl <- nfl[nfl$Playoff.Game. == "", ]


teams <- unique(as.character(c(nfl$Home.Team, nfl$Away.Team)))
for(i in 1:length(teams)) {
  nfl$Home.Team.Numeric[nfl$Home.Team == teams[i]] <- i
  nfl$Away.Team.Numeric[nfl$Away.Team == teams[i]] <- i
}

i <- which(nfl$Month == 12)
nfl.predict <- nfl[i, ]
nfl <- nfl[-i, ]
rm(i)


# Description
getNflDataStan <- function(nfl) {
  teams <- unique(nfl$Home.Team)
  
  dt <- c()
  for(team in teams) {
    x <- nfl[nfl$Home.Team == team, ]
    y <- nfl[nfl$Away.Team == team, ]
    
    d <- data.frame(team = x$Home.Team[1],
                    team.numeric = x$Home.Team.Numeric[1],
                    data = c(x$Date, y$Date),
                    score = c(x$Home.Score, y$Away.Score),
                    score.TD = c(x$Home.Score, y$Away.Score)/7,
                    month = c(x$Month, y$Month),
                    day = c(x$Day, y$Day),
                    date = c(x$Date, y$Date),
                    home.away = rep(x = c("H", "A"), times = c(nrow(x), nrow(y))),
                    home.away.numeric = ifelse(
                      test = rep(x = c("H", "A"), times = c(nrow(x), nrow(y))) == "H", yes = 1, no = -1),
                    win = c(x$Home.Score>x$Away.Score, y$Home.Score<y$Away.Score),
                    delta = c(x$Home.Score-x$Away.Score, y$Away.Score-y$Home.Score),
                    delta.TD = c(x$Home.Score-x$Away.Score, y$Away.Score-y$Home.Score)/7)
    
    d$time <- d$month*d$day
    d <- d[order(d$time, decreasing = F), ]
    
    dt <- rbind(dt, d)
  }
  
  return (dt)
}

nfl <- getNflDataStan(nfl = nfl)

nfl.mean <- aggregate(score.TD~team, data = nfl, FUN = mean)
nfl.mean <- nfl.mean[order(nfl.mean$score.TD, decreasing = T), ]
nfl.mean$rank <- as.character(nfl.mean$team)
nfl$rank <- as.character(nfl$team)
nfl$rank <- factor(x = nfl$rank, levels = nfl.mean$rank)
rm(nfl.mean)
ggplot(data = nfl)+
  geom_point(aes(y = rank, x = score.TD, shape = home.away, col = win))+
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgray")+
  theme_bw()+
  theme(legend.position = "top")




nfl.mean <- aggregate(delta.TD~team, data = nfl, FUN = mean)
nfl.mean <- nfl.mean[order(nfl.mean$delta.TD, decreasing = T), ]
nfl.mean$rank <- as.character(nfl.mean$team)
nfl$rank <- as.character(nfl$team)
nfl$rank <- factor(x = nfl$rank, levels = nfl.mean$rank)
rm(nfl.mean)
ggplot(data = nfl)+
  geom_point(aes(y = rank, x = delta.TD, shape = home.away, col = win))+
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgray")+
  theme_bw()+
  theme(legend.position = "top")







data.list <- list(N = nrow(nfl),
                  Nt = max(nfl$team.numeric),
                  y = nfl$delta.TD,
                  x = nfl$team.numeric,
                  home = nfl$home.away.numeric,
                  N_pred = nrow(nfl.predict),
                  x_pred = nfl.predict[, c("Home.Team.Numeric", 
                                           "Away.Team.Numeric")])

glm <- rstan::sampling(object = m.delta,
                       data = data.list,
                       chains = 4,
                       cores = 4,
                       iter = 5000,
                       warmup = 2000,
                       refresh = 1000,
                       include = TRUE,
                       control = list(adapt_delta = 0.95,
                                      max_treedepth = 10))


x <- summary(glm, "y_delta")$summary
e <- rstan::extract(object = glm, par = "y_delta")

score_mu <- summary(glm, "score_mu")$summary
e <- rstan::extract(object = glm, par = "score_home")
home.savagery <- apply(X = e$score_home, MARGIN = 2, FUN = getPmax)
nfl$team.savagery <- NA
for(i in 1:length(home.savagery)) {
  nfl$team.savagery[nfl$team.numeric == i] <- home.savagery[i]
}
plot(home.savagery)


x <- cbind(nfl.predict, x)
x$pmax <- apply(X = e$y_delta, MARGIN = 2, FUN = getPmax)
x <- x[order(x$pmax, decreasing = T), ]
x$rank <- paste(x$Home.Team, x$Away.Team, sep = '-')
x$rank <- factor(x = x$rank, levels = x$rank)
x$real.delta.TD <- (x$Home.Score - x$Away.Score)/7
x$OR <- x$Home.Odds.Open/(x$Away.Odds.Open + x$Home.Odds.Open)
x$OR.bit <- ifelse(test = 1-x$Home.Odds.Open/(x$Home.Odds.Open+x$Away.Odds.Open)>=0.5, yes = 1, no = 0)
x$my.bit <- ifelse(test = x$mean >= 0, yes = 1, no = 0)
x$win.bit <- ifelse(test = x$Home.Score>x$Away.Score, yes = 1, no = 0)

ggplot(data = x)+
  geom_errorbarh(aes(y = rank, xmin = `2.5%`, xmax = `97.5%`), col = "darkgray")+
  geom_point(aes(x = real.delta.TD, y = rank), col = "red", size = 2, shape = 21)+
  geom_point(aes(x = mean, y = rank), col = "black", size = 2, shape = 21)+
  geom_text(aes(x = -10, y = rank, label = round(pmax, digits = 2)), size = 2.5)+
  geom_text(aes(x = -8, y = rank, label = paste("OR:", round(x = OR, digits = 2))), size = 2.5)+
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgray")+
  theme_bw(base_size = 9)


# cost
x$case <- ifelse(test = (x$real.delta.TD > 0 & x$mean > 0) | (x$real.delta.TD < 0 & x$mean < 0), yes = 1, no = -1)
cost <- 0
for(i in 1:nrow(x)) {
  if(x$case[i] == -1) {
    cost <- cost - 1
  }
  else {
    if(x$mean[i]>=0) {
      cost <- cost + x$Home.Odds.Open[i]*x$case[i]
    } 
    else {
      cost <- cost + x$Away.Odds.Open[i]*x$case[i]
    }
  }
}
cost


plot(x$OR, x$pmax)
plot(cumsum(x$case)/nrow(x))


x$OR
