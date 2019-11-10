nfl <- read.csv(file = "nfl.csv", sep = ";", as.is = T)
x <- do.call(rbind, strsplit(x = nfl$Date, split = '-'))
nfl$Year <- as.numeric(x[, 1])
nfl$Month <- as.numeric(x[, 2])
nfl$Day <- as.numeric(x[, 3])
rm(x)


team <- "New England Patriots"
team <- "Seattle Seahawks"
team <- "Baltimore Ravens"


x <- nfl[nfl$Year >= 2019 & nfl$Month >= 8, ]
y <- x[x$Away.Team == team, ]
x <- x[x$Home.Team == team, ]

d <- data.frame(data = c(x$Date, y$Date),
                score = c(x$Home.Score, y$Away.Score),
                month = c(x$Month, y$Month),
                day = c(x$Day, y$Day),
                date = c(x$Date, y$Date),
                home.away = rep(x = c("H", "A"), times = c(nrow(x), nrow(y))),
                win = c(x$Home.Score>x$Away.Score, y$Home.Score<y$Away.Score),
                delta = c(x$Home.Score-x$Away.Score, y$Away.Score-y$Home.Score))
d$time <- d$month*d$day
d <- d[order(d$time, decreasing = F), ]


require(ggplot2)
ggplot(data = d)+
  geom_point(aes(y = date, x = delta/7, shape = home.away, col = win))+
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgray")+
  theme_bw()+
  theme(legend.position = "top")

ggplot(data = d)+
  geom_point(aes(y = date, x = score/7, shape = home.away, col = win))+
  geom_vline(xintercept = 0, linetype = "dashed", col = "darkgray")+
  theme_bw()+
  theme(legend.position = "top")


