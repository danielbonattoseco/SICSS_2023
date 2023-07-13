pacman::p_load(tidyverse, rio, mirt, psych, lubridate, changepoints,changepoint,
               ggchangepoint)

options(scipen = 9999999)

####### Check de indicadores #########

bd <- import("Projeto final/2023-07-10-11-06-19-BRT-search-csv-export.csv")

bd <- bd |> 
  mutate(like_p = (Likes - min(bd$Likes))/(max(bd$Likes)- min(bd$Likes)),
         comment_p = (Comments - min(bd$Comments))/(max(bd$Comments)- min(bd$Comments)),
         share_p = (Shares - min(bd$Shares))/(max(bd$Shares)- min(bd$Shares)),
         love_p = (Love - min(bd$Love))/(max(bd$Love)- min(bd$Love)),
         wow_p = (Wow - min(bd$Wow))/(max(bd$Wow)- min(bd$Wow)),
         haha_p = (Haha - min(bd$Haha))/(max(bd$Haha)- min(bd$Haha)),
         sad_p = (Sad - min(bd$Sad))/(max(bd$Sad)- min(bd$Sad)),
         angry_p = (Angry - min(bd$Angry))/(max(bd$Angry)- min(bd$Angry)),
         care_p = (Care - min(bd$Care))/(max(bd$Care)- min(bd$Care)))

m_p <- bd |> select(ends_with("_p"))

alpha(m_p)

psych::fa.parallel(m_p)

TRI1 <- mirt(data = m_p,model = 1,itemtype = 'graded')

Item1 <- mirt::itemplot( TRI1, type = 'info',item = 1, main = "Like")
Item2 <- mirt::itemplot( TRI1, type = 'info',item = 2, main = "Comment")
Item3 <- mirt::itemplot( TRI1, type = 'info',item = 3, main = "Share")
Item4 <- mirt::itemplot( TRI1, type = 'info',item = 4, main = "Love")
Item5 <- mirt::itemplot( TRI1, type = 'info',item = 5, main = "Wow")
Item6 <- mirt::itemplot( TRI1, type = 'info',item = 6, main = "Haha")
Item7 <- mirt::itemplot( TRI1, type = 'info',item = 7, main = "Sad")
Item8 <- mirt::itemplot( TRI1, type = 'info',item = 8, main = "Angry")
Item9 <- mirt::itemplot( TRI1, type = 'info',item = 9, main = "Care")

gridExtra::grid.arrange(Item1,Item2,Item3,Item4,Item5,Item6,Item7,Item8,Item9)

cor.plot(m_p)

rm(list = ls())
####################### FACEBOOK ########################
############ Analise do Bolsonaro por post ##############

bd <- import("Projeto final/Posts_facebook.csv", quote = "")

bd <- bd |> mutate(data = mdy_hm(Date),
                   day = day(data),
                   month = month(data),
                   year = year(data),
                   week = week(data)) |> 
  unite(MY,  year,month, sep = "-") |> 
  unite(DMY, MY, day, sep = "-" ) |> 
  mutate(DMY = ymd(DMY))
dev.off()

bd |> 
  ggplot(aes(x = data, y = `Number of Reactions, Comments & Shares`)) +
  geom_path()

bd |> 
  ggplot(aes(x = data, y = `Number of Reactions, Comments & Shares`)) +
  geom_smooth()

bd |> 
  ggplot(aes(x = data, y = `Number of Reactions, Comments & Shares`)) +
  geom_path()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Engajamento por post - Jair Bolsonaro (Julho 2022 - Julho 2023)", x = "Data", y =
         "Reações, comentários e compartilhamentos")

change <- cpt.meanvar(bd$`Number of Reactions, Comments & Shares`,penalty = "MBIC",
                   method = "PELT")
plot(change, main = "Changepoints dos posts- Jair Bolsonaro (Julho 2022 - Julho 2023)", xlab = "Tempo", ylab = "Engajamento")

print(change)

points_of_change <- c(39, 93, 253, 312, 404, 420, 492, 545, 680, 746, 785, 922, 924,
                      930)

bd <- bd |> mutate(is_change_post = ifelse(row_number()%in% points_of_change,1,0 ))

bd_ponts <- bd |> filter(is_change_post == 1)

############ Analise do Bolsonaro por dia ##############

# Cores para os gráficos
colorsslide <- c("#E7B800","coral2","#2E9FDF")
colorspaper <- c("gray15","gray50","gray75")

bd_day <- bd |> 
  group_by(DMY) |> 
  summarise(mean = mean(`Number of Reactions, Comments & Shares`),
            median = median(`Number of Reactions, Comments & Shares`),
            number_posts = n())


ggplot(data = bd_day, aes(x = DMY, y = number_posts)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Intervalo", y = "Quantidade de posts", title = "Posts por dia no Facebook")


bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_path()

bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_smooth()

bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_path()+
  geom_smooth()


bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_path()

bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_smooth()

bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_path()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Engajamento por dia - Jair Bolsonaro (Julho 2022 - Julho 2023)", x = "Data", y =
         "Mediana das reações, comentários e compartilhamentos")

change_DMeam <- cpt.meanvar(bd_day$mean,penalty = "MBIC",
                      method = "PELT")
plot(change_DMeam)
print(change_DMeam)

change_DMedian <- cpt.meanvar(bd_day$median,penalty = "MBIC",
                            method = "PELT")
plot(change_DMedian, main = "Changepoints Engajamento diário- Jair Bolsonaro (Julho 2022 - Julho 2023)", xlab = "Tempo", ylab = "Mediana do Engajamento")
print(change_DMedian)

d_p_c <- c(100, 103, 128, 140, 166, 259)

bd_day <- bd_day |> mutate(is_change_post = ifelse(row_number()%in% d_p_c,1,0 ))

bd_day_change <- bd_day |> filter(is_change_post == 1)

export(bd_day_change, "Posts_change.xlsx")

############ Analise do Bolsonaro por semana ##############

bd_week <- bd |> 
  group_by(week) |> 
  summarise(mean = mean(`Number of Reactions, Comments & Shares`),
            median = median(`Number of Reactions, Comments & Shares`))


bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_path()

bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_smooth()

bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_path()+
  geom_smooth()


bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_path()

bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_smooth()

bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_path()+
  geom_smooth()

change_WMeam <- cpt.meanvar(bd_week$mean,penalty = "MBIC",
                            method = "PELT")
plot(change_WMeam)
print(change_WMeam)

change_WMedian <- cpt.meanvar(bd_week$median,penalty = "MBIC",
                              method = "PELT")
plot(change_WMedian)
print(change_WMedian)

d_p_c <- c(100, 103, 128, 140, 166, 259)

bd_day <- bd_day |> mutate(is_change_post = ifelse(row_number()%in% d_p_c,1,0 ))

bd_day_change <- bd_day |> filter(is_change_post == 1)

####################### Instagram ########################
############ Analise do Bolsonaro por post ##############

bd <- import("Projeto final/bd_insta.csv")

class(bd$Total.interactions)

bd <- bd |> mutate(data = ymd(Date),
                   day = day(data),
                   month = month(data),
                   year = year(data),
                   week = week(data)) |> 
  unite(MY,  year,month, sep = "-") |> 
  unite(DMY, MY, day, sep = "-" ) |> 
  mutate(DMY = ymd(DMY)) |> 
  arrange(desc(DMY))
dev.off()

bd |> 
  ggplot(aes(x = data, y = Total.interactions)) +
  geom_path()

bd |> 
  ggplot(aes(x = data, y = Total.interactions)) +
  geom_smooth()

bd |> 
  ggplot(aes(x = data, y = Total.interactions)) +
  geom_path()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Engajamento por post - Jair Bolsonaro (Julho 2022 - Julho 2023)", x = "Data", y =
         "Reações, comentários e compartilhamentos")

change <- cpt.meanvar(bd$Total.interactions,penalty = "MBIC",
                      method = "PELT")
plot(change, main = "Changepoints dos posts- Jair Bolsonaro (Julho 2022 - Julho 2023)", xlab = "Tempo", ylab = "Engajamento")

print(change)

points_of_change <- c(17, 19, 228, 272, 276, 347, 392, 449, 608, 615)

bd <- bd |> mutate(is_change_post = ifelse(row_number()%in% points_of_change,1,0 ))

bd_ponts <- bd |> filter(is_change_post == 1)

############ Analise do Bolsonaro por dia ##############

class(bd_day$Total.interactions)

bd_day <- bd |> 
  group_by(DMY) |> 
  mutate(as.numeric(Total.interactions)) |> 
  summarise(mean = mean(Total.interactions),
            median = median(Total.interactions))


bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_path()

bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_smooth()

bd_day |> 
  ggplot(aes(x = DMY, y = mean)) +
  geom_path()+
  geom_smooth()


bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_path()

bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_smooth()

bd_day |> 
  ggplot(aes(x = DMY, y = median)) +
  geom_path()+
  geom_smooth()+
  theme_minimal()+
  labs(title = "Engajamento por dia - Jair Bolsonaro (Julho 2022 - Julho 2023)", x = "Data", y =
         "Mediana das reações, comentários e compartilhamentos")

change_DMeam <- cpt.meanvar(bd_day$mean,penalty = "MBIC",
                            method = "PELT")
plot(change_DMeam)
print(change_DMeam)

change_DMedian <- cpt.meanvar(bd_day$median,penalty = "MBIC",
                              method = "PELT")
print(change_DMedian)

d_p_c <- c(38, 99, 103, 127, 129, 169, 235 )
dados_plot <- bd_day[d_p_c, ]

# Plot com os labels personalizados
plot(change_DMedian, main = "Changepoints Engajamento diário- Jair Bolsonaro", xlab = "Tempo", ylab = "Mediana do Engajamento", xaxt = "n")
axis(1, at = d_p_c, labels = dados_plot$DMY, las = 2, cex.axis = 0.5)


bd_day <- bd_day |> mutate(is_change_post = ifelse(row_number()%in% d_p_c,1,0 ))

bd_day_change <- bd_day |> filter(is_change_post == 1)

export(bd_day_change, "Posts_change_Insta.xlsx")

############ Analise do Bolsonaro por semana ##############

bd_week <- bd |> 
  group_by(week) |> 
  summarise(mean = mean(`Number of Reactions, Comments & Shares`),
            median = median(`Number of Reactions, Comments & Shares`))


bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_path()

bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_smooth()

bd_week |> 
  ggplot(aes(x = week, y = mean)) +
  geom_path()+
  geom_smooth()


bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_path()

bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_smooth()

bd_week |> 
  ggplot(aes(x = week, y = median)) +
  geom_path()+
  geom_smooth()

change_WMeam <- cpt.meanvar(bd_week$mean,penalty = "MBIC",
                            method = "PELT")
plot(change_WMeam)
print(change_WMeam)

change_WMedian <- cpt.meanvar(bd_week$median,penalty = "MBIC",
                              method = "PELT")
plot(change_WMedian)
print(change_WMedian)

d_p_c <- c(100, 103, 128, 140, 166, 259)

bd_day <- bd_day |> mutate(is_change_post = ifelse(row_number()%in% d_p_c,1,0 ))

bd_day_change <- bd_day |> filter(is_change_post == 1)