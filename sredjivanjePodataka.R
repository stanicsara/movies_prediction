
#Ucitavanje dataset-ova

movies <- read.csv("data/imdb_movies.csv")
meta <- read.csv("data/meta.csv")
colnames(meta)[2] <- "metascore"


#Ucitavanje potrebnih paketa

#install.packages("stringr")
library(stringr)
library(tidyverse)
library(tidyr)
library(corrplot)
library(nortest)
library(ggplot2)
library(caret)

str(movies)
summary(movies)

#Analiza i sredjivanje izlazne promenljive

sort(unique(movies$usa_gross))[1:50]
sum(movies$usa_gross == "")/nrow(movies)
## 0.962

sort(unique(movies$u_gross_money))[1:50]
sum(is.na(movies$u_gross_money))/nrow(movies)
## 0.962
## usa_gross i u_gross_money imaju iste podatke, ali drugacijeg tipa, pa cemo koristiti numericku varijablu u_gross_money
## Posto ima dosta nedostajucih vrednosti redukovacemo dataset

with_usa_gross <- movies[!is.na(movies$u_gross_money),]
dim(with_usa_gross)
# 18528    33
remove(movies)


#Analiza metascore varijable

sum(is.na(with_usa_gross$metascore))
sum(is.na(with_usa_gross$metascore))/nrow(with_usa_gross) #0.45
sum(is.na(meta$metascore)) #0

length(intersect(meta$name, 
                 with_usa_gross[is.na(with_usa_gross$metascore),]$name))
## Spajanjem metascore i with_usa_gross, mozemo dodati metascore za 1713 filmova koji trenutno nemaju metascore

n_distinct(meta$name)
n_distinct(with_usa_gross$name)
## Imena filmova nisu unikatna ni u jednom datasetu, pa ova dva dataset-a ne mozemo spojiti i metascore ne moze biti popunjen iz meta dataset-a
remove(meta)


#Izbacivanje varijabli

n_distinct(with_usa_gross$production_companies)
## 15636
table(with_usa_gross$production_companies) |> sort(decreasing = T) |> head(10)
## Previse raznovrsno 

vars_to_remove <- c('name', 'org_name', 'date', 'director', 'writer', 
                    'story_line', 'cast','world_gross', 'usa_gross', 'budget',
                    'casts_id', 'keywords', 'production_companies')
## world_gross, usa_gross, budget i date imaju svoje numericke verzije koje mogu biti koriscene

vars_to_keep <- setdiff(names(with_usa_gross), vars_to_remove)
with_usa_gross <- with_usa_gross[, vars_to_keep]

colnames(with_usa_gross)[2] <- 'release_year'

table(with_usa_gross$BlogPage) |> prop.table() |> round(3)
## 0.997  0.003  => izbaciti
table(with_usa_gross$CompPage) |> prop.table() |> round(3)
## 0.954  0.046 => izbaciti
table(with_usa_gross$HomePage) |> prop.table() |> round(3)
## 0.65 0.35 => zadrzati
with_usa_gross <- with_usa_gross[, -c(16,17)]

#Analiza language varijable

n_distinct(with_usa_gross$language)
## 1537
table(with_usa_gross$language) |> sort(decreasing = TRUE) |> head(20)
## Pravimo tri grupe jezika: English_only, English_option, Non_English

lang_group <- function(movie_language){
  langs <- str_split(movie_language, ",")[[1]]
  if((length(langs) == 1) & (langs[1] == "English"))
    return("English")
  if((length(langs) > 1) & ("English" %in% langs))
    return("English_option")
  return("Non_English")
}

with_usa_gross$language_group <- sapply(with_usa_gross$language, lang_group)
with_usa_gross$language_group <- as.factor(with_usa_gross$language_group)
table(with_usa_gross$language_group)
with_usa_gross$language <- NULL


#Analiza country varijable

n_distinct(with_usa_gross$country)
## 2107
unique(with_usa_gross$country) |> sort() |> head(20)
table(with_usa_gross$country) |> sort(decreasing = TRUE) |> head(20)
## Pravimo 3 grupe drzava: USA_only, USA_co_production, Non_USA

cont_group <- function(movie_country){
  countries <- str_split(movie_country, c(",", ", ", " ,"))[[1]]
  if((length(countries) == 1) & (countries[1] == "USA"))
    return("USA_only")
  if((length(countries) > 1) & ("USA" %in% countries))
    return("USA_co_production")
  return("Non_USA")
}

with_usa_gross$country_group <- sapply(with_usa_gross$country, cont_group)
with_usa_gross$country_group <- as.factor(with_usa_gross$country_group)
table(with_usa_gross$country_group)
with_usa_gross$country <- NULL


#Analiza genres varijable

n_distinct(with_usa_gross$genres)
## 1909
table(with_usa_gross$genres) |> sort(decreasing = TRUE) |> head(20)

all_genres <- c("Drama")
for(genres in with_usa_gross$genres) {
  g_list <- str_split(genres, ',', simplify = TRUE)
  for(g_item in g_list) {
    if(!(g_item %in% all_genres))
      all_genres = append(all_genres, g_item)
  }
}
## Kreirali smo listu svih razlicitih zanrova

all_genres

all_genres_df <- data.frame(genre = all_genres,
                            count = 0)
for(genres in with_usa_gross$genres) {
  g_list <- str_split(genres, ',', simplify = TRUE)
  for(g_item in g_list) {
    cnt <- all_genres_df$count[all_genres_df$genre == g_item]
    all_genres_df$count[all_genres_df$genre == g_item] <- cnt + 1
  }
}
## Izracunali smo frekvenciju svih zanrova

all_genres_df |> arrange(desc(count))
## Pravimo po jednu kolonu za prvih 8 zanrova, a ostale cemo svrstati Other

unique_genres <- all_genres_df |> arrange(desc(count)) |> pull(genre) |> head(8)
unique_genres

genres_df <- with_usa_gross |>
  select(movie_id, genres) |>
  separate_rows(genres, sep = ",") |>
  mutate(genres = ifelse(genres %in% unique_genres, genres, "Other"))

genres_df_wide <- genres_df %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = genres, values_from = value, values_fn = mean, values_fill = 0)

with_usa_gross |>
  select(-c(genres)) |> 
  inner_join(genres_df_wide) -> movies_final

glimpse(movies_final)

#Pretvaramo dobijene varijable u faktorske

movies_final$Comedy <- as.factor(movies_final$Comedy)
movies_final$Action <- as.factor(movies_final$Action)
movies_final$Adventure <- as.factor(movies_final$Adventure)
movies_final$Thriller <- as.factor(movies_final$Thriller)
movies_final$Documentary <- as.factor(movies_final$Documentary)
movies_final$Drama <- as.factor(movies_final$Drama)
movies_final$Romance <- as.factor(movies_final$Romance)
movies_final$Crime <- as.factor(movies_final$Crime)
movies_final$Other <- as.factor(movies_final$Other)
movies_final$HomePage <- as.factor(movies_final$HomePage)

#Provera nedostajucih vrednosti kod numerickih varijabli

with_usa_gross |> select_if(is.numeric) -> num_vars_df
apply(num_vars_df, 2, function(x) sum(is.na(x)))
apply(num_vars_df, 2, function(x) round(sum(is.na(x))/nrow(num_vars_df), 4))
## Veliki procenat nedostajucih vrednosti imaju metascore i dollar_budget varijabla


#Provera korelacije w_gross_money i u_gross_money, provera korelacije dollar_budget i metascore sa U_gross_money

movies_cor <- cor(num_vars_df[!is.na(num_vars_df$w_gross_money + num_vars_df$metascore + num_vars_df$dollar_budget),])
corrplot.mixed(movies_cor) 
## Korelacija je 0.94 izmedju w_gross_money i u_gross_money, korelacije izmedju dollar_budget i u_gross_money je 0.69, a izmedju 
## metascore i u_gross_money je 0.14

movies_final$w_gross_money <- NULL
## Izbacujemo w_gross_money jer cemo u_gross_money koristiti kao izlaznu varijablu, a izmedju njih postoji visoka zavisnost

final_data <- movies_final[!is.na(movies_final$dollar_budget),]
str(final_data)
summary(final_data)
## Redukujemo dataset prema nedostajucim vrednostima za dollar_budget jer nam je ta varijabla znacajnija


#Provera nedostajucih vrednosti nakon sredjivanja podataka i redukcije 

final_data |> select_if(is.numeric) -> num_vars_df_2
apply(num_vars_df_2, 2, function(x) sum(is.na(x)))
apply(num_vars_df_2, 2, function(x) round(sum(is.na(x))/nrow(num_vars_df_2), 4))
final_data$metascore <- NULL 
## Izbacujemo varijablu metascore jer ima 23% nedostajucih vrednosti


#Dopuna nedostajucih vrednosti

ad.test(final_data$point) #Anderson-Darling test za proveru normalnosti
mdn_point <- median(final_data$point, na.rm = TRUE)
final_data$point[is.na(final_data$point)] <- mdn_point

ad.test(final_data$runtime)
mdn_runtime <- median(final_data$runtime, na.rm = TRUE)
final_data$runtime[is.na(final_data$runtime)] <- mdn_runtime


#Mnozenje dollar_budzeta i u_gross_money sa inflation_coeff

final_data$dollar_budget <- final_data$dollar_budget * final_data$inflation_coeff
final_data$u_gross_money <- final_data$u_gross_money * final_data$inflation_coeff
final_data$inflation_coeff <- NULL
summary(final_data)
str(final_data)



####### PRAVLJENJE IZLAZNE PROMENLJIVE #######

revenue_75perc <- quantile(final_data$u_gross_money, probs = 0.75)
final_data$high_revenue <- ifelse(test = final_data$u_gross_money > revenue_75perc,
                                  yes = "Yes",
                                  no = "No")

final_data$u_gross_money <- NULL
final_data$high_revenue <- as.factor(final_data$high_revenue)
prop.table(table(final_data$high_revenue))



######################## PROCENA ZNACAJNOSTI PROMENLJIVIH ##############################


#Provera znacajnosti faktorskih varijabli


plot_language <- ggplot(final_data, aes(x = language_group, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Percentage of movies") +
  xlab("Language group") +
  theme_bw()
plot_language #znacajna

plot_country <- ggplot(final_data, aes(x = country_group, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Country group") +
  theme_bw()
plot_country #znacajna

plot_comedy <- ggplot(final_data, aes(x = Comedy, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Comedy") +
  theme_bw()
plot_comedy #nije znacajna

plot_homepage <- ggplot(final_data, aes(x = HomePage, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("HomePage") +
  theme_bw()
plot_homepage #nije znacajna

plot_action <- ggplot(final_data, aes(x = Action, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Action") +
  theme_bw()
plot_action #znacajna

plot_adventure <- ggplot(final_data, aes(x = Adventure, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Adventure") +
  theme_bw()
plot_adventure #znacajna

plot_thriller <- ggplot(final_data, aes(x = Thriller, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Thriller") +
  theme_bw()
plot_thriller #nije znacajna

plot_documentary <- ggplot(final_data, aes(x = Documentary, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Documentary") +
  theme_bw()
plot_documentary #znacajna

plot_drama <- ggplot(final_data, aes(x = Drama, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Drama") +
  theme_bw()
plot_drama #znacajna

plot_romance <- ggplot(final_data, aes(x = Romance, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Romance") +
  theme_bw()
plot_romance #nije znacajna

plot_crime <- ggplot(final_data, aes(x = Crime, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Crime") +
  theme_bw()
plot_crime #nije znacajna

plot_other <- ggplot(final_data, aes(x = Other, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Other genre") +
  theme_bw()
plot_other #znacajna


#Provera znacajnosti numerickih varijabli


plot_release_year <- ggplot(final_data, aes(x = release_year, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_release_year 
kruskal.test(release_year ~ high_revenue, data = final_data) #znacajna

plot_point <- ggplot(final_data, aes(x = point, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_point
kruskal.test(point ~ high_revenue, data = final_data) #znacajna

plot_point_volume <- ggplot(final_data, aes(x = point_volume, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_point_volume 
kruskal.test(point_volume ~ high_revenue, data = final_data) #znacajna

plot_user_reviews <- ggplot(final_data, aes(x = user_reviews, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_user_reviews
kruskal.test(user_reviews ~ high_revenue, data = final_data) #znacajna

plot_critic_reviews <- ggplot(final_data, aes(x = critic_reviews, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_critic_reviews
kruskal.test(critic_reviews ~ high_revenue, data = final_data) #znacajna

plot_runtime <- ggplot(final_data, aes(x = runtime, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_runtime 
kruskal.test(runtime ~ high_revenue, data = final_data) #znacajna

plot_budget <- ggplot(final_data, aes(x = dollar_budget, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_budget
kruskal.test(dollar_budget ~ high_revenue, data = final_data) #znacajna

plot_release_month <- ggplot(final_data, aes(x = release_month, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_release_month
kruskal.test(release_month ~ high_revenue, data = final_data) #znacajna

plot_release_day <- ggplot(final_data, aes(x = release_day, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
plot_release_day
kruskal.test(release_day ~ high_revenue, data = final_data) #nije znacajna


vars_to_keep_2 <- c(2:8, 10, 12:13, 15:17, 19:20, 23)
final_data <- final_data[, vars_to_keep_2]
saveRDS(final_data, "movies_final.RDS")




