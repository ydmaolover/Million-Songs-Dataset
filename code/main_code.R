

library(tidyverse)
library(corrplot)
library(lme4)
library(arm)
library(jtools)
library(ggstance)
library(lattice)


df_songs <- read_csv("Data/songs.csv")

#drop columns of NA&zero
df1 <- dplyr::select(df_songs, -(energy:idx_tatums_start))
df2 <- dplyr::select(df1, -c(danceability, analyzer_version, idx_artist_mbtags))
df3 <- dplyr::select(df2, -(genre:idx_similar_artists))


colSums(!is.na(df3))
#columns contains NA: 
#artist_familiarity: 9996 (almost compelete)
#artist_latitude, location, longitude: many NA, could be omitted
#artist_mbid: the musicbrainz.org ID for this artists is db9, could be omitted
#song_hotttnesss: 5648 (key matrix)
#title: 9999 (almost complete)
#year: 10000 but many zeros


#drop columns of id/name/incomplete
#keep track_id, artist_id, song_id
df4 <- dplyr::select(df3, -c(analysis_sample_rate, audio_md5, artist_7digitalid, artist_latitude, artist_location, artist_longitude
                      , artist_mbid, artist_name, artist_playmeid, release, release_7digitalid, title, track_7digitalid))


#drop rows with NA in song_hotttnesss
df5 <- filter(df4, !is.na(df4$song_hotttnesss))
colSums(!is.na(df5))
#only 1 NA in artist_familiarity, drop
df6 <- filter(df5, !is.na(artist_familiarity))


colSums(df6 == 0)
#columns where zeros do not make sense: tempo, time_signature (drop); year (large number, keep)
df7 <- filter(df6, tempo != 0)
df8 <- filter(df7, time_signature != 0)
#df8: 5637 obs, 15 variables (+3 ids)
colSums(is.na(df8))
colSums(df8 == 0)




#plots to get an overview
#histograms of some direct variables
hist(df8$song_hotttnesss, col="blue", xlab = "song_hotttnesss", main = "Distribution of song_hotttnesss")
hist(df8$artist_familiarity, col="blue", xlab = "artist_familiarity", main = "Distribution of artist_familiarity")
hist(df8$artist_hotttnesss, col="blue", xlab = "artist_hotttnesss", main = "Distribution of artist_hotttnesss")
hist(df8$duration, col="blue", xlab = "duration", main = "Distribution of duration")
hist(df8$year[df8$year != 0], col="blue", xlab = "year", main = "Distribution of year (0 dropped)") #plot years without 0




#check correlation
df9 <- dplyr::select(df8, -c(track_id, artist_id, song_id))
cor <- cor(df9)
round(cor, 2)
#visualization corrplot
library(corrplot)
corrplot::corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#dependent variable pairs 
#duration & start_of_fade_out; key_confidence & mode_confidence; artist_familiarity & artist_hotttness
#also found song_hotttness most correlated with artist_familiarity & artist_hotttness
#another interesting point: recent artists are more familiar and hot.



#geom_point
ggplot(df8, aes(artist_familiarity, song_hotttnesss)) +
  geom_point()
ggplot(df8, aes(artist_hotttnesss, song_hotttnesss)) +
  geom_point()
ggplot(df8, aes(loudness,song_hotttnesss)) +
  geom_point()
#how to deal with zeros in song_hotttness


df9 <- filter(df9, song_hotttnesss != 0)


#transformation to z-score
z_duration <- (df9$duration - mean(df9$duration)) / sd(df9$duration)
z_end_of_fade_in <- (df9$end_of_fade_in - mean(df9$end_of_fade_in)) / sd(df9$end_of_fade_in)
z_key <- (df9$key - mean(df9$key)) / sd(df9$key)
z_key_confidence <- (df9$key_confidence - mean(df9$key_confidence)) / sd(df9$key_confidence)
z_loudness <- (df9$loudness - mean(df9$loudness)) / sd(df9$loudness)
z_mode <- (df9$mode - mean(df9$mode)) / sd(df9$mode)
z_mode_confidence <- (df9$mode_confidence - mean(df9$mode_confidence)) / sd(df9$mode_confidence)
z_start_of_fade_out <- (df9$start_of_fade_out - mean(df9$start_of_fade_out)) / sd(df9$start_of_fade_out)
z_tempo <- (df9$tempo - mean(df9$tempo)) / sd(df9$tempo)
z_time_signature <- (df9$time_signature - mean(df9$time_signature)) / sd(df9$time_signature)
z_time_signature_confidence <- (df9$time_signature_confidence - mean(df9$time_signature_confidence)) / sd(df9$time_signature_confidence)
z_artist_familiarity <- (df9$artist_familiarity - mean(df9$artist_familiarity)) / sd(df9$artist_familiarity)
z_artist_hotttnesss <- (df9$artist_hotttnesss - mean(df9$artist_hotttnesss)) / sd(df9$artist_hotttnesss)
#assume no record in year 1950 (before 1950)
df9$year[df9$year == 0] <- 1950
#convert year to decades (exp, 80s, 90s)
decade <- floor((df9$year-1900)/10)*10
hist(decade, col="blue")
z_decade <- (decade - mean(decade)) / sd(decade)



#Linear Regression
lin_model <- lm(df9$song_hotttnesss ~ z_artist_hotttnesss + z_artist_familiarity + z_duration + z_end_of_fade_in +
                  z_key + z_key_confidence + z_loudness + z_mode + z_mode_confidence + z_start_of_fade_out +
                  z_tempo + z_time_signature + z_time_signature_confidence + z_decade +
                  z_artist_familiarity:z_artist_hotttnesss +
                  z_key_confidence:z_mode_confidence +
                  z_duration:z_start_of_fade_out +
                  z_decade:z_artist_familiarity +
                  z_decade:z_artist_hotttnesss)
summary(lin_model)
#fix only using significant variables
lin_model_fit <- lm(df9$song_hotttnesss ~ z_artist_hotttnesss + z_artist_familiarity + z_duration +
                      z_loudness + z_start_of_fade_out + z_tempo + z_decade +
                      z_artist_familiarity:z_artist_hotttnesss +
                      z_key_confidence:z_mode_confidence +
                      z_artist_familiarity:z_decade)
summary(lin_model_fit)


plot_summs(lin_model, scale = "TRUE", inner_ci_level = .95)


resid_lm <- resid(lin_model_fit)
plot(z_artist_hotttnesss, resid_lm, 
     ylab="Residuals", xlab="artist_hotttnesss", 
     main="Residual plot") 
abline(0, 0, col="red")# the horizon



plot(z_artist_hotttnesss, df9$song_hotttnesss) +
  curve(coef(lin_model_fit)[1] + coef(lin_model_fit)[2]*x, add = TRUE)
plot(z_artist_familiarity, df9$song_hotttnesss) +
  curve(coef(lin_model_fit)[1] + coef(lin_model_fit)[3]*x, add = TRUE)
plot(z_loudness, df9$song_hotttnesss) +
  curve(coef(lin_model_fit)[1] + coef(lin_model_fit)[5]*x, add = TRUE)




#mixed model grouped by decade
#EDA
df9$decade <- as.character(decade)
ggplot(df9, aes(artist_familiarity, song_hotttnesss, color=decade))+
  geom_point(size=1)


#drop 0 in song_hotttness



#mixed model
mix_model <- lmer(df9$song_hotttnesss ~ z_artist_hotttnesss + z_artist_familiarity + z_duration + z_end_of_fade_in +
                    z_key + z_key_confidence + z_loudness + z_mode + z_mode_confidence + z_start_of_fade_out +
                    z_tempo + z_time_signature + z_time_signature_confidence +
                    z_artist_familiarity:z_artist_hotttnesss +
                    z_key_confidence:z_mode_confidence +
                    z_duration:z_start_of_fade_out +
                    z_decade:z_artist_familiarity +
                    z_decade:z_artist_hotttnesss +
                    (1|decade))
summary(mix_model)

ggplot(df9, aes(artist_familiarity, song_hotttnesss, color=decade)) +
  geom_line(aes(y=predict(mix_model), group=decade))

## look at random effect
lattice::dotplot(ranef(mix_model, condVar=TRUE))



mix_slope <- lmer(df9$song_hotttnesss ~ z_artist_hotttnesss + z_artist_familiarity + z_duration+ z_loudness + 
                    z_start_of_fade_out +
                    (1+z_artist_hotttnesss + z_artist_familiarity + z_duration+ z_loudness + 
                       z_start_of_fade_out|decade))
summary(mix_slope)
lattice::dotplot(ranef(mix_slope, condVar=TRUE))
plot(mix_slope, resid(.) ~ fitted(.) | decade, abline=c(h = 0),  lty = 1,  type = c("p", "smooth")) # per decade

