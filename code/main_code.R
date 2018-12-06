

library(tidyverse)
library(corrplot)



df_songs <- read_csv("~/Desktop/678-final-project/data/songs.csv")

#drop columns of NA&zero
df1 <- select(df_songs, -(energy:idx_tatums_start))
df2 <- select(df1, -c(danceability, analyzer_version, idx_artist_mbtags))
df3 <- select(df2, -(genre:idx_similar_artists))


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
df4 <- select(df3, -c(analysis_sample_rate, audio_md5, artist_7digitalid, artist_latitude, artist_location, artist_longitude
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
hist(df8$song_hotttnesss)
hist(df8$artist_familiarity)
hist(df8$artist_hotttnesss)
hist(df8$year[df8$year != 0]) #plot years without 0




#check correlation
df9 <- select(df8, -c(track_id, artist_id, song_id))
cor <- cor(df9)
round(cor, 2)
#visualization corrplot
library(corrplot)
corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#dependent variable pairs 
#duration & start_of_fade_out; key_confidence & mode_confidence; artist_familiarity & artist_hotttness
#also found song_hotttness most correlated with artist_familiarity & artist_hotttness



#geom_point
ggplot(df8, aes(song_hotttnesss, artist_familiarity)) +
  geom_point()

ggplot(df8, aes(song_hotttnesss, artist_hotttnesss)) +
  geom_point()
#how to deal with zeros in song_hotttness






