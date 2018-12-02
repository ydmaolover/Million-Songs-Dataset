

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
library(rhdf5)
library(tidyverse)



#test code for song1
h5ls("~/Desktop/678-final-project/data/MillionSongSubset/data/A/A/A/TRAAAAW128F429D538.h5")

test1 <- h5read("~/Desktop/678-final-project/data/MillionSongSubset/data/A/A/A/TRAAAAW128F429D538.h5", "/analysis/songs")
test1$key
test2 <- h5read("~/Desktop/678-final-project/data/MillionSongSubset/data/A/A/A/TRAAAAW128F429D538.h5", "/metadata/songs")
test3 <- h5read("~/Desktop/678-final-project/data/MillionSongSubset/data/A/A/A/TRAAAAW128F429D538.h5", "/musicbrainz/songs")
df_songs <- merge(
  merge(test1, test2), test3)



#file directories
dirs1 <- list.dirs("~/Desktop/678-final-project/data/MillionSongSubset/data", recursive = FALSE)
dirs2 <- list.dirs(dirs1, recursive = FALSE)
dirs3 <- list.dirs(dirs2, recursive = FALSE)
files <- list.files(dirs3, full.names = TRUE)



#read function
read_songs <- function(f){
  song1 <- h5read(f, "/analysis/songs")
  song2 <- h5read(f, "/metadata/songs")
  song3 <- h5read(f, "/musicbrainz/songs")
  song <- merge(
    merge(song1, song2), song3)
}

list_songs <- lapply(files, read_songs)



#rbind 
for (i in 2:length(files)) {
  df_songs <- rbind(df_songs, list_songs[[i]])
}



write_csv(df_songs, "~/Desktop/678-final-project/data/songs.csv")


