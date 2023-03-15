library(geniusr)
library(dplyr)

# create directory for corpus
new_dir <- file.path(getwd(), "corpus")
dir.create(new_dir)

# access token for genius api
client_access_token <- "wuOhiXe1HaSJBkR-XGoQpYlZ4VcwRDxdrC6uK3gR0QjZJgm4in1ZgJTlDkjRNPtO"

# read files for male and female artists and shuffle list with sample()
maleArtists <- as.list(readLines("./male_artists.txt"))
maleArtists <- sample(maleArtists)
femaleArtists <- as.list(readLines("./female_artists.txt"))
femaleArtists <- sample(femaleArtists)

# function to format song titles in saveable format
formatTitle <- function(title) {
  removeSpecialCharacters <- gsub("[^[:alnum:][:space:]]", "", title)
  removeWhitespaces <- gsub(" ", "", removeSpecialCharacters)
  return(removeWhitespaces)
}

# function to retrieve lyrics in try catch block
getLyrics <- function(id, jahr, gender) {
  tryCatch(
    expr =  {
      lyrics <- get_lyrics_id(song_id = id, access_token = client_access_token)
      
      # filter malformed songs
      if(!is.na(lyrics$artist_name[1]) && !is.na(lyrics$song_name[1]) && !is.na(lyrics[, 1])) {
        artistName <- lyrics$artist_name[1]
        artistName <- formatTitle(artistName)
        
        songTitel <- lyrics$song_name[1]
        songTitel <- formatTitle(songTitel)
        
        titel <- gender
        titel <- paste0(titel, jahr)
        titel <- paste0(titel, "_")
        titel <- paste0(titel, artistName)
        titel <- paste0(titel, "_")
        titel <- paste0(titel, songTitel)
        titel <- paste0(titel, ".txt")
        speicherpfad <- file.path("./corpus", titel)
        write.table(lyrics[, 1], file = speicherpfad, col.names = FALSE, row.names = FALSE, quote = FALSE)
      }
    },
    error = function(e) {
      # print occuring errors
      print(e)
    }
  )
}

# function to assign id to name of an artist to a dataframe, if it exists on genius 
assignArtistId <- function(artistList, artists_df) {
  for(artist in artistList) {
    searchRequest <- search_artist(search_term = artist, access_token = client_access_token)
    
    for(i in 1:nrow(searchRequest)) {
      row <- searchRequest[i, ]
      artistName <- formatTitle(artist)
      artistName <- tolower(artistName)
      artistSearchResult <- formatTitle(row$artist_name)
      artistSearchResult <- tolower(artistSearchResult)
      if(artistSearchResult == artistName) {
        artists_df <- rbind(artists_df, c(artist, row$artist_id))
      }
    }
  }
  artists_df <- slice(artists_df, -1)
  print("process done: assigning ids to artist names")
  return(artists_df)
}

# function to assign songsids to publisihing years to a dataframe
assignSongIds <- function(artists_df, songs_df) {
  for(i in 1:nrow(artists_df)) {
    row <- artists_df[i, ]
    # songs für artist id
    songsByArtist <- get_artist_songs(artist_id = row$id, sort = "title", access_token = client_access_token)
    # für jeden song füge songid und jahr zu songs_df
    for(song in songsByArtist$content) {
      # check if song has valid release year
      if(!is.null(song$release_date_components$year)) {
        songs_df <- rbind(songs_df, c(song$release_date_components$year, song$id))
      }
    }
  }
  print("process done: assigning ids to artist names")
  return(songs_df)
}

# function to iterate over songids of male and female artists to write lyrics in corpus directory
writeLyricsFromDataframe <- function (maleSongs, femaleSongs) {
  # iterate from 2013 until 2023
  for(i in 2013:2023) {
    # check if song ids exist for year
    if(length(subset(maleSongs, jahr == i)$id) > 0 && length(subset(femaleSongs, jahr == i)$id) > 0) {
      # set maximum amount of lyrics to generate to the amount of female song ids per year, maximum 100 to limit corpus
      maximumAmountOfLyrics <- length(subset(femaleSongs, jahr == i)$id)
      if(maximumAmountOfLyrics > 100) {
        maximumAmountOfLyrics <- 100
      }
      
      # retrieve subset of ideas for year and shuffle values
      maleSongsSubset <- subset(maleSongs, jahr == i)
      maleSongsSubset <- sample(maleSongsSubset)
      femaleSongsSubset <- subset(femaleSongs, jahr == i)
      femaleSongsSubset <- sample(femaleSongsSubset)
      
      # iterate over male song ids of i-year
      for(m in 1:nrow(maleSongsSubset)) {
        # stop taking song lyrics if maxAmount reached
        if(m > maximumAmountOfLyrics) {
          break
        }
        row <- maleSongsSubset[m, ]
        getLyrics(row$id, i, "male_")
      }
      
      # iterate over female song ids of i-year
      for(f in 1:nrow(femaleSongsSubset)) {
        row <- femaleSongsSubset[f, ]
        getLyrics(row$id, i, "female_")
      }
    }
  }
  print("process done: writing lyrics to the corpus directory")
}

# template dataframe for artist
# attributes: name, id
templateArtists_df <- data.frame(name = c(""), id = c(0))

# transform lists to dataframes
maleArtists_df <- assignArtistId(maleArtists, templateArtists_df)
femaleArtists_df <- assignArtistId(femaleArtists, templateArtists_df)

# template dataframe for songs
# attributes: year, songid
templateSongs_df <- data.frame(jahr = c(0), id = c(0))

# creat dataframes for male and female songs
maleSongs_df <- assignSongIds(maleArtists_df, templateSongs_df)
femaleSongs_df <- assignSongIds(femaleArtists_df, templateSongs_df)

# write lyrics to corpus from song datatframes
writeLyricsFromDataframe(maleSongs_df, femaleSongs_df)