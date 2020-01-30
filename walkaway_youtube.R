library(tuber)
library(tidyverse)
library(stringr)
library(purrr)

#verify the application 
client_id <- "802849702091-37hpnvutvcgm750qrgv5qseuehu60s6s.apps.googleusercontent.com"
client_secret <- "aT5PWKlXuLtbsgRdGn_txICB"
yt_oauth(client_id, client_secret)

#get the youtube ID for the walkaway testimonials
walkaway_playlist_id <- stringr::str_split(
                        string = "https://www.youtube.com/playlist?list=PLjbi7adhMoizwZTWPCAb2ELVNaFLzx79b",
                        pattern = "=", 
                        n = 2,
                        simplify = TRUE)[ , 2]

#get all the raw videos 
walkaway_raw <- tuber::get_playlist_items(filter = c(playlist_id = walkaway_playlist_id), 
                                                   max_results = 400,
                                                   part = "contentDetails")

#get the video ids 
video_ids <- base::as.vector(walkaway_raw$contentDetails.videoId)

#get the list of caption tracks needed
get_caption_tracks <- function(vid_id) {
 tuber::list_caption_tracks(video_id = vid_id) 
}
 

#make a function to get the transcripts of the videos 
get_transcripts <- function(vid_id) {
  tuber::get_captions(video_id= vid_id)
}

get_all_stats <- function(id) {
  tuber::get_stats(video_id = id)
} 

#use purr to get the captions for all of the videos 
walkaway_stats <- purrr::map_df(.x = video_ids,
                                .f = get_all_stats)

walkaway_caption_tracks <- purrr::map_df(.x = walkaway_stats$id[1],
                                         .f = get_caption_tracks)
