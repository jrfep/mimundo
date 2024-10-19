require(dplyr)
#library(FlickrAPI)
#readRenviron(here::here(".Renviron")) # read the API key

filter_ggl_photo <- 
    function(data, description = NULL,
        album_name = NULL, nfotos = NULL) {
   outdata <- data
   if (!is.null(description)) {
    outdata <- filter(outdata,
        grepl(description, mediaItems.description))
   }
   if (!is.null(album_name)) {
    outdata <- filter(outdata,
        album %in% album_name)
   }
   if (!is.null(nfotos)) {
    outdata <- slice_sample(outdata, n = nfotos)
   }
   return(outdata)
}

filter_inat_photo <-     
    function(inat_obs_db = "iNaturalist-obs-NeoMapas.rds", place = NULL) {
        file_name <- here::here("data", inat_obs_db)
        if (!file.exists(file_name)) {
            error("File with information from iNaturalist is missing!")
        }
        outdata <- readRDS(file = file_name) 
        if (!is.null(place)) {
            outdata <- filter(outdata,
                grepl(place, place_guess))
        }
        return(outdata)
    }


filter_flickr_photo <-     
    function(flickr_photos_db = "flickr-photos.rds", 
        find = NULL, seleccion = NULL) {
        file_name <- here::here("data", flickr_photos_db)
        if (!file.exists(file_name)) {
            error("File with information from Flickr is missing!")
        }
        outdata <- readRDS(file = file_name) 
        if (!is.null(seleccion)) {
            outdata <- filter(outdata, title %in% seleccion)
        }
        if (!is.null(find)) {
            outdata <- filter(outdata, grepl(find, title))
        }
        return(outdata)
    }


pull_inat_paths_captions <-
    function(data, howmany=NULL) {
        if (!is.null(howmany))
            data <- slice_sample(data, n = howmany) 
        caption_str <- "<strong>%s</strong> (<emph>%s</emph>) by <a href='%s' target='iNat'>%s @ iNaturalist</a>"
        paths <- pull(data, image_url)
        captions <- data |>
            transmute(caption = 
                sprintf(caption_str, species_guess, scientific_name, url, user_name)) |> 
                pull(caption)
        return(list(paths=paths, captions=captions))
    }
pull_flickr_preview <-
    function(data, howmany = 1) {
        data <- slice_sample(data, n = howmany) 
        # photos_info <- getPhotoInfo(photo_id = data$id, output = "url")
        path_str <- "<img src='%s' width='%s'  height='%s' class='preview-image'>"
        caption_str <- "<strong>%s</strong> by <a href='https://www.flickr.com/photos/%s/%s' target='flickr'>%s @ flickr</a>"
        paths <- data |>
            transmute(path = 
                sprintf(path_str, url_m, width_m, height_m)) |> 
                pull(path)
        captions <- data |>
            transmute(caption = 
                sprintf(caption_str, title, owner, id, ownername)) |> 
                pull(caption)
        return(list(paths=paths, captions=captions))
    }