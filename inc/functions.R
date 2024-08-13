require(dplyr)

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