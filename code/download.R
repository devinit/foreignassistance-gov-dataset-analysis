options(timeout=1e9)
download_foreignassistance = function(location="large_input/us_foreign_aid_complete.csv"){
  if(!file.exists(location)){
    download.file(
      "https://s3.amazonaws.com/files.explorer.devtechlab.com/us_foreign_aid_complete.csv",
      location
    )
  }
  return(location)
}
