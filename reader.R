# Arquivo reader para ler os dataframes
if (!file.exists("data")) {
  dir.create("data")
  
  # Downloading the file
  fileUrl <- "http://files.grouplens.org/datasets/movielens/ml-20m.zip"
  download.file(fileUrl, destfile = "./data/ml-20m.zip") #usuários Mac method = "curl" em download.file para https
  
  # Unziping the file
  #exdir defines the directory to extract files to. It will be created
  #if not already available. If you don't set exdir, unzip will 
  #just unzip it to your current working directory.
  zipFileIn <- "./data/ml-20m.zip"
  zipFileOut <- "./data/"
  unzip(zipFileIn,exdir=zipFileOut)
  
}

# Reading the csv files
ratings <- read_csv("./data/ml-20m/ratings.csv")
movies <- read_csv("./data/ml-20m/movies.csv")



