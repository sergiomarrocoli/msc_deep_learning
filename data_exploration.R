# install.packages("dplyr")
# install.packages("tidyr")
# install.packages('datetime')
library(dplyr)
library(tidyr)
library(datetime)


# load data
# setwd("/media/sergio/0C5EC1615EC14464/chimp_and_see/data/raw")
setwd("D:/R/msc")

# ubuntu
setwd("/media/sergio/0C5EC1615EC14464/chimp_and_see/data/raw")
coarse_classifications <- read.csv2('2019-07-07_chimp_classifications.csv', stringsAsFactors = F, sep = ",", header = TRUE)
fine_classifications <- read.csv2('2019-07-07_chimp_discussions.csv', stringsAsFactors = F, sep = ",", header = TRUE)

# windows
setwd("D:/Documents/msc_deep_learning/Data/")
coarse_classifications <- read.csv2('2019-06-30_chimp_classifications.csv', stringsAsFactors = F, sep = ",", header = TRUE)
# fine_classifications <- read.csv2('2019-07-07_chimp_discussions.csv', stringsAsFactors = F, sep = ",", header = TRUE)



# explore coarse classifications
species_classifications <- unique(coarse_classifications$animal)
species_classifications <- species_classifications[-(species_classifications == "")]
# [1] ""                    "red river hog"       "large ungulate"      "human"               "small grey duiker"  
# [6] "chimpanzee"          "other (non-primate)" "red duiker"          "dark duiker"         "bird"               
# [11] "small antelope"      "other (primate)"     "rodent"              "elephant"            "gorilla"            
# [16] "hippopotamus"        "small cat"           "hyena"               "warthog"             "leopard"            
# [21] "zebra duiker"        "reptile"             "giant forest hog"    "pangolin"            "Jentik's duiker"    
# [26] "cattle"              "porcupine"           "forest buffalo"      "wild dog"            "lion"               
# [31] "Jentink's duiker" 

data.frame(table(coarse_classifications$animal))
coarse_sp_fq <- data.frame(table(coarse_classifications$animal))
str(coarse_sp_fq)
sum(coarse_sp_fq$Freq)/4
str(coarse_sp_fq)
colnames(coarse_sp_fq) <- c("Species", "Count")
coarse_sp_fq[order(-coarse_sp_fq$Count),]

#                Species   Count
# 19     other (primate)  346162
# 28   small grey duiker  313770
# 22          red duiker  262320
# 11               human  248262
# 4           chimpanzee  169680
# 5          dark duiker   96032
# 18 other (non-primate)   83771
# 26      small antelope   73887
# 25              rodent   69580
# 2                 bird   43663
# 29             warthog   40203
# 23       red river hog   36966
# 6             elephant   32719
# 14    Jentink's duiker   18879
# 21           porcupine   18283
# 8     giant forest hog   12823
# 15      large ungulate   10924
# 3               cattle    8676
# 27           small cat    8577
# 31        zebra duiker    8561
# 9              gorilla    7607
# 16             leopard    3968
# 7       forest buffalo    3484
# 20            pangolin    3335
# 10        hippopotamus    3207
# 30            wild dog    3112
# 12               hyena    2643
# 24             reptile    1175
# 13     Jentik's duiker     838
# 17                lion     654


str(fine_classifications)
# data stores as ; seperated tags atatched to video: "coolsilence:1;infant:1;mangabey:1;sooty:1" 

fine_classes <- unique(unlist(strsplit(fine_classifications$tags, split = ";")))

# need to remove start of some tags (1_) and quantity (:1)
fine_classes <- gsub(":.*", "", fine_classes)
fine_classes <- gsub("*_", "", fine_classes)
unique(fine_classes) # 5,500 classes - ignore for now


# lists of videos of each species

# split into day and night

# classify by hour
coarse_classifications$hour <- sapply(coarse_classifications$created_at, function(x){as.integer(strsplit(strsplit(x, " ")[[1]][[2]], ":")[[1]][[1]])})

day_videos <- coarse_classifications[coarse_classifications$hour > 8 & coarse_classifications$hour < 16,]
night_videos <- coarse_classifications[coarse_classifications$hour < 4 | coarse_classifications$hour > 20,]

nrow(day_videos) # 1039292
nrow(night_videos) # 1033975
nrow(coarse_classifications) # 3821088


# split into training and test

set.seed(1)
# how?
# iterate over species list
# select by species
# sample to training
# sample to test
assign_to_test_and_training <- function(species_col, test_prop = 0.3){
  # species_list <- species_col[-(species_col == "")]
  species_list <- unique(species_col)
  species_list <- species_list[-(species_list == "")]
  assignment <- rep("Training", length(species_col))  
  for(species in species_list){
    species_index <- which(species_col == species)
    test_index <- sample(species_index, test_prop * length(species_index))
    assignment[test_index] = "Test"    
  }
  return(assignment)
}

day_videos$Training <- assign_to_test_and_training(day_videos$animal)
table(x)





# get classes for test videos
setwd("D:/R/msc")
test_videos <- read.csv("test_videos.csv", stringsAsFactors = FALSE)
table(test_videos$zamba_classification)
test_videos$filename <- gsub("[.].*", "", test_videos$filename)
files <- test_videos$filename

human_classifications <- coarse_classifications[coarse_classifications$subject_id %in% files, ]
human_classifications <- human_classifications[c("subject_id", "animal", "number")]
colnames(human_classifications)


# get most common human classifications
human_classifications_species <- human_classifications %>%
  select(-number) %>%
  group_by(subject_id) %>%
  slice(which.max(table(animal)))
  
human_classifications_number <- human_classifications %>%
  select(-animal) %>%
  group_by(subject_id) %>%
  slice(which.max(table(number)))

# join to test_videos
colnames(human_classifications_species) <- c("filename", "human_classification_species")
colnames(human_classifications_number) <- c("filename", "human_classification_number")

test_videos <- test_videos %>%
  left_join(human_classifications_species, by = "filename")

test_videos <- test_videos %>%
  left_join(human_classifications_number, by = "filename")


# add my algo classification
yolo_classifications<- read.csv("video_predictions_20191221.csv", stringsAsFactors = FALSE)


test_videos <- test_videos %>%
  left_join(yolo_classifications, by = "filename")

# rename columns
colnames(test_videos) <- c("filename", "zamba_species",
                           "me_species", "me_number",
                           "cit_sci_species", "cit_sci_number", 
                           "yolo_species", "yolo_number")

# replace NAs with blanks where necessary
test_videos[is.na(test_videos$yolo_species), 7] <- "blank"
test_videos[test_videos$yolo_species == "blank", 8] <- 1


# replace NAs with blanks where necessary
test_videos[test_videos$cit_sci_species == "", 5] <- "blank"
test_videos[test_videos$cit_sci_species == "blank", 6] <- 1
# make 5+ 5
test_videos[test_videos$cit_sci_number == "5+", 6] <- 5
test_videos$cit_sci_number <- as.integer(test_videos$cit_sci_number)


# plots and correlations of numbers
plot(test_videos$me_number, test_videos$yolo_number)
cor.test(test_videos$me_number, test_videos$yolo_number)


plot(test_videos$me_number, test_videos$cit_sci_number)
cor.test(test_videos$me_number, test_videos$cit_sci_number)


# get totals of each species for me
yolo_totals <- test_videos[7:8] %>%
  group_by(yolo_species) %>%
  summarise("yolo_total" = sum(yolo_number))

me_totals <- test_videos[3:4] %>%
  group_by(me_species) %>%
  summarise("me_total" = sum(me_number))

colnames(yolo_totals)[1] <- "species"
colnames(me_totals)[1] <- "species"

me_vs_yolo_totals <- full_join(me_totals, yolo_totals, by = "species")

me_vs_yolo_totals[is.na(me_vs_yolo_totals)] <- 0

plot(me_vs_yolo_totals$me_total, me_vs_yolo_totals$yolo_total, 
     xlim = c(0, 250), ylim = c(0, 250),
     xlab = "My total", ylab = "Yolov3 total")
cor.test(me_vs_yolo_totals$me_total, me_vs_yolo_totals$yolo_total)
me_yolo_model <- lm(me_vs_yolo_totals$yolo_total ~ me_vs_yolo_totals$me_total)
abline(me_yolo_model)

#########################################################################################

# ZAMBA DATA PREP

# zamba vs
test_videos_zamba_categories <- test_videos
test_videos_zamba_categories[test_videos_zamba_categories == "red duiker"] <- "duiker"
test_videos_zamba_categories[test_videos_zamba_categories == "small grey duiker"] <- "duiker"
test_videos_zamba_categories[test_videos_zamba_categories == "red river hog"] <- "hog"
test_videos_zamba_categories[test_videos_zamba_categories == "sooty mangabey"] <- "other (primate)"
test_videos_zamba_categories[test_videos_zamba_categories == "black and white colobus"] <- "other (primate)"
test_videos_zamba_categories[test_videos_zamba_categories == "red colobus"] <- "other (primate)"
test_videos_zamba_categories[test_videos_zamba_categories == "baboon"] <- "other (primate)"

# get totals of each species for me
yolo_count <- test_videos_zamba_categories[7:8] %>%
  group_by(yolo_species) %>%
  summarise("yolo_count" = n())

me_count <- test_videos_zamba_categories[3:4] %>%
  group_by(me_species) %>%
  summarise("me_count" = n())

zamba_count <- test_videos_zamba_categories[2] %>%
  group_by(zamba_species) %>%
  summarise("zamba_count" = n())

colnames(yolo_count)[1] <- "species"
colnames(me_count)[1] <- "species"
colnames(zamba_count)[1] <- "species"

zamba_vs_all_count <- full_join(me_count, yolo_count, by = "species")
zamba_vs_all_count <- full_join(zamba_vs_all_count, zamba_count, by = "species")

zamba_vs_all_count[is.na(zamba_vs_all_count)] <- 0

# plots
plot(zamba_vs_all_count$me_count, zamba_vs_all_count$yolo_count,
     xlab = "My count", ylab = "Zamba/Yolov3 count")
points(zamba_vs_all_count$me_count, zamba_vs_all_count$zamba_count, pch = 19)

zamba_me_model <- lm(zamba_vs_all_count$zamba_count ~ zamba_vs_all_count$me_count)
abline(zamba_me_model)

yolo_me_model <- lm(zamba_vs_all_count$yolo_count ~ zamba_vs_all_count$me_count)
abline(yolo_me_model, lty = 3)
