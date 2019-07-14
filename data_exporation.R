# load data
setwd("/media/sergio/0C5EC1615EC14464/chimp_and_see/data/raw")

coarse_classifications <- read.csv2('2019-07-07_chimp_classifications.csv', stringsAsFactors = F, sep = ",", header = TRUE)
fine_classifications <- read.csv2('2019-07-07_chimp_discussions.csv', stringsAsFactors = F, sep = ",", header = TRUE)

#  data
setwd("/media/sergio/0C5EC1615EC14464/chimp_and_see/data/raw")

# explore coarse classifications
unique(coarse_classifications$animal)
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
unique(fine_classes) # 5,500 classes
