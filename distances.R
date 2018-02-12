# Calculate distances and neighbours 
###################################


# Load quality ratings
# ////////////////////

library(rio)
library(dplyr)
library(sp)
library(rgeos)
library(geosphere)
library(RASS)
library(RANN)
library(tidyr)


    ratings = import("/Users/Personas/My Cloud/ch2_phd_may2017/chapter2/quality/agglomeration/data/processed/geo_ratings.csv")
    ratings_extended = import("/Users/Personas/My Cloud/ch2/paper/quality/agglomeration/data/processed/geo_ratings_extended.csv")

# South East
# //////////

  se =  ratings_extended %>%  filter(Location.Region == "South East")

# I am interested in the geographic position regardless of other information
se_geo = se %>% select(Location.ID, Location.Name, Location.Post.Code, X, Y) %>% rename(long = X, lat = Y) %>% 
  unique() %>% arrange(Location.ID, Location.Name)


# Distance matrix
dist_matrix <- select(se_geo, long, lat) %>% 
  distm %>% `/`(1000)%>%  as.dist


# rename the rows with the postcodes 

library(reshape2)

# df gives the distance between the roows and columns of the dataset. 

    df <- melt(as.matrix(dist_matrix), varnames = c("row", "col"))
    head(df)

# name the rows and the columns.  The idea is to add more information to the distance between rows and columns. 
# I identify which the rows and the columns are linking the dataset 

# create "row.1" to identify first the rows according to their number and their postcode
    s = se_geo %>% mutate(row.1 = 1:nrow(se_geo)) %>% select(Location.ID:Location.Post.Code, row.1)

# I link the rows according to their number in the first data frame.

    df = left_join(df, s, by = c("row"= "row.1"))

# As the matrix is symmetric, columns must follow the same order as rows. 
    df = left_join(df, s, by = c("col"= "row.1"))

# Note: output gives information regarding the distance between every pair of care home



# clean and rename variables in data frame

    df = df %>% 
      select(from.care.home = Location.Name.x, to.care.home= Location.Name.y,
             from.ID = Location.ID.x, to.ID = Location.ID.y, 
             from.postcode = Location.Post.Code.x, to.postcode = Location.Post.Code.y, distance = value) %>%
      filter(distance >0)


##############
# NEIGHBOURS #
##############

# Radius of 10 KM 
#/////////////////

df = df %>% mutate(radius10 = ifelse(distance <=10, "1", "0")) 

# those that are close
close = df %>% filter(radius10 == "1")

close = close %>% select(from.ID:distance) %>% unique()

# calculate neighbours and mean distance
close = close %>% group_by(from.ID) %>% mutate(neighbours_10 = n(),
                                               mean_distance10 = mean(distance)) %>% arrange(from.ID, desc(neighbours_10), distance)


# 5 closest neighbours 
close = close %>%
  group_by(from.ID) %>%
  mutate(knn10 = ifelse(row_number() %in% c(1:5), "1", "0"))



test = left_join(df, close, by = c("from.ID" ,"to.ID" , "from.postcode", "to.postcode", "distance" ))


# recode some variables

test = test %>% mutate(radius10 = ifelse(is.na(radius10), 0, radius10),
                       neighbours_10 = ifelse(is.na(neighbours_10), 0, neighbours_10),
                       knn10 = ifelse(is.na(knn10), 0, knn10), 
                       mean_distance10 = ifelse(is.na(mean_distance10), 0, mean_distance10))




# label variables

library(Hmisc)


var.labels <- c( from.care.home = "name care home origin",
                 to.care.home = "name care home destiny",
                 from.ID = "ID care home origin", 
                 to.ID = "ID care home destiny",
                 from.postcode = "postcode origin",
                 to.postcode = "postcode destiny",
                 distance = "distance between postcodes",
                 radius10 =  "within 10km radius; 1 - yes, 0 - no",
                 neighbours_10 = "number of neighbours within a 10Km radius for care home origin",
                 mean_distance10 ="mean distance between neighbours within 10Km radius for care home origin", 
                 knn10 = "close neighbour; 1 - yes, 0 - no")

# apply the labels
test <- Hmisc::upData(test, labels = var.labels) 

head(test, 200)

############################
# OUTCOMES: QUALITY RATING # 
############################


# origin care homes 

ratings_se = ratings_extended %>% select(Location.ID, Location.Name, Location.Post.Code:oslaua, -initial) %>% 
  mutate_each(funs(as.Date), date) %>%
  group_by(Location.ID) %>% 
  arrange(Location.ID, date) %>% 
  unique() %>% 
  filter(Location.Region == "South East")


# see inspections - to determine time waves

check = ratings_se %>% group_by(Location.ID) %>% filter(n()>1)
check = check %>% group_by(Location.ID) %>% mutate(inspection = date - lag(date))


mean(check$inspection,na.rm=TRUE) # average duration of inspections 

# create waves 

  ratings_se = ratings_se %>% mutate(wave = ifelse(date < "2015-11-01", 1, ifelse(
    date >= "2015-11-01", 2, 3)))
  
  
# neighbours - those 5 that are closer within 10km
  
  neigh =  test %>% filter(knn10 == 1)

  final_data = left_join(ratings_se, neigh, by = c("Location.ID" = "from.ID"))

  
#########
# PEERS
#########
  
  # select information referred to the neighbours
  
  neigh_wide = neigh %>% select(from.care.home:to.ID)
  
  se_wide = se %>% select(Location.ID, Location.Name, Overall.Rating, date) %>% mutate(wave = ifelse(date < "2015-11-01", 1, ifelse(
    date >= "2015-11-01", 2, 3)))
  
  # wave 1
  # -------
  
  se_wide_one = se_wide %>% filter(wave == 1)
  
  # link peers' information with their respective outcome in wave 1
  
  neigh_outcome = left_join(neigh_wide, se_wide_one, by = c("to.ID"=  "Location.ID")) %>% select(-Location.Name) %>% unique()
  
  
  
 

  test = neigh_outcome %>%  select(from.ID, to.ID, Overall.Rating) %>% mutate(good = ifelse(Overall.Rating == "Good", 1, 0                                                                                        ),
                                                                              inadequate = ifelse(Overall.Rating == "Inadequate", 1, 0),
                                                                              outstanding = ifelse(Overall.Rating == "Outstanding", 1, 0),
                                                                              improvement = ifelse(Overall.Rating == "Requires improvement", 1, 0))
 
   # missing observations are because these care homes were not inspected during this wave - I assume no rating --> 0
  
    test1 =  test %>% complete(from.ID, fill = list(good = 0, inadequate = 0, outstanding = 0, improvement = 0))
  
    # get the means of the peers for each category 
    test1 = test1 %>% group_by(from.ID) %>% mutate(peer.good = mean(good, round = 2),
                                                 
                                               peer.inadequate = mean(inadequate, round = 2),
                                               
                                               peer.outstanding = mean(outstanding, round = 2),
                                               
                                               peer.improvement = mean(improvement,  round = 2)) %>% 
                                               as.data.frame()
  
  
  
  peer_quality_one= test1 %>% select(from.ID, to.ID, peer.good:peer.improvement) %>% 
    gather(peer.quality, mean.peer, peer.good:peer.improvement) %>%
    select(from.ID, peer.quality, mean.peer) %>% mutate(wave = 1) %>% unique()
  
  
  # wave 2 
  # ---------
  
  se_wide_two = se_wide %>% filter(wave == 2)
  
  
  # link peers' information with their respective outcome in wave 1
  
  rm(neigh_outcome)

  neigh_outcome = left_join(neigh_wide, se_wide_two, by = c("to.ID"=  "Location.ID")) %>% select(-Location.Name) %>% unique()
  
  test = neigh_outcome %>%  select(from.ID, to.ID, Overall.Rating) %>% mutate(good = ifelse(Overall.Rating == "Good", 1, 0                                                                                        ),
                                                                              inadequate = ifelse(Overall.Rating == "Inadequate", 1, 0),
                                                                              outstanding = ifelse(Overall.Rating == "Outstanding", 1, 0),
                                                                              improvement = ifelse(Overall.Rating == "Requires improvement", 1, 0))
  
  # missing observations are because these care homes were not inspected during this wave - I assume no rating --> 0
  
  test1 =  test %>% complete(from.ID, fill = list(good = 0, inadequate = 0, outstanding = 0, improvement = 0))
  
  # get the means of the peers for each category 
  test1 = test1 %>% group_by(from.ID) %>% mutate(peer.good = mean(good, round = 2),
                                                 
                                                 peer.inadequate = mean(inadequate, round = 2),
                                                 
                                                 peer.outstanding = mean(outstanding, round = 2),
                                                 
                                                 peer.improvement = mean(improvement,  round = 2)) %>% 
    as.data.frame()
  
  
  
  peer_quality_two= test1 %>% select(from.ID, to.ID, peer.good:peer.improvement) %>% 
    gather(peer.quality, mean.peer, peer.good:peer.improvement) %>%
    select(from.ID, peer.quality, mean.peer) %>% mutate(wave = 2) %>% unique()
  
  
  peers = full_join(peer_quality_one, peer_quality_two)
  
  
  # summary statistics peers 
  
  peer_sum = peers  %>% group_by(peer.quality, wave) %>% summarise_each(funs(mean)) %>% select(-from.ID)
  
  p <- ggplot(peer_sum, aes(peer.quality, mean.peer))
  p + geom_boxplot() 

  #####################
  # OUTCOME OF CARE HOMES
  #####################
  
  outcome_ids = ratings_extended %>% select(Location.ID, Overall.Rating, date, Location.Region) %>% filter(Location.Region == "South East") 
  
  # wave
  
  outcome_ids = outcome_ids %>% mutate_each(funs(as.Date), date) %>% mutate(wave = ifelse(date < "2015-11-01", 1, ifelse(
    date >= "2015-11-01", 2, 3))) %>% select(-Location.Region) %>% arrange(Location.ID, date) %>% unique()
  

  # create dummy with all the results 
  

 library(mlr)
 
 ex_dummies = createDummyFeatures(outcome_ids$Overall.Rating, cols = "rating")
 
 outcomes_ids_test = cbind(outcome_ids, ex_dummies) %>% mutate_each(funs(as.factor), Location.ID, Overall.Rating, Good, Inadequate, Outstanding, `Requires improvement`)
 

 
 
 # link information about location and its peers 
 
    # extend information on peers
   
   peers_wide = tidyr::spread(peers, peer.quality, mean.peer)
 
   test = left_join(peers_wide, outcomes_ids_test, by = c("from.ID" = "Location.ID", "wave" = "wave"))
 
 
                           
                                             
  