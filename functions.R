####Load References####
room_list <- read.csv(paste(getwd(), "/references/room_list.csv", sep = ""))
look_list <- read.csv(paste(getwd(), "/references/look_list.csv", sep = ""))

####Look####
#To get description
look <- function(x){
  #If no object supplied, give room description
  if(missingArg(x)){
    cat(room_current$RoomDescription)
  } else {
    #Format input
    x <- tolower(x)
    #Get room objects and inventory objects
    look_current <- subset(look_list, look_list$Location == room_current$RoomID | look_list$Location == "inventory")
    #Check room has any objects
    if(nrow(look_current) == 0){
      cat("You cannot see this.")
    } else {
      #If room has objects, check input is in there
      look_current <- subset(look_current, look_current$PossibleName == x)
      if(nrow(look_current) == 0){
        cat("You cannot see this.")
      }
      #If there is more than one match then ask player again
      if(nrow(look_current) > 1){
        cat("Which one?")
      }
      if(nrow(look_current) == 1){
        if(look_current$type == "item"){
          look_current <- subset(item_list, item_list$ItemID == look_current$ID)
          cat(look_current$ItemDescription)
        }
        if(look_current$type == "actor"){
          look_current <- subset(actor_list, actor_list$ItemID == look_current$ID)
          cat(look_current$ActorDescription)
        }
      }
    }
  }
}

####Go####
#To move the player
go <- function(x){
  #reset valid direction check
  check <- 0
  #format input
  x <- gsub("-", " ", x)
  x <- strsplit(x, " ")
  x <- paste(substr(x[[1]][1], 1, 1), substr(x[[1]][2], 1, 1), sep = "")
  x <- tolower(x)
  x <- gsub("na", "", x)
  #reset new coords
  ycoords_new <- 0
  xcoords_new <- 0
  #check input and set new coords
  if(x == "n"){
    ycoords_new <- ycoords + 1
    xcoords_new <- xcoords
    check <- 1
  }
  if(x == "s"){
    ycoords_new <- ycoords - 1
    xcoords_new <- xcoords
    check <- 1
  }
  if(x == "e"){
    xcoords_new <- xcoords + 1
    ycoords_new <- ycoords
    check <- 1
  }
  if(x == "w"){
    xcoords_new <- xcoords - 1
    ycoords_new <- ycoords
    check <- 1
  }
  if(x == "ne"){
    ycoords_new <- ycoords + 1
    xcoords_new <- xcoords + 1
    check <- 1
  }
  if(x == "nw"){
    ycoords_new <- ycoords + 1
    xcoords_new <- xcoords - 1
    check <- 1
  }
  if(x == "se"){
    ycoords_new <- ycoords - 1
    xcoords_new <- xcoords + 1
    check <- 1
  }
  if(x == "sw"){
    ycoords_new <- ycoords - 1
    xcoords_new <- xcoords - 1
    check <- 1
  }
  #check if input was valid
  if(check == 0){
    cat("That's not a direction.")
  } else {
    #check if can go in direction in current room
    if(room_current[[paste("cango", toupper(x), sep = "")]] == 1){
      #set coords to coords_new, change room and look in new room
      ycoords <<- ycoords_new
      xcoords <<- xcoords_new
      room_current <<- subset(room_list, room_list$ycoords == ycoords_new & room_list$xcoords == xcoords_new)
      look()
    } else {
      cat("You cannot go that way.")
    }
  }
}
