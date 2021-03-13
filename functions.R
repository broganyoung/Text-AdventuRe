####Load Libraries####

####Load References####
room_list <- read.csv(paste(getwd(), "/references/room_list.csv", sep = ""))
look_list <- read.csv(paste(getwd(), "/references/look_list.csv", sep = ""))
item_list <- read.csv(paste(getwd(), "/references/item_list.csv", sep = ""))
actor_list <- read.csv(paste(getwd(), "/references/actor_list.csv", sep = ""))
text_list <- read.csv(paste(getwd(), "/references/text_list.csv", sep = ""))

####Setup####
room_current <- subset(room_list, room_list$RoomID == "Test5")
ycoords <- 0
xcoords <- 0
inventory <- data.frame()

####Look####
#To get description
look <- function(x){
  #If no object supplied, give room description
  if(missingArg(x)){
    RoomDescription <- room_current$RoomDescription
    extras <- c()
    item_list_current <- subset(item_list, item_list$Location == room_current$RoomID)
    if(nrow(item_list_current) > 0){
      for(a in 1:nrow(item_list_current)){
        extras <- (paste(extras, "You see a ", item_list_current$ItemName[a], "\n", sep = ""))
      }
    }
    actor_list_current <- subset(actor_list, actor_list$Location == room_current$RoomID)
    if(nrow(actor_list_current) > 0){
      for(a in 1:nrow(actor_list_current)){
        extras <- (paste(extras, "You see a ", actor_list_current$ActorName[a], "\n", sep = ""))
      }
    }
    RoomDescription <- paste(RoomDescription, "\n", extras, sep = "")
    cat(RoomDescription)
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
      #If just one, then cat the description
      if(nrow(look_current) == 1){
        if(look_current$Type == "item"){
          look_current <- subset(item_list, item_list$ItemID == look_current$ID)
          cat(look_current$ItemDescription)
        } else {
          look_current <- subset(actor_list, actor_list$ActorID == look_current$ID)
          cat(look_current$ActorDescription)
        }
      }
    }
  }
}


####Get####
#To pickup objects and put in inventory
get <- function(x){
  #format x
  x <- tolower(x)
  #check if item exists in room
  room_items <- subset(item_list, item_list$Location == room_current$RoomID)
  room_items <- subset(room_items, room_items$PossibleName == x)
  if(nrow(room_items) == 0){
    cat("You can't see this.")
  } else {
    #if item exists in room, add to inventory and change location
    room_items$Location <- NULL
    inventory <<- rbind(inventory, cbind(room_items))
    item_list$Location[[item_list$ItemID == room_items$ItemID[1]]] <<- "inventory"
    look_list$Location[[look_list$ItemID == room_items$ItemID[1]]] <<- "inventory"
    cat(paste("You pick up the "), x, sep = "")
  }
}

####Go####
#To move the player
go <- function(x){
  if(missingArg(x)){
    cat("Go where?")
  } else {
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
        direction_text <- subset(text_list, text_list$TextID == x)
        direction_text <- direction_text$Text
        cat(paste("You go ", direction_text, "\n", sep = ""))
        look()
      } else {
        cat("You cannot go that way.")
      }
    }
  }
}

#generate prompt text
prompt_text <- subset(prompt_list$Prompt_Text, prompt_list$Prompt_Number == sample(1:10, 1))
#ask player what they want to do
command <- readline(prompt = paste(prompt_text, " ", sep = ""))
if(str_detect(command, "go") == TRUE | str_detect(command, "move") == TRUE){
  command <- gsub("go", "", command)
  command <- gsub("move", "", command)
  command <- gsub("to", "", command)
  command <- gsub("the", "", command)
  str_trim(command)
  go(command)
}
if(str_detect(command, "look") == TRUE){
  command <- gsub("look", "", command)
  command <- gsub("at", "", command)
  command <- gsub("the", "", command)
  str_trim(command)
  if(command == ""){
    look()
  } else {
    look(command)
  }
}
if(str_detect(command, "get") == TRUE | str_detect(command, "take") == TRUE | str_detect(command, "pick up") == TRUE){
  command <- gsub("get", "", command)
  command <- gsub("take", "", command)
  command <- gsub("pick up", "", command)
  command <- gsub("the", "", command)
  str_trim(command)
  go(command)
}