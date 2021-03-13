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
    cat("That's not a direction")
  } else {
    #check if can go in direction in current room
    if(room_current[[paste("cango", toupper(x), sep = "")]] == 1){
      #set coords to coords_new, change room and look in new room
      ycoords <- ycoords_new
      xcoords <- xcoords_new
      current_room <- subset(room_list, room_list$ycoords == ycoords & room_list$xcoords == xcoords)
      look(current_room)
    } else {
      cat("You cannot go that way.")
    }
  }
}

####Look####
#To get 