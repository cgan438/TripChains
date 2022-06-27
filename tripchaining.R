
trip.chains.per.person <- function(person){
  # this function calculates the legs of each trip a person goes on

  trips <- tr.df[tr.df$key == person,]

  # number of legs this person goes on

  n <- nrow(trips)
  trip.num <- 1

  if (n > 0){
    trips$trip.num <- 1
  }
  else
  {
    return(trips)
  }

  if (n > 1){
    # need find faster than this for loop
    for (i in 2:n){

      # need to accommodate days of the week differing
      trip.leave <- trips[i,"trleaveh"] + 24 * (trips[i,"Daywk"] != trips[i-1,"Daywk"])
      prev.trip.arrive <- trips[i-1,"trarrivh"]

      # if goes home, to work or to school, end the trip chain
      if ((trips[i-1, "tractiv"] == 2) || (trips[i-1, "tractiv"] == 3) || (trips[i-1, "tractiv"] == 7)){
        trip.num <- trip.num + 1
      }
      else if ((trip.leave - prev.trip.arrive > 1.5) & (trips[i-1, "tractiv"] != 1)){
        trip.num <- trip.num + 1
      }

      trips[i, "trip.num"] <- trip.num

    }

  }

  return (trips)
}




trip.chain.main.purpose <- function(trip){
  # this function finds the "main" purpose for a trip chain

  n <- nrow(trip)

  home.duration <- 0#2, 15
  work.duration <- 0#3,6,7
  maintain.duration <- 0#4,8,
  leisure.duration <- 0#5,11,14
  accompany.duration <- 0#9,10,12
  mode.change.duration <- 0#1

  for (i in 1:n){

    purpose <- trip[i, "tractiv"]

    if ((purpose == 2) | (purpose == 15)){
      home.duration <- home.duration + trip[i, "duration"]
    }

    if ((purpose == 3) | (purpose == 6) | (purpose == 7)){
      work.duration <- work.duration + trip[i, "duration"]
    }

    if ((purpose == 4) | (purpose == 8)){
      maintain.duration <- maintain.duration + trip[i, "duration"]
    }

    if ((purpose == 5) | (purpose == 11) | (purpose == 14)){
      leisure.duration <- leisure.duration + trip[i, "duration"]
    }

    if ((purpose == 9) | (purpose == 10) | (purpose == 12)){
      accompany.duration <- accompany.duration + trip[i, "duration"]
    }

    if (purpose == 1){
      mode.change.duration <- mode.change.duration + trip[i, "duration"]
    }

  }

  # if going to work in trip chain then main purpose

  if(work.duration > 0){
    return(3)
  }

  # if the start of trip chain is home, then main purpose can not be returning home
  if (trip[1, "tripstart_activ"] == 2){
    home.duration <- 0
  }

  #just added this line
  mode.change.duration <- ifelse(mode.change.duration > 0, 0.01, 0)

  durations <- c(mode.change.duration, home.duration, work.duration, maintain.duration, leisure.duration, accompany.duration)

  main.purpose <- which.max(durations)
  # 1 is mode change, 2 is home, 3 is work, 4 is maintain, 5 is leisure, 6 is accompany



  return(main.purpose)
}




trip.chain.main.mode <- function(trip){
  # this function finds the "main" mode for a trip chain

  n <- nrow(trip)

  #trmode1
  car.driving <- 0
  car.passenger <- 0

  active.modes <- 0
  pt.modes <- 0

  other.modes <- 0



  for (i in 1:n){

    mode <- trip[i, "trmode"]

    if (is.na(mode)){
      return(1)
    }

    if (mode == 1){
      if(trip[i, "Newmode"] == 2 & !is.na(trip[i, "Newmode"])){
        car.passenger = car.passenger + trip[i, "bestdist"]
      }
      else{
        car.driving = car.driving + trip[i, "bestdist"]
      }

    } else if((mode == 2) | (mode == 4)){

      active.modes <- active.modes + trip[i, "bestdist"]

    } else if ((mode == 3) | (mode == 5) | (mode == 6)){

      pt.modes <- pt.modes + trip[i, "bestdist"]

    }
    else{

      other.modes <- other.modes + trip[i, "bestdist"]

    }

  }

  modes <- c(car.driving, car.passenger, active.modes, pt.modes, other.modes)

  main.mode <- which.max(modes)
  # 1 is car.driving, 2 is car.passenger, 3 is active.mode, 4 is pt.mode, 5 is other.modes

  return(main.mode)
}



construct.trip.chains <- function(person){

  key.id <- pe.df[person, "key"]

  # step 1- create the trip chains
  all.chains <- trip.chains.per.person(key.id)
  chains <- split(all.chains, all.chains$trip.num)


  n = length(chains)

  key <- numeric(n)
  mesh.id <- numeric(n)
  region <- character(n)
  trip.weights <- numeric(n)


  gender <- character(n)
  area <- character(n)
  age <- numeric(n)


  employ <- character(n)
  child.status <- character(n)

  num.segments <- numeric(n)
  duration <- numeric(n)
  trip.length <- numeric(n)

  pt <- numeric(n)
  active <- numeric(n)
  car.driver <- numeric(n)
  car.passenger <- numeric(n)

  main.purpose <- numeric(n)
  main.mode <- numeric(n)

  home.purpose <- numeric(n)
  work.purpose <- numeric(n)
  leisure.purpose <- numeric(n)
  app.purpose <- numeric(n)
  pickup.purpose <- numeric(n)

  every.segment.car <- numeric(n)

  if (n == 0){
    return(NULL)
  }


  for (i in 1:n){

    key[i] <- key.id
    mesh.id[i] <- pe.df[person, "mesh.id"]
    region[i] <- pe.df[person, "Region"]

    trip.weights[i] <- mean(chains[[i]]$Tripwgt_ann_mill_fullhh)

    if (is.na(trip.weights[i])){
      w.factor <- pe.df[person, "peweight_fullhh"] / pe.df[person, "peweight"]
      trip.weights[i] <- mean(chains[[i]]$Tripwgt_ann_mill) * w.factor
    }

    gender[i] <- pe.df[person, "sex"]
    area[i] <- pe.df[person, "Areatype2"]
    age[i] <- pe.df[person, "age.group"]

    employ[i] <- pe.df[person, "employment.status"]
    child.status[i] <- pe.df[person, "has.children"]

    num.segments[i] <- nrow(chains[[i]])
    duration[i] <- sum(chains[[i]]$duration)
    trip.length[i] <- sum(chains[[i]]$jdist)

    pt[i] <- ifelse(sum(c(3,5,6) %in% chains[[i]]$trmode) > 0, 1, 0)
    active[i] <- ifelse(sum(c(2,4) %in% chains[[i]]$trmode) > 0, 1, 0)
    car.driver[i] <- ifelse(sum(1 %in% chains[[i]]$Newmode) > 0, 1, 0)
    car.passenger[i] <- ifelse(sum(2 %in% chains[[i]]$Newmode) > 0, 1, 0)

    every.segment.car[i] <- ifelse(sum(chains[[i]]$trmode == 1) == nrow(chains[[i]]), 1, 0)

    main.purpose[i] <- trip.chain.main.purpose(chains[[i]])
    main.mode[i] <- trip.chain.main.mode(chains[[i]])

    home.purpose[i] <- ifelse(sum(c(2,15) %in% chains[[i]]$tractiv) > 0, 1, 0)
    work.purpose[i] <- ifelse(sum(c(3,6,7) %in% chains[[i]]$tractiv) > 0, 1, 0)
    leisure.purpose[i] <- ifelse(sum(c(5,11,14) %in% chains[[i]]$tractiv) > 0, 1, 0)
    app.purpose[i] <- ifelse(sum(c(4,8) %in% chains[[i]]$tractiv) > 0, 1, 0)
    pickup.purpose[i] <- ifelse(sum(c(9,10,12) %in% chains[[i]]$tractiv) > 0, 1, 0)


  }

  return(list(key, mesh.id, region, trip.weights, gender, area, age, employ, child.status, num.segments, duration, trip.length, pt, active, car.driver, car.passenger, every.segment.car, main.purpose, main.mode, home.purpose, work.purpose, leisure.purpose, app.purpose, pickup.purpose))


}


construct.v.trip.chains <- function(n){
  v.trip.chains <- Vectorize(construct.trip.chains)
  chains.df <- v.trip.chains(1:n)
  chains2 <- do.call(rbind.data.frame, chains.df)
  colnames(chains2) <- c("key", "mesh.id", "region", "trip.weights", "gender", "area", "age", "employ", "child.status", "num.segments", "duration", "trip.length", "pt", "active", "car.driver", "car.passenger", "every.segment.car", "main.purpose", "main.mode", "home.purpose", "work.purpose", "leisure.purpose", "app.purpose", "pickup.purpose")

  return(chains2)

}


## Now do the stats one


purposes.per.chain <- function(chain, primary, secondary){

  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  if(main.purpose != primary){
    return(NA)
  }

  if (secondary == 4){
    sec.purposes <- c(4,8)
  }
  else if (secondary == 5){
    sec.purposes <- c(5,11,14)
  }
  else if (secondary == 6){
    sec.purposes <- c(9,10,12)
  }

  chain.purposes <- chain[(1:n), "tractiv"]

  if (sum(sec.purposes %in% chain.purposes) > 0){
    return(1)
  }
  else{
    return(0)
  }

}




main.mode.per.purpose<- function(chain, purpose, mode){

  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  if(main.purpose != primary){
    return(NA)
  }

  main.mode <- trip.chain.main.mode(chain)

  if(main.mode == mode){
    return(1)
  }
  else{
    return(0)
  }
}



modes.per.chain <- function(chain, mode1, mode2){

  n <- nrow(chain)

  modes <- chain[(1:n), "trmode"]
  pass.or.driver <- chain[(1:n), "Newmode"]
  # driver = 1, passenger = 2
  if (mode1 == 3){
    mode1.new <- c(2,4)
  }
  else if (mode1 == 4){
    mode1.new <- c(3,5,6)
  }

  if (mode2 == 3){
    mode2.new <- c(2,4)
  }
  else if (mode2 == 4){
    mode2.new <- c(3,5,6)
  }

  if (mode1 == 1|| mode1 == 2){
    if(mode1 %in% pass.or.driver && sum(mode2.new %in% modes)>0){
      return(1)
    }
    else
      return(0)
  }

  if(mode1 %in% modes && mode2 %in% modes){
    return(1)
  }
  else
    return(0)

}



car.only.chain <- function(chain){

  n <- nrow(chain)
  modes <- chain[(1:n), "trmode"]

  if (mean(modes,na.rm=TRUE)==1){
    return(1)
  }
  else
    return(0)
}



purpose.modes.per.chain <- function(chain, purpose, mode1, mode2){

  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  if(main.purpose != purpose){
    return(NA)
  }

  modes <- chain[(1:n), "trmode"]
  pass.or.driver <- chain[(1:n), "Newmode"]
  # driver = 1, passenger = 2
  if (mode1 == 3){
    mode1.new <- c(2,4)
  }
  else if (mode1 == 4){
    mode1.new <- c(3,5,6)
  }

  if (mode2 == 3){
    mode2.new <- c(2,4)
  }
  else if (mode2 == 4){
    mode2.new <- c(3,5,6)
  }

  if (mode1 == 1|| mode1 == 2){
    if(mode1 %in% pass.or.driver && sum(mode2.new %in% modes)>0){
      return(1)
    }
    else
      return(0)
  }

  if(mode1 %in% modes && mode2 %in% modes){
    return(1)
  }
  else
    return(0)

}



purpose.avg.chain.length <- function(chain, purpose){

  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  if(main.purpose != purpose){
    return(NA)
  }
  else{
    return(n)
  }
}




mode.avg.chain.length <- function(chain, mode){

  n <- nrow(chain)
  main.mode <- trip.chain.main.mode(chain)

  if(main.mode != mode){
    return(NA)
  }
  else{
    return(n)
  }
}



active.use <- function(trip){
  # this function calculates whether active modes such as cycling or walking are used in a trip- no (0),
  # or yes (1)


  # number of legs in the trip
  n <- nrow(trip)

  # each mode of transport used
  modes <- trip[(1:n), "trmode"]

  # the codes for active use, 2 = walking, 4 = cycling
  active.modes <- c(2,4)

  # default to 0
  active.use <- 0

  # if pt used for any leg in a trip, set to 1
  if (sum(modes %in% active.modes) > 0){
    active.use <- 1
  }

  # return whether pt ever used
  return(active.use)

}



is.pt <- function(trip){
  # this function calculates whether pt is used in any leg of a trip- no (0),
  # or yes (1)


  # number of legs in the trip
  n <- nrow(trip)

  # each mode of transport used
  modes <- trip[(1:n), "trmode"]

  # the codes for pt - 3 = bus, 5 = train, 6 = ferry
  pt.modes <- c(3,5,6)

  # default to 0
  pt.use <- 0

  # if pt used for any leg in a trip, set to 1
  if (sum(modes %in% pt.modes) > 0){
    pt.use <- 1
  }

  # return whether pt ever used
  return(pt.use)

}



construct.individual.stats <- function(person){

  # this function calculates a whole bunch of stats for one person
  # in particular this uses many of the functions written above to find these
  # more can and will be added over time

  key.id <- pe.df[person, "key"]

  # step 1- create the trip chains
  all.chains <- trip.chains.per.person(key.id)
  chains <- split(all.chains, all.chains$trip.num)

  # rename trip.chain.main.purpose to chain
  chain.main.purposes <- unname(unlist(lapply(chains, trip.chain.main.purpose)))

  n = length(chains)

  if(n==0){
    return(list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  }

  # chains main purpose = work, and go to leisure, maintenance, pickup
  work.and.maintenance <- mean(unlist(lapply(chains, purposes.per.chain, primary = 3, secondary = 4)), na.rm=TRUE)
  work.and.leisure <- mean(unlist(lapply(chains, purposes.per.chain, primary = 3, secondary = 5)), na.rm=TRUE)
  work.and.pickup <- mean(unlist(lapply(chains, purposes.per.chain, primary = 3, secondary = 6)), na.rm=TRUE)

  # chains main purpose = home, and go to leisure, maintenance, pickup
  home.and.maintenance <- mean(unlist(lapply(chains, purposes.per.chain, primary = 2, secondary = 4)), na.rm=TRUE)
  home.and.leisure <- mean(unlist(lapply(chains, purposes.per.chain, primary = 2, secondary = 5)), na.rm=TRUE)
  home.and.pickup <- mean(unlist(lapply(chains, purposes.per.chain, primary = 2, secondary = 6)), na.rm=TRUE)

  # 1- drive, 2- passenger, 3- active, 4- pt
  # chains mode with car + walk + pt
  car.and.active <- mean(unlist(lapply(chains, modes.per.chain, mode1 = 1, mode2 = 3)), na.rm=TRUE)
  car.and.pt <- mean(unlist(lapply(chains, modes.per.chain, mode1 = 1, mode2 = 4)), na.rm=TRUE)
  pass.and.active <- mean(unlist(lapply(chains, modes.per.chain, mode1 = 2, mode2 = 3)), na.rm=TRUE)
  pass.and.pt <- mean(unlist(lapply(chains, modes.per.chain, mode1 = 2, mode2 = 4)), na.rm=TRUE)
  active.and.pt <- mean(unlist(lapply(chains, modes.per.chain, mode1 = 3, mode2 = 4)), na.rm=TRUE)





  # chain segment length for leisure, main, etc
  home.segments <- mean(unlist(lapply(chains, purpose.avg.chain.length, purpose = 2)), na.rm=TRUE)
  work.segments <- mean(unlist(lapply(chains, purpose.avg.chain.length, purpose = 3)), na.rm=TRUE)
  main.segments <- mean(unlist(lapply(chains, purpose.avg.chain.length, purpose = 4)), na.rm=TRUE)
  leisure.segments <- mean(unlist(lapply(chains, purpose.avg.chain.length, purpose = 5)), na.rm=TRUE)
  pickup.segments <- mean(unlist(lapply(chains, purpose.avg.chain.length, purpose = 6)), na.rm=TRUE)

  # chains were every segment is car
  every.segment.car <- mean(unlist(lapply(chains, car.only.chain)), na.rm=TRUE)

  driving.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 1)), na.rm=TRUE)
  passenger.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 2)), na.rm=TRUE)
  active.segments <- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 3)), na.rm=TRUE)
  pt.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 4)), na.rm=TRUE)

  #### ADDED BELOW HERE- AND TO RETURN AND COLNAMES

  # modes
  pt.use <- sum(unlist(lapply(chains, is.pt))) / length(chains)

  # stat 8 <- % of time with any walking/cycling
  active.use <- sum(unlist(lapply(chains, active.use))) / length(chains)

  # have any active
  # stat26 <- home.purpose
  home.purpose <- sum(chain.main.purposes == 2) / length(chain.main.purposes)

  # stat27 <- work.purpose
  work.purpose <- sum(chain.main.purposes == 3) / length(chain.main.purposes)

  # stat28 <- maintain.purpose
  maintain.purpose <-sum(chain.main.purposes == 4) / length(chain.main.purposes)

  # stat29 <- leisure.purpose
  leisure.purpose <- sum(chain.main.purposes == 5) / length(chain.main.purposes)

  # stat30 <- accompany.purpose
  accompany.purpose <-sum(chain.main.purposes == 6) / length(chain.main.purposes)

  #below here new
  chain.main.modes <- unname(unlist(lapply(chains, trip.chain.main.mode)))

  drive.mode <- sum(chain.main.modes == 1) / length(chain.main.purposes)
  passenger.mode <- sum(chain.main.modes == 2) / length(chain.main.purposes)
  active.mode <- sum(chain.main.modes == 3) / length(chain.main.purposes)
  pt.mode <- sum(chain.main.modes == 4) / length(chain.main.purposes)


  return(list(work.and.maintenance, work.and.leisure, work.and.pickup, home.and.maintenance, home.and.leisure, home.and.pickup, car.and.active, car.and.pt, pass.and.active, pass.and.pt, active.and.pt, home.segments, work.segments, main.segments, leisure.segments, pickup.segments, driving.segments, passenger.segments, active.segments, pt.segments, every.segment.car, pt.use, active.use, home.purpose, work.purpose, maintain.purpose, leisure.purpose, accompany.purpose, drive.mode, passenger.mode, active.mode, pt.mode))
}



construct.v.individual.stats <- function(n){
  v.person.stats <- Vectorize(construct.individual.stats)
  results.df <- v.person.stats(1:n)
  results.df2 <- do.call(rbind.data.frame, results.df)
  num.stats <- nrow(results.df2) / nrow(pe.df)
  results.df3 <- data.frame(matrix(unlist(t(results.df2)), byrow = T, nrow(pe.df), num.stats))
  colnames(results.df3) <- c("work.and.maintenance", "work.and.leisure", "work.and.pickup", "home.and.maintenance", "home.and.leisure", "home.and.pickup", "car.and.active", "car.and.pt", "pass.and.active", "pass.and.pt", "active.and.pt", "home.segments", "work.segments", "main.segments", "leisure.segments", "pickup.segments", "driving.segments", "passenger.segments", "active.segments", "pt.segments", "every.segment.car", "pt.use", "active.use", "home.purpose", "work.purpose", "maintain.purpose", "leisure.purpose", "accompany.purpose", "drive.mode", "passenger.mode", "active.mode", "pt.mode")
  results.df3$key <- rownames(results.df3)
  # add the results to the pe.df dataframe #x _> row.num
  people.df <- merge(pe.df, results.df3, by.x = "row.num", by.y = "key")
  people.df <- people.df[!is.na(people.df$car.and.active),]
  return(people.df)

}
