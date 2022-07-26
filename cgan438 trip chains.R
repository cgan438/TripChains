### This file outlines the functions used for my project analyzing the results
### of the 14-18 NZHTS. These results are not included.

### To use, the following should be done:

### 1) read in the data
### example as below
### tr.df <- read.csv("tr_y1418_2021-08-18.csv")
### hh.df <- read.csv("hh_y1418_2021-08-18.csv")
### pe.df <- read.csv("pe_y1418_2021-08-18.csv")

### 2) clean the data
###
### tr.df <- clean.trips(tr.df)
### pe.df <- clean.people(pe.df, hh.df)
### hh.df <- clean.households(hh.df)

### 3) generate the trip chains
### should be noted this takes ~5min
### chains.data <- generate.trip.chains(pe.df, tr.df)

### the variables are:
### key- identifier of person
### mesh.id- identifier of meshblock of person (disguised, nothing identifiable)
### region - region of person
### trip.weights - weight of trip (see nzhts methodology reports for more detail)
### gender - gender of person
### area - type of area, rural major urban or small urban area
### age - age group, either 0-4, 5-17, 18-35, 36-59 or 60+
### employ -  employed (Y) or unemployed (N)
### child.status - in household with children (Y) or not (N)
### num.segments - number of trip segments in trip chain
### duration - total duration of trip chain (in hr)
### trip.length - total distance travelled in trip chain (in km)
### pt - was public transport used in any segment? 0 = no, 1 = yes
### active - was an active mode (walking/cycling) used in any segment? 0 = no, 1 = yes
### car.driver - was driving a car in any segment? 0 = no, 1 = yes
### car.passenger - was being a passenger in any segment? 0 = no, 1 = yes
### every.segment.car - was every segment in the trip chain in a vehicle (either driver or passenger)
### main.purpose - main purpose of trip chain (1 is mode change, 2 is home, 3 is work, 4 is maintain, 5 is leisure, 6 is accompany)
### main.mode- main travel mode of trip chain (1 is car.driving, 2 is car.passenger, 3 is active.mode, 4 is pt.mode, 5 is other.modes)
### home.purpose - did any segment involve travelling home? 0 = no, 1 = yes
### work.purpose - did any segment involve travelling to work/school? 0 = no, 1 = yes
### leisure.purpose- did any segment involve travelling for leisure? 0 = no, 1 = yes
### app.purpose - did any segment involve travelling for maintenance (appointments)? 0 = no, 1 = yes
### pickup.purpose - did any segment involve travelling for accompanying?0 = no, 1 = yes


### 4) generate the individual statistics
### should be noted takes ~10 min
### people.data <- generate.individual.stats(pe.df, tr.df)

### the variables are:
### work.and.maintenance/leisure/pickup - % of trip chains for person with main purpose of work and a secondary maintenance/leisure/accompanying segment
### home.and.maintenance/leisure/pickup - % of trip chains for person with main purpose of home and a secondary maintenance/leisure/accompanying segment
### car.and.active/car.and.pt/pass.and.active/pass.and.pt/active.and.pt - % of trip chains with
### both trip modes where car = driver, pt = public transport, pass = passenger in car and active = active mode
### home.segments/work.segments/main.segments/leisure.segments/pickup.segments - average number of trip segments in chains for person for each main purpose
### driving.segments/passenger.segments/active.segments/pt.segments - average number of trip segments in chains for person for each main mode
### every.segment.car - % of time for person that trip chain only involves driving
### pt.use/active.use - % of time for person that trip chain involves either public transport ot active mode
### home.purpose/work.purpose/maintain.purpose/leisure.purpose/accompany.purpose - % of time for person that main purpose of trip chain is each purpose
### drive.mode/passenger.mode/active.mode/pt.mode - % of time for person that main mode of trip chain is each travel mode

### the other variables are as defined in the data dictionary.

### 5) complete analysis
### this was completed with help of the survey package, and is not included.
### example usage would be:
### for the people:
### people.design <- svydesign(ids = ~mesh.id, strata = ~Region, weights = ~peweight_fullhh, data = people.data, nest = TRUE)
### for the trip chains:
### chains.design <- svydesign(ids = ~mesh.id, strata = ~region, weights = ~trip.weights, data = chains.data, nest = TRUE)

### it should be noted that the other functions included are used in the functions
### provided aboved. therefore, an example usage for them has been ommitted,
### however the function itself has documentation about its purpose.

clean.trips <- function(trips){
  # this function performs some "cleaning" on the trips data.
  # this involves creating a unique key for each person that completes each trip
  # and also involves removing trips from non-responding households

  # there is one input- the trips data
  # there is one output- the cleaned trips data

  # create key for each trip to correspond to person
  trips$key <- factor(paste0(trips$samno, trips$person, trips$Year))
  trips$key <- as.numeric(levels(trips$key))[trips$key]

  # remove trips with illegal weights
  trips <- trips[trips$Tripwgt_ann_mill_fullhh > 0,]
  trips <- trips[!is.na(trips$key),]

  return(trips)

}

clean.people <- function(people, households){
  # this function performs some "cleaning" on the people data.
  # at first this involves creating a unique key for each person and household.
  # people from non-responding households are then removed.

  # a set of new variables is then established.
  # these are:
  # has.children, Y is household of person has children, N otherwise
  # age.group, 0 = 0-4 years old, 1 = 5-17, 2 = 18-35, 3 = 35-59, 4 = 60+
  # employment.status, Y = employed, N = unemployed (children are set to unemployed)

  # there are two inputs, the people data and the households data
  # there is one output- the cleaned people data

  # create key for each person
  people$key <- factor(paste0(people$samno, people$person, people$Year))
  people$key <- as.numeric(levels(people$key))[people$key]

  # create key for each household
  people$hh.key <- factor(paste0(people$samno, people$Year))
  people$hh.key <- as.numeric(levels(people$hh.key))[people$hh.key]

  # remove trips with illegal weights
  people <- people[people$peweight_fullhh > 0,]

  # create has.children variable - where = "N" if no children in household, or "Y" if there are
  people$has.children <- "N"
  family.households <- households[households$hhtype %in% c(4,6,7), "hh.key"]
  people[people$age < 18, "has.children"] <- "Y"
  people[people$hh.key %in% family.households, "has.children"] <- "Y"

  # remove people with G as gender
  people <- people[people$sex != "G",]

  # categorize into age groups,
  # 0 = 1-4
  # 1 = 5-17
  # 2 = 18-35
  # 3 = 35-59
  # 5 = 60+

  people$age.group <- 0
  people[abs(people$age) > 4, "age.group"] <- 1
  people[abs(people$age) > 17, "age.group"] <- 2
  people[abs(people$age) > 35, "age.group"] <- 3
  people[abs(people$age) > 59, "age.group"] <- 4

  # create employment variable - "N" = unemployed, "Y" = employed
  people$employment.status <- "N"
  people[abs(people$peempstat) < 4, "employment.status"] <- "Y"

  # find meshblock ids
  households$hh.key <- factor(paste0(households$samno, households$Year))
  households$hh.key <- as.numeric(levels(households$hh.key))[households$hh.key]

  households$mesh.id <- factor(paste0(households$NZDep_score_2013, households$Pop))
  people$mesh.id <- households$mesh.id[match(people$hh.key, households$hh.key)]
  people$row.num <- 1:nrow(people)

  return(people)

}


clean.households <- function(households){
  # this function performs some "cleaning" on the household data.
  # this involves creating a unique key for each household
  # non-responding households are then removed

  # there is one input- the households data
  # there is one output- the cleaned households data

  # find household key
  households$hh.key <- factor(paste0(households$samno, households$Year))
  households$hh.key <- as.numeric(levels(households$hh.key))[households$hh.key]

  # remove illegal households
  households <- households[households$hhweight_fullhh > 0,]
  households$mesh.id <- factor(paste0(households$NZDep_score_2013, households$Pop))

  return(households)

}






generate.trip.chains <- function(people, trips){
  # this function generates each trip chain taken in the trips dataframe
  # and creates a dataframe with the following statistics:
  #
  #
  # this function has two inputs, the people and the trips data
  # this function has one output, the trip chains constructed

  # find each trip chain for each person
  chains.df <- v.trip.chains(person = 1:nrow(people), people, trips)

  # reformat into nicer df
  chains2 <- do.call(rbind.data.frame, chains.df)
  colnames(chains2) <- c("key", "mesh.id", "region", "trip.weights", "gender", "area", "age", "employ", "child.status", "num.segments", "duration", "trip.length", "pt", "active", "car.driver", "car.passenger", "every.segment.car", "main.purpose", "main.mode", "home.purpose", "work.purpose", "leisure.purpose", "app.purpose", "pickup.purpose")

  return(chains2)

}




generate.individual.stats <- function(people, trips){
  # this function creates a dataframe with the following statistics for each person:
  #
  #
  # this function has two inputs, the people and the trips data
  # this function has one output, the statistics obtained

  # find stats for each person
  results.df <- v.person.stats(person = 1:nrow(people), people, trips)

  #reformat into nicer df
  results.df2 <- do.call(rbind.data.frame, results.df)

  num.stats <- nrow(results.df2) / nrow(people)
  results.df3 <- data.frame(matrix(unlist(t(results.df2)), byrow = T, nrow(people), num.stats))

  colnames(results.df3) <- c("work.and.maintenance", "work.and.leisure", "work.and.pickup", "home.and.maintenance", "home.and.leisure", "home.and.pickup", "car.and.active", "car.and.pt", "pass.and.active", "pass.and.pt", "active.and.pt", "home.segments", "work.segments", "main.segments", "leisure.segments", "pickup.segments", "driving.segments", "passenger.segments", "active.segments", "pt.segments", "every.segment.car", "pt.use", "active.use", "home.purpose", "work.purpose", "maintain.purpose", "leisure.purpose", "accompany.purpose", "drive.mode", "passenger.mode", "active.mode", "pt.mode")

  results.df3$key <- rownames(results.df3)

  # merge results with people
  people.df <- merge(people, results.df3, by.x = "row.num", by.y = "key")
  people.df <- people.df[!is.na(people.df$car.and.active),]

  return(people.df)
}



trip.chains.per.person <- function(person, tr.df){
  # this function calculates the trip chains for each person.
  # this is done by finding the number trip chain that a particular trip segment was in for
  # that person.
  # a new trip chain occurs when the person has been at a location for at least 90 minutes
  # is starting a trip from home, work or school or is their first trip.

  # this function has two inputs, one the key for a person, and the second the trips data
  # this function has one output, the trips completed by one person, with an added trip chain
  # variable representing the number chain the trip is in

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
  # find the trip chain for each segment
  if (n > 1){

    for (i in 2:n){

      # accommodate days of the week differing
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
  # this function calculates the main purpose of a trip chain
  # the main purpose is dependent on the following criteria:
  # if the trip chain has any work, then main purpose = work/school
  # if the trip starts at home then the purpose may not be to return home
  # changing mode may only be main purpose if no other purposes are present
  # else main purpose is the purpose with the longest duration
  # is starting a trip from home, work or school or is their first trip.

  # this function has one input, the trip chain
  # this function has one output, the main purpose, coded by the following:
  # 1 is mode change, 2 is home, 3 is work, 4 is maintain, 5 is leisure, 6 is accompany

  n <- nrow(trip)

  home.duration <- 0#2, 15
  work.duration <- 0#3,6,7
  maintain.duration <- 0#4,8,
  leisure.duration <- 0#5,11,14
  accompany.duration <- 0#9,10,12
  mode.change.duration <- 0#1

  # find duration of each purpose in trip chain
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


  mode.change.duration <- ifelse(mode.change.duration > 0, 0.01, 0)

  durations <- c(mode.change.duration, home.duration, work.duration, maintain.duration, leisure.duration, accompany.duration)

  main.purpose <- which.max(durations)
  # 1 is mode change, 2 is home, 3 is work, 4 is maintain, 5 is leisure, 6 is accompany



  return(main.purpose)
}




trip.chain.main.mode <- function(trip){
  # this function calculates the main travel mode of a trip chain
  # the main travel mode of a trip chain is the travel mode that
  # is used to travel the greatest distance

  # this function has one input, the trip chain
  # this function has one output, the main travel mode, coded by the following:
  # 1 is car.driving, 2 is car.passenger, 3 is active.mode, 4 is pt.mode, 5 is other.modes

  n <- nrow(trip)

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



construct.trip.chains <- function(person, pe.df, tr.df){
  # this function consturcts the trip chains (and corresponding statistics) for
  # an input person.

  # this function has three inputs, person, a number representing a row of the people data
  # pe.df, the people data and tr.df, the trips data
  # this function has one output, the statistics about the trip chains of the input person

  key.id <- pe.df[person, "key"]
  # step 1- create the trip chains
  all.chains <- trip.chains.per.person(key.id, tr.df)
  chains <- split(all.chains, all.chains$trip.num)

  # initialise stats
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

  # if no trip chains then return nothing
  if (n == 0){
    return(NULL)
  }

  # generate trip chain stats
  for (i in 1:n){

    key[i] <- key.id
    mesh.id[i] <- pe.df[person, "mesh.id"]
    region[i] <- pe.df[person, "Region"]

    trip.weights[i] <- mean(chains[[i]]$Tripwgt_ann_mill_fullhh)

    if (is.na(trip.weights[i])){
      w.factor <- pe.df[person, "peweight_fullhh"] / pe.df[person, "peweight"]
      trip.weights[i] <- mean(chains[[i]]$Tripwgt_ann_mill) * w.factor
    }

    # individual stats
    gender[i] <- pe.df[person, "sex"]
    area[i] <- pe.df[person, "Areatype2"]
    age[i] <- pe.df[person, "age.group"]

    employ[i] <- pe.df[person, "employment.status"]
    child.status[i] <- pe.df[person, "has.children"]

    num.segments[i] <- nrow(chains[[i]])
    duration[i] <- sum(chains[[i]]$duration)
    trip.length[i] <- sum(chains[[i]]$jdist)

    # was each type of mode used in the trip chain?
    pt[i] <- ifelse(sum(c(3,5,6) %in% chains[[i]]$trmode) > 0, 1, 0)
    active[i] <- ifelse(sum(c(2,4) %in% chains[[i]]$trmode) > 0, 1, 0)
    car.driver[i] <- ifelse(sum(1 %in% chains[[i]]$Newmode) > 0, 1, 0)
    car.passenger[i] <- ifelse(sum(2 %in% chains[[i]]$Newmode) > 0, 1, 0)

    # was every segment in the trip chain in a vehicle (either driving or passenger)
    every.segment.car[i] <- ifelse(sum(chains[[i]]$trmode == 1) == nrow(chains[[i]]), 1, 0)

    #main purpose and main mode of trip chain
    main.purpose[i] <- trip.chain.main.purpose(chains[[i]])
    main.mode[i] <- trip.chain.main.mode(chains[[i]])

    # was each purpose travelled for in the trip chain?
    home.purpose[i] <- ifelse(sum(c(2,15) %in% chains[[i]]$tractiv) > 0, 1, 0)
    work.purpose[i] <- ifelse(sum(c(3,6,7) %in% chains[[i]]$tractiv) > 0, 1, 0)
    leisure.purpose[i] <- ifelse(sum(c(5,11,14) %in% chains[[i]]$tractiv) > 0, 1, 0)
    app.purpose[i] <- ifelse(sum(c(4,8) %in% chains[[i]]$tractiv) > 0, 1, 0)
    pickup.purpose[i] <- ifelse(sum(c(9,10,12) %in% chains[[i]]$tractiv) > 0, 1, 0)


  }
  # return the stats
  return(list(key, mesh.id, region, trip.weights, gender, area, age, employ, child.status, num.segments, duration, trip.length, pt, active, car.driver, car.passenger, every.segment.car, main.purpose, main.mode, home.purpose, work.purpose, leisure.purpose, app.purpose, pickup.purpose))


}



# vectorise the trip chain constructor, so that it may be run on everyone
v.trip.chains <- Vectorize(construct.trip.chains, vectorize.args = "person")



purposes.per.chain <- function(chain, primary, secondary){
  # this function returns a 0 or a 1 or NA, depending on whether the
  # input trip chain has both the input primary purpose, and the secondary purpose
  # at any point

  # there are three inputs into this function, a trip chain, a primary purpose
  # (such as home or work), and a secondary purpose
  # there is one output of this function, a 1 if the chain has the input primary purpose
  # and secondary purpose or 0 if the chain has the input primary purpose but not the
  # secondary purpose, and NA if the chain does not have the input main purpose

  # find main purpose of chain
  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  # if not match with input primary then NA
  if(main.purpose != primary){
    return(NA)
  }

  # else find secondary purposes
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

  # if secondary purpose in chain return 1, else 0
  if (sum(sec.purposes %in% chain.purposes) > 0){
    return(1)
  }
  else{
    return(0)
  }

}




main.mode.per.purpose<- function(chain, purpose, mode){
  # this function returns a 0 or a 1 or NA, depending on whether the
  # input trip chain has both the input primary purpose, and the main travel mode

  # there are three inputs into this function, a trip chain, a primary purpose
  # (such as home or work), and a main travel mode (such as driving or walking)
  # there is one output of this function, a 1 if the chain has the input primary purpose
  # and mode or 0 if the chain has the input primary purpose but not the
  # travel mode, and NA if the chain does not have the input main purpose

  # find main purpose of chain
  n <- nrow(chain)
  main.purpose <- trip.chain.main.purpose(chain)

  if(main.purpose != primary){
    return(NA)
  }
  # find main mode of chain
  main.mode <- trip.chain.main.mode(chain)

  # if main mode is input mode than 1, else 0
  if(main.mode == mode){
    return(1)
  }
  else{
    return(0)
  }
}



modes.per.chain <- function(chain, mode1, mode2){
  # this function returns either 0 or 1.
  # a 1 is returned if the input trip chain has a segment with both of the two input
  # modes in it, and 0 otherwise

  # this function has three inputs, a trip chain and two mode types
  # this function has one output, a 0 or a 1, defined above
  n <- nrow(chain)

  #trip modes in chain
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
  # this function returns a 1 if a car was used in every leg of a trip chain
  # this function has one input, a trip chain
  n <- nrow(chain)
  modes <- chain[(1:n), "trmode"]

  if (mean(modes,na.rm=TRUE)==1){
    return(1)
  }
  else
    return(0)
}



purpose.modes.per.chain <- function(chain, purpose, mode1, mode2){
  # this function works as with modes.per.chain, but a NA is returned if the
  # main purpose of the trip chain does not match the input purpose

  # this function has four inputs, a trip chain and two mode types and a main purpose
  # this function has one output, a 0 or a 1 or a NA, defined above
  n <- nrow(chain)

  # find the main purpose
  main.purpose <- trip.chain.main.purpose(chain)

  # if the main purpose not input, then NA
  if(main.purpose != purpose){
    return(NA)
  }

  # find all modes
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
  # this function calculates the average chain length for the input chain if the purpose
  # matches the input purpose

  # this function has two inputs, a trip chain and a purpose
  # this function has one output, the number of segments in the trip chain, if the purpose of the trip chain was the same as the input purpose. if not the same purpose, na is returned

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
  # this function calculates the average chain length for the input chain if the mode
  # matches the input mode

  # this function has two inputs, a trip chain and a mode
  # this function has one output, the number of segments in the trip chain, if the purpose of the trip chain was the same as the input mode if not the same mode, na is returned
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



construct.individual.stats <- function(person, pe.df, tr.df){

  # this function calculates a whole bunch of stats for one person
  # in particular this uses many of the functions written above to find these
  # more can and will be added over time

  key.id <- pe.df[person, "key"]

  # step 1- create the trip chains
  all.chains <- trip.chains.per.person(key.id, tr.df)
  chains <- split(all.chains, all.chains$trip.num)

  # rename trip.chain.main.purpose to chain
  chain.main.purposes <- unname(unlist(lapply(chains, trip.chain.main.purpose)))

  n = length(chains)
  # if no trip chains return nothing
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

  # number of segments in chains with each mode type as main mode
  driving.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 1)), na.rm=TRUE)
  passenger.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 2)), na.rm=TRUE)
  active.segments <- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 3)), na.rm=TRUE)
  pt.segments<- mean(unlist(lapply(chains, mode.avg.chain.length, mode = 4)), na.rm=TRUE)

  # public transport usage
  pt.use <- sum(unlist(lapply(chains, is.pt))) / length(chains)

  # stat 8 <- % of time with any walking/cycling
  active.use <- sum(unlist(lapply(chains, active.use))) / length(chains)

  # have any active
  # stat26 <- home.purpose % of time
  home.purpose <- sum(chain.main.purposes == 2) / length(chain.main.purposes)

  # stat27 <- work.purpose % of time
  work.purpose <- sum(chain.main.purposes == 3) / length(chain.main.purposes)

  # stat28 <- maintain.purpose % of time
  maintain.purpose <-sum(chain.main.purposes == 4) / length(chain.main.purposes)

  # stat29 <- leisure.purpose % of time
  leisure.purpose <- sum(chain.main.purposes == 5) / length(chain.main.purposes)

  # stat30 <- accompany.purpose % of time
  accompany.purpose <-sum(chain.main.purposes == 6) / length(chain.main.purposes)

  # % of time that each mode was main mode
  chain.main.modes <- unname(unlist(lapply(chains, trip.chain.main.mode)))

  drive.mode <- sum(chain.main.modes == 1) / length(chain.main.purposes)
  passenger.mode <- sum(chain.main.modes == 2) / length(chain.main.purposes)
  active.mode <- sum(chain.main.modes == 3) / length(chain.main.purposes)
  pt.mode <- sum(chain.main.modes == 4) / length(chain.main.purposes)


  return(list(work.and.maintenance, work.and.leisure, work.and.pickup, home.and.maintenance, home.and.leisure, home.and.pickup, car.and.active, car.and.pt, pass.and.active, pass.and.pt, active.and.pt, home.segments, work.segments, main.segments, leisure.segments, pickup.segments, driving.segments, passenger.segments, active.segments, pt.segments, every.segment.car, pt.use, active.use, home.purpose, work.purpose, maintain.purpose, leisure.purpose, accompany.purpose, drive.mode, passenger.mode, active.mode, pt.mode))
}



# vectorise the individual stats function
v.person.stats <- Vectorize(construct.individual.stats, vectorize.args = "person")

### Example Code Run

tr.df <- read.csv("tr_y1418_2021-08-18.csv")
hh.df <- read.csv("hh_y1418_2021-08-18.csv")
pe.df <- read.csv("pe_y1418_2021-08-18.csv")

tr.df <- clean.trips(tr.df)
pe.df <- clean.people(pe.df, hh.df)
hh.df <- clean.households(hh.df)

chains.data <- generate.trip.chains(pe.df, tr.df)
people.data <- generate.individual.stats(pe.df, tr.df)
