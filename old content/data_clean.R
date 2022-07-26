tr.df <- #DATA GOES HERE
hh.df <- #DATA GOES HERE
pe.df <- #DATA GOES HERE

tr.df <- tr.df[-294487,]

tr.df$key <- factor(paste0(tr.df$samno, tr.df$person, tr.df$Year))
pe.df$key <- factor(paste0(pe.df$samno, pe.df$person, pe.df$Year))

tr.df$key <- as.numeric(levels(tr.df$key))[tr.df$key]
pe.df$key <- as.numeric(levels(pe.df$key))[pe.df$key]

pe.df$hh.key <- factor(paste0(pe.df$samno, pe.df$Year))
pe.df$hh.key <- as.numeric(levels(pe.df$hh.key))[pe.df$hh.key]

hh.df$hh.key <- factor(paste0(hh.df$samno, hh.df$Year))
hh.df$hh.key <- as.numeric(levels(hh.df$hh.key))[hh.df$hh.key]

#full.18.keys <- pe.df[pe.df$peweight_fullhh > 0 & pe.df$Year == 18, "key"]
tr.df <- tr.df[tr.df$Tripwgt_ann_mill_fullhh > 0,]
#tr.df <- tr.df[(tr.df$Tripwgt_ann_mill_fullhh > 0 & tr.df$Year < 18)| (tr.df$key %in% full.18.keys),]

pe.df <- pe.df[pe.df$peweight_fullhh > 0,]
hh.df <- hh.df[hh.df$hhweight_fullhh > 0,]

tr.df <- tr.df[!is.na(tr.df$key),]

# for stats, if has children
pe.df$has.children <- "N"

family.households <- hh.df[hh.df$hhtype %in% c(4,6,7), "hh.key"]

pe.df[pe.df$age < 18, "has.children"] <- "Y"
pe.df[pe.df$hh.key %in% family.households, "has.children"] <- "Y"

# two people have gender written as 'g'
pe.df <- pe.df[pe.df$sex != "G",]

pe.df$age.group <- 0
pe.df[abs(pe.df$age) > 4, "age.group"] <- 1
pe.df[abs(pe.df$age) > 17, "age.group"] <- 2
pe.df[abs(pe.df$age) > 35, "age.group"] <- 3
# this line new
pe.df[abs(pe.df$age) > 59, "age.group"] <- 4

pe.df$employment.status <- "N"
pe.df[abs(pe.df$peempstat) < 4, "employment.status"] <- "Y"

hh.df$mesh.id <- factor(paste0(hh.df$NZDep_score_2013, hh.df$Pop))
pe.df$mesh.id <- hh.df$mesh.id[match(pe.df$hh.key, hh.df$hh.key)]

pe.df$row.num <- 1:nrow(pe.df)
