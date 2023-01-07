library(dplyr);
library(stringr);
library(rgl);
library(rio);

# global params ----
statnames <- c('atmosphere','gravity','temperature','water','resources');

anomalies <- import('anomalies.tsv');

planetparams <- list(
  atmosphere = c("Breathable", "Marginal", "Non-breathable", "Toxic", "Corrosive", "No atmosphere")
  ,gravity = c(0.1, 5)
  ,temperature = c(0, 4000)
  ,water = c("Oceans", "Ice caps", "Planet-wide ocean", "Ice-covered surface", "Trace", "No water")
  ,resources = c("Rich", "Poor", "No resources")
);

# initialize the global planet database
planetGlobalDB <- data.frame(x=numeric(0),y=numeric(0),z=numeric(0),info=I(list()));

# small helper functions ----

# How accurate is a given sensor given its level and distance to target?
sensorAccuracy <- function(sensorlevel,distance
                           ,sensorweight=20,distanceweight=10){
  pmax(0,1-exp(-sensorlevel/sensorweight)-(distance/distanceweight)^2)};

# Given a sensor's level, at what distance does it's accuracy drop to 0?
sensorMaxRange <- function(sensorlevel,sensorweight=20,distanceweight=10){
  sqrt(distanceweight^2 * (1 - exp(-sensorlevel/sensorweight)))};

# generic function for distance between two sets of coordinates
p3Distance <- function(coords1,coords2,...){UseMethod('p3Distance')};

p3Distance.default <- function(coords1,coords2,...){c(dist(rbind(coords1,coords2)))};
p3Distance.data.frame <- function(coords1,coords2,...){apply(coords1,1,p3Distance.default,coords2)};
p3Distance.matrix <- p3Distance.data.frame;

# find all planets in planetGlobalDB or planetLocalDB within a certain range of
# distances relative to a set of coordinates
withinDist <- function(coords,maxd,mind=0,db=planetGlobalDB,coordnames=c('x','y','z')){
  filter(db,between(p3Distance(db[,coordnames],coords),mind,maxd))};

extractPlanetInfo <- function(infocol){
  sapply(infocol,function(xx) {
    # make sure there are no null values
    for(ii in statnames) xx[[ii]] <- c(xx[[ii]],NA)[1];
    for(ii in setdiff(statnames,names(xx))) xx[[ii]]<-NA;
    data.frame(unclass(xx)[statnames],stringsAsFactors = F);
    },simplify=F) %>% 
    bind_rows};

numericNoises <- function(val,accuracy,valname,params=planetparams){
  pars <- params[[valname]];
  pmin(pmax(min(pars),val+rnorm(length(val),sd=median(pars)*(1-accuracy))),max(pars))};

nominalNoises <- function(val,accuracy,valname,...){
  unlist(.mapply(.nominalNoise,dots=list(val=val,accuracy=accuracy),MoreArgs=list(...)))};

.nominalNoise <- function(val,accuracy,valname,params=planetparams
                         ,accuracyweight=10){
  # clean up variables
  accuracy <- pmax(0.001,accuracy);
  if(missing(valname)) valname <- c(names(params)[sapply(planetparams,function(xx){
    match(xx,val,nomatch = 0) %>% max})!=0],'Unknown')[1];
  if(valname == 'Unknown'){
    warning('Cannot match val ',val,' to any parameter');
    return(NA)};
  candidateValues <- c(setdiff(possibleValues<-params[[valname]],val),'Unknown');
  # generate the number of candidate values
  nCandidateValues <- pmin(length(candidateValues),rpois(1,length(candidateValues)/accuracy/accuracyweight));
  candidateValues <- sample(candidateValues,nCandidateValues);
  if(length(candidateValues)==length(possibleValues)) return('Indeterminate');
  # generate the probability of being accurate
  if(runif(1)<accuracy^.25) candidateValues <- sample(c(candidateValues,val));
  return(paste0(candidateValues,collapse=' or '))
}

# use env like list
with.staRship <- function(data,expr,...){
  expr <- substitute(expr);
  with.default(mget(ls(data),data),eval(expr),...)};

formatCoords <- function(coords,template='("%s")',sep='", "',numformat='%.3f'){
  sprintf(template,paste0(sprintf(numformat,coords),collapse=sep))};

# conditionalUpdate <- function(data,valname,oldsuffix='_sensor',newsuffix='_sensor_current',finalsuffix='_sensor_final',infocol='info'){
#   oldquality <- paste0(valname,oldsuffix);
#   newquality <- paste0(valname,newsuffix);
#   finalquality <- paste0(valname,finalsuffix);
#   data[[finalquality]] <- pmax(coalesce(data[[oldquality]],0),data[[newquality]]);
#   updates <- which(coalesce(data[[oldquality]],0) != data[[finalquality]]);
# }

shipLog <- function(description='${name} is at ${coords}, carrying ${colonists} colonists and ${probes} probes.
SENSORS
atm: ${atmosphere_sensor}\tgrv: ${gravity_sensor}\th2o: ${water_sensor}\tres: ${resources_sensor}\ttmp: ${temperature_sensor}
SYSTEMS
landing gear: ${landing_gear}
construction equipment: ${equipment}'
    ,tags=NA,target=NA,stardate=Sys.time(),msg=c('message','cat','invisible')
    ,preset=c('custom','depart','arrive','event'),updateLog=T,ship){
  # because the default description is so verbose (for demonstrating all the 
  # possible substitution variables), if it, preset, and updateLog have not
  # been explicitly set by the user, we change the updateLog from its default
  # TRUE value to FALSE so you can get a quick status report by typing 
  # shipLog() without spamming the ship's log file.
  if(missing(description)&missing(preset)&missing(updateLog)) updateLog <- F;
  # This will often be called inside functions, and we standardize on calling
  # the staRship object 'ship' so auto-getting that object should be a reasonable
  # default.
  if(missing(ship)) ship <- get('ship',envir=parent.frame(1));
  # Collapse preset and msg to single choices.
  preset <- match.arg(preset); msg<-match.arg(msg);
  # Presets automate creation of frequently ocurring log messages.
  switch(preset
         ,depart={
           description<-'${name} departs from ${coords}';
           tags<- c(tags,'transit')}
         ,arrive={
           description<-'${name} arrives at ${coords}';
           tags<- c(tags,'arrival')}
         ,event={
           description<-'${name} experiences an event';
           tags<- c(tags,'event')}
  );
  # If a target is specified for a custom or depart log entry, append those 
  # coordinates
  if(preset %in% c('custom','depart') && !missing(target)){
    description <- paste(description, 'toward',formatCoords(target))}
  
  description <- str_interp(description,mget(ls(ship),ship)) %>% 
    gsub('c\\(\\"','("',.);
  
  tags <- paste0(na.omit(unique(tags)),collapse=';');
  log <- data.frame(description=description,tags=tags,stardate=stardate
                    ,x=ship$coords[1],y=ship$coords[2],z=ship$coords[3]);
  if(updateLog) ship$log <- rbind(log,ship$log);
  
  report <- switch(msg,message=message,cat=cat,invisible=invisible);
  report(unlist(list('\n',stardate,'\t',formatCoords(ship$coords),'\n',description)));
}

promptPlanet <- function(ship){
  ship$planetLocalDB
}

prepStarChart <- function(ship,cols=c('distance','dst','x','y','z','visited'
                                       ,"atmosphere", "gravity", "temperature"
                                       ,"water", "resources")){
  knownsystems <- ship$planetLocalDB;
  knownsystems$distance <- p3Distance(knownsystems[,c('x','y','z')],ship$coords);
  knownsystems <- arrange(knownsystems,distance) %>%
    cbind(extractPlanetInfo(knownsystems$info));
  if(is.null(lastd<-ship$lastdestination)) knownsystems$dst <- '' else {
    lastd <- cbind(data.frame(as.list(ship$lastdestination) %>% 
                                setNames(c('x','y','z'))),dst='>');
    knownsystems <- left_join(knownsystems,lastd) %>% 
      mutate(dst=coalesce(dst,''));
    };
  knownsystems$dst <- paste0(knownsystems$dst,ifelse(knownsystems$distance==0,'*',''));
  rownames(knownsystems) <- 1:nrow(knownsystems);
  if(identical(cols,NA)) return(knownsystems) else return(knownsystems[,cols]);
}

promptNav <- function(ship,digits=3,cols=c('distance','dst','x','y','z','visited'
                                           ,"atmosphere", "gravity", "temperature"
                                           ,"water", "resources")){
  knownsystems <- prepStarChart(ship,NA);
  selected <- '';
  while(!gsub('i','',selected) %in% c('c','r',rownames(knownsystems))){
    print(format(knownsystems[,cols],digits=digits));
    cat('\n---\nFrom the above table, select the row number of the system to which you want to travel, 
"c" to cancel, "iX" to get information about a specific system (where X is row number), 
or "r" to resume traveling to an existing destination.');
    selected <- readline('Row number: ');
  };
  if(grepl('^i',selected)){
    return(knownsystems[gsub('i','',selected),'info'][[1]])};
  switch(selected
         ,c=return()
         ,r=moveShip(ship)
         ,moveShip(ship,knownsystems[selected,c('x','y','z')])
         );
}

rglStarChart <- function(ship,size=5,herecol='orange',...){
  data <- prepStarChart(ship,NA);
  rglids<-with(data,plot3d(x=x,y=y,z=z,size=size));
  points3d(x=ship$coords[1],y=ship$coords[2],z=ship$coords[3],size=size*2,color=herecol);
  sensornames <- grep('_sensor$',names(ship),val=T);
  sensorlevels <- mget(sensornames,ship);
  # calculate rr, the distance at which the currently best sensor's accuracy drops to 0.
  rr <- sapply(sensorlevels,sensorMaxRange) %>% max(na.rm=T);
  spheres3d(ship$coords[1],ship$coords[2],ship$coords[3],col='lightgreen',alpha=0.2,radius=rr);
  # TODO: planets can actually be plotted using spheres3d and given a texture argument which 
  #       points to a path to a .png file. Type = 'rgb' or 'rgba' or 'luminance' or 'alpha'
  # 
  with(ship$log,lines3d(x=x,y=y,z=z,col='blue'));
  if(!is.null(ship$lastdestination)){
    points3d(x=ship$lastdestination[[1]],y=ship$lastdestination[[2]],z=ship$lastdestination[[3]],size=size*1.5,color='green')};
  rglids;
}

rglCoordCapture <- function(rglids){
  out <- c(0,0,0); thisenv <- environment();
  selectpoints3d(rglids['data'],multiple=function(xx){
    print(xx);
    thisenv$out <- rbind(thisenv$out,xx);
  });
  out[-1,]; 
}

parseVectorStrings <- function(xx,match=c()){
  sapply(xx,function(xx) eval(parse(text=paste0('c(',xx,')'))),simplify=F) %>% 
    sapply(function(xx) if(is.null(xx)) 1 else prod(xx[intersect(names(xx),match)]));
}

nudgeShip <- function(ship,tolerance=1e6){
  almosthere <- prepStarChart(ship) %>% filter(distance>0 & distance < 1e-6);
  if(NROW(almosthere)==0) return();
  moveShip(ship,almosthere[1,c('x','y','z')])};

# big functions ----
newShip <- function(...) {
  # Create list to store ship variables
  args <- list(...);
  ship <- list(
    # Set default values for variables
    water_sensor = 50,atmosphere_sensor = 50,gravity_sensor = 50,temperature_sensor = 50,resources_sensor = 50
    ,colonists = 10000
    ,dbase = 50,equipment = 100,landing_gear = 100,probes = 10
    ,coords = c(0, 0, 0)
    ,state = 'space'
    ,name = 'Earthseed'
    ,planetLocalDB = data.frame(
      x = numeric(0), y = numeric(0), z = numeric(0),
      visited = integer(0), info = character(0),
      water_sensor = numeric(0), atmosphere_sensor = numeric(0),
      gravity_sensor = numeric(0), temperature_sensor = numeric(0),
      resources_sensor = numeric(0), stringsAsFactors = FALSE));
  
  # Evaluate named arguments and insert them into output
  for (ii in names(args)) {
    ship[[ii]] <- args[[ii]]};
  ship <- as.environment(ship);
  class(ship) <- 'staRship';
  shipLog('${name} begins its voyage!',tags='start');
  scanPlanets(ship);
  return(ship);
};


sendProbes <- function(ship){
  nudgeShip(ship);
  nearbyplanet <- prepStarChart(ship,NA) %>% filter(distance==0);
  if(NROW(nearbyplanet)==0){
    warning('Not near a planet, cannot send probes');
    return()};
  if(nearbyplanet$visited == 2){
    warning('No probes sent because ',ship$name, ' already sent probes to this planet');
    return(nearbyplanet$info[[1]]);
  }
  if(ship$probes < 1){
    warning('No probes sent because ',ship$name,' has run out of probes!');
    return();
  }
  # get the accurate planet info
  fullplanetinfo <- right_join(planetGlobalDB,nearbyplanet[,c('x','y','z')])$info[[1]];
  # but only the anomaly and code fields are needed... now the more accurate ones
  fullplanetinfo$anomalies <- transmute(fullplanetinfo$anomalies,anomaly=details,code=code);
  # overwrite the old planet info with the new in the ship database
  ship$planetLocalDB[with(ship$planetLocalDB,x==nearbyplanet$x,y==nearbyplanet$y,z==nearbyplanet$z),'info'][[1]] <- list(fullplanetinfo);
  ship$planetLocalDB[with(ship$planetLocalDB,x==nearbyplanet$x,y==nearbyplanet$y,z==nearbyplanet$z),'visited'] <- 2L;
  ship$probes <- ship$probes - 1;
  shipLog('${name} has sent probes to explore a new planet. ${probes} remain.',tags='probes');
  # TODO: come up with a more dramatic exposition when planet anomalies are done
  return(fullplanetinfo);
}

# The higher the eventweight, the fewer events. Think of it as the ship speed--
# the faster it goes, the less chance of running into events
moveShip <- function(ship, target,eventweight=4) {
  # TODO: if called from nudgeShip don't scan and maybe abbreviated log
  if(missing(target)){
    if(is.null(ship$lastdestination)){
      warning('No previous destination set. ',ship$name,' remains where it is.');
      return();
    } else target <- ship$lastdestination;
  } 
  if(length(target)!=3){
    warning('Invalid coordinates: coordinates must be a vector of three numeric values');
    return()};
  ship$lastdestination <- target;
  # Calculate distance between ship and target
  rr <- p3Distance(ship$coords,target);
  
  # Generate random number from exponential distribution
  ss <- rexp(1,rate=1/eventweight);
  
  # Calculate unit vector pointing in direction of line from ship to target
  unit_vector <- (target - ship$coords) / rr;
  
  # Calculate displacement vector by multiplying unit vector by ss
  displacement <- unit_vector * ss;
  
  shipLog(target=target,preset = 'depart',tags='auto');
  
  # Update ship's coords and state
  if(ss < rr){
    ship$coords <- ship$coords + displacement; ship$state <- 'event';
    shipLog(preset='event');
    scanPlanets(ship);
  } else {
    ship$coords <- target; ship$state <- 'space';
    shipLog(preset='arrive',tags='auto');
    ship$lastdestination <- NULL;
    scanPlanets(ship);
  };

  # return updated object
  return(ship);
};


# Helper function to generate random event node
generateEventNode <- function(maxDepth, depth = 1,description='MISSING') {
  if (maxDepth > 0) {
    # Generate random number of child nodes
    numNodes <- sample(1:maxDepth, 1);
    nodes <- list();
    choices <-  paste0("You decide to ", sample(c("attempt to communicate with the object", "ignore the object and continue on your way", "try to negotiate with the pirates", "try to outrun the pirates", "fight the pirates", "try to land on the planet and explore", "continue on your way"), numNodes), ".");
    for (ii in 1:numNodes) {
      # Generate random description for non-terminal node
      nodes[[choices[ii]]] <- generateEventNode(maxDepth - 1, depth + 1, description=choices[ii]);
    }
    if (depth == 1) {
      # Generate random description for top-level non-terminal node
      description <- paste("You come across a ", sample(c("strange object", "group of space pirates", "hostile planet"), 1), ". What do you do?")
    }
    c(list(description = description), nodes)
  } else {
    # Generate random named numeric vector of unique ship variables and random description for terminal node
    variables <- sample(c("sensors", "colonists", "dbase", "equipment", "landing_gear", "probes"), sample(1:6, 1), replace = FALSE)
    outcomes <- sapply(variables,function(xx) sample(1:10,1),simplify=F);
    description <- paste("As a result, you gain/lose the following: ", paste(variables, collapse = ", "))
    list(description = description, outcomes=outcomes);
  }
}


# Helper function to navigate event hierarchy
navigateEvent <- function(eventData) {
  if (! 'outcomes' %in% names(eventData)) {
    # If eventData is a list, print description and numbered list of choices
    if ("description" %in% names(eventData)) {
      print(eventData$description)
    }
    choices <- names(eventData)[!names(eventData) %in% c("description")];
    choiceslist <- paste0(seq_along(choices),': ',choices,collapse='\n');
    choiceNum <- -1;
    while(! choiceNum %in% seq_along(choices)){
      cat(choiceslist);
      # Prompt user to choose a question
      choiceNum <- as.numeric(readline("Enter the number of your choice: "))
    };
    
    # Navigate to chosen choice
    navigateEvent(eventData[[choices[choiceNum]]])
  } else {
    # If eventData is not a list, it is a terminal node containing a named numeric vector of ship variables
    if ("description" %in% names(eventData)) {
      print(eventData$description)
    }
    eventData$outcomes;
  }
}

generateAnomaly <- function(planetinfo,anomdata=anomalies) {
  planetstats<-planetinfo[c('water','atmosphere','gravity','temperature','resources')] %>% unlist %>% gsub(' ','_',.);
  planettags <- planetinfo$anomalies$tags; if(is.null(planettags)) planettags <- '';
  anomdata <- mutate(anomdata
                     # convert statprobs to numeric vectors, filter by values, and get aggregate planet-specific probability modifier
                     ,statprobs=parseVectorStrings(statprobs,planetstats)
                     # same but use tags (currently, from previous anomalies) instead of planet statistic values
                     ,tagprobs=parseVectorStrings(tagprobs,planetinfo$anomalies$tags)
                     # TODO: one of these for planet numeric statistics
                     # the the final probability by multiplying base probability by the two modifiers
                     ,finalprob=baseprob*statprobs*tagprobs
                     )
  anomaly <- anomdata[sample(seq_len(nrow(anomdata)), 1, prob = anomdata$finalprob),];
}


generatePlanet <- function(maxAnomalies=9
                           # uniquefeatures means max one of each per planet
                           ,uniqueFeatures = c("Moon", "Alien Observers")
                           ,params = planetparams,...) {
  args <- list(...);
  # Set default values for the variables
  out <- list(
    atmosphere = sample(params$atmosphere, 1)
    ,gravity = runif(1, params$gravity[1],params$gravity[2])
    ,temperature = runif(1, params$temperature[1], params$temperature[2])
    ,water = sample(params$water, 1)
    ,resources = sample(params$resources, 1)
    ,description = sample(c("A beautiful planet with stunning landscapes.", "A desolate planet with little vegetation.", "A tropical planet with lush forests and white sandy beaches.", "A cold planet with icy tundras and frozen oceans."), 1));
  
  # Set the number of times to call the generateAnomaly function
  num_anomalies <- sample(0:maxAnomalies, 1);
  # We loop over them the old fashioned way because each anomaly alters the probabilities
  # of subsequent anomalies
  out$anomalies <- data.frame(anomaly=character(0),code=character(0));
  while(NROW(out$anomalies)<=num_anomalies) out$anomalies <- rbind(out$anomalies,generateAnomaly(out));
  out$anomalies <- unique(out$anomalies);
  # Set the actual visibilities for this planet's instances of the anomalies
  out$anomalies$visibility <- 1*(runif(NROW(out$anomalies))>out$anomalies$p_hidden);
  # TODO: instead of the below, just use tags for one-of-a-kind anomalies
  # out$anomalies <- replicate(num_anomalies,generateAnomaly(out),simplify=F) %>% 
  #   c(list(data.frame(eventID=character(0),visibility=logical(0))),.) %>% 
  #   bind_rows %>% group_by(eventID) %>% 
  #   # Filter the anomalies data frame to remove duplicate eventID values
  #   filter(row_number() == 1 | !(eventID %in% uniqueFeatures)) %>% ungroup();

  # Replace default values with specified values if they are provided
  for(ii in names(args)) out[[ii]] <- args[[ii]];
  out;
}

genPlanets <- function(coords, dist = 9, maxp = 50) {
  # Count the number of existing planets within the specified distance of the coordinates
  existing_planets <- withinDist(coords,dist) %>% NROW;
  # Calculate the number of new planets to generate
  nn <- maxp - existing_planets;
  # Exit the function if no new planets need to be generated
  if (nn <= 0) return();
  # Set the number of new planets to a random integer between 0 and nn
  if((num_new_planets <- sample(0:nn, 1))==0) return();
  # generate potential locations for these planets
  if(num_new_planets==1) num_new_planets<- 2;
  new_planets <- sapply(coords
                        ,function(xx) runif(num_new_planets,xx-dist,xx+dist)) %>% 
    as.data.frame %>% setNames(c('x','y','z'));
  # find locations that are not too close to any already discovered planet
  new_planets_sufficientlyfar <- apply(new_planets,1,function(xx) NROW(withinDist(xx,dist)))==0;
  # keep only planets that are not too close, if any
  new_planets <- new_planets[new_planets_sufficientlyfar,];
  if(NROW(new_planets)==0) return();
  new_planets$info <- I(replicate(nrow(new_planets),generatePlanet(),simplify=F));
  return(new_planets);
}

currentPlanet <- function(ship){
  
}

scanPlanets <- function(ship){
  # name of the sensor variables
  sensornames <- grep('_sensor$',names(ship),val=T);
  # calculate rr, the distance at which the currently best sensor's accuracy drops to 0.
  sensorlevels <- mget(sensornames,ship);
  rr <- sapply(sensorlevels,sensorMaxRange) %>% max(na.rm=T);
  planetGlobalDB <<- rbind(planetGlobalDB,genPlanets(ship$coords,dist=rr));
  # select all the entries from planetGlobalDB whose x, y, and z values are a distance from ship$coords that is <= rr
  planets_scanned <- withinDist(ship$coords,rr);
  planets0 <- left_join(planets_scanned,ship$planetLocalDB,by=c('x','y','z')
                   ,suffix=c('_global',''));
  shipLog(paste0('Total systems in scanner range: ',NROW(planets0)),tags=c('auto','scan'));
  if((planets_discovered<-NROW(subset(planets0,is.na(visited))))>0){
    shipLog(paste0('Discovered ',planets_discovered,' new systems.'),tags='discovery')};
  # Calculate distance and accuracy for each sensor at that distance. 
  planets1 <- p3Distance(planets0[,c('x','y','z')],ship$coords) %>% 
    cbind(as.data.frame(sensorlevels),distance=.) %>% 
    mutate(across(ends_with('_sensor'),~sensorAccuracy(.x,distance))) %>% 
    cbind(extractPlanetInfo(planets0$info_global)) %>% 
    mutate(across(all_of(names(planetparams)) & where(is.character)
                           ,~nominalNoises(.x
                                    ,cur_data()[[paste0(cur_column(),'_sensor')]]
                                    ,cur_column()))
           ,across(all_of(names(planetparams)) & where(is.numeric)
                   ,~numericNoises(.x
                                   ,cur_data()[[paste0(cur_column(),'_sensor')]]
                                   ,cur_column()))
           ) %>% 
    # rename the sensor columns so they don't collide with planets0
    rename_at(vars(ends_with("_sensor")), funs(paste0(., "_current")));
  # final stage of planet data-- combine everything, precalculate update decisions
  planets2 <- cbind(planets0,planets1);
  for(ii in seq_len(NROW(planets2))){
    iiinfo <- planets2[ii,'info'][[1]];
    if(!is.list(iiinfo)) iiinfo <- list();
    iiupd <- FALSE;
    # for each of water, atmosphere, gravity, temperature, and resource compare the existing sensor accuracy to the new one. 
    # If the new one is higher, overwrite old value and accuracy with the new ones. 
    # Otherwise skip to the next. 
    for(jj in statnames){
      oldjjsn <- paste0(jj,'_sensor'); newjjsn <- paste0(oldjjsn,'_current');
      if(is.null(iiinfo[[jj]])||coalesce(planets2[ii,oldjjsn],0)<planets2[ii,newjjsn]){
        iiupd <- TRUE; # at least one value needed to be updated
        # update the value in the extracted planet-info
        planets2[ii,oldjjsn] <- planets2[ii,newjjsn];
        # update the appropriate _sensor column to indicate what the sensor 
        # accuracy was at the time this reading was obtained
        iiinfo[[jj]] <- planets2[ii,jj];
      }
    }
    if(iiupd) planets2[ii,'info'][[1]] <- list(I(iiinfo));
  }
  class(planets2$info) <- 'AsIs';
  planets2$visited <- coalesce(planets2$visited,0L);

  # If the distance is 0 and the visited column is 0...   
  currentPlanet <- with(planets2,which(visited==0 & distance == 0));
  if(length(currentPlanet)>0){
    if(length(currentPlanet)>1) stop('Cannot be in multiple systems at the same time');
    #  Set the visited column to 1
    ship$state <- 'planet';
    planets2[currentPlanet,'visited'] <- 1L;
    #  Populate the description of the current planetLocalDB info entry with first value of description from the corresponding planetGlobalDB info entry 
    #planets2[currentPlanet,'info'][[1]][[1]]$description<- planets2[currentPlanet,'info_global'][[1]][[1]]$description[1];
    #planets2[currentPlanet,'info'][[1]]$description <- planets2[currentPlanet,'info_global'][[1]]$description[1]
    #     planets2[currentPlanet,'info'][[1]][[1]]$anomalies <- planets2[currentPlanet,'info_global'][[1]][[1]]$anomalies %>% filter(visibility==1);
    currentPlanetInfo <- planets2[currentPlanet,'info'][[1]];
    pdesc <- currentPlanetInfo$description <- planets2[currentPlanet,'info_global'][[1]]$description;
    currentPlanetInfo$anomalies <- planets2[currentPlanet,'info_global'][[1]]$anomalies %>% 
      filter(visibility==1) %>% select(all_of(c('anomaly','code'))) %>% mutate(code=NA);
    planets2[currentPlanet,'info'][[1]]<- list(currentPlanetInfo);
    #  Select the anomalies from the corresponding planetGlobalDB info entry such that visibility > 0, and use the resulting data.frame to populate the anomalies of the current planetLocalDB info entry. 
    shipLog(paste0('${name} is orbiting: \n',pdesc),tags='starsystem');
  }
  # add new planets and update old ones in the planetLocalDB of the ship
  ship$planetLocalDB <- rbind(anti_join(ship$planetLocalDB,planets2,by=c('x','y','z'))
                                        ,planets2[,names(ship$planetLocalDB)]);
}

planetReport <- function(ship){
  starchart <- prepStarChart(ship,NA) %>% subset(grepl('\\*'));
  
}


foo <- newShip();
# basic event-loop sketch ----
# to get a 3d starmap
ids <- rglStarChart(foo);
# ids can then be used to catch mouse clicks
moveShip(foo,suppressWarnings(rglCoordCapture(ids)));ids<-rglStarChart(foo);
# To keep moving after event interruption...
moveShip(foo);ids<-rglStarChart(foo);
promptNav(foo);


# TODO 
# * launch probes
# * events
# * colonization
# * shiny integration

# if stops on event:
# do event and then...
# What does it mean to 'do' an event?
# 1. Present node text.
# 2. If effects present, run code to apply them
# 2a. If manual choices, present them and collect user input.
# 2b. If random choices, run code to select.
# ... repeat at next node
# ...when completing leaf node: just exit and maybe set state to 'space'
#foo <- moveShip(foo,.target);
# if stops on planet:
# then choose whether to send probes, colonize, or moveShip again.


NULL
