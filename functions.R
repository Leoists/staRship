library(dplyr);
library(stringr);
library(rgl);
library(rio);
library(data.tree);

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


#. code for event-tree ----
repair_drone <- Node$new('repair_drone',description=function(ship,node,...) c('A probe from your homeworld finally caught up with ${ship$name}.', 'It is offering to perform repairs')
                         ,effect= function(ship,node,...) list(tags=c(testA=1,testB=-1,testC=2.3)
                                                               ,eventtags=c(evA=2.3,evB=1,evC=-1)
                                                               ,shipstats=list(min_damage=c(probes=0,landing_gear=-5,equipment=-20),max_damage=c(landing_gear=20,default=5),probs=c(gravity_sensor=2),damageable=c('landing_gear','equipment','gravity_sensor','water_sensor'),max_systems=4)
                         )
                         ,nchoices = function(ship,node,...) if(isTRUE(ship$tags$test1choice>0)) length(node$children)-1 else valueModFromTags(unlist(c(ship$tags,ship$eventtags)),c(evB=1.5,evC=0,evD=0.1,evA=1.1,testC=2.1),mod=`+`,aggmod=sum)
                         ,decideOutcome=function(ship,thisnode,nextnode,...){browser()})
repair_drone$AddChild('Ignore the message from the probe'
                      ,baseprob=function(ship,node,...) 1,probmods=function(ship,node,...) c(testA=1.1,testB=0,testC=0.5)
                      ,choicetext=function(ship,node,...) {'${ship$name} ignores message'} ,description='You ignore the message from the probe and maintain course',finish=T)
repair_drone$AddChild('Rendezvous with the probe',description=function(ship,node,...) {browser()},decideOutcome=function(ship,thisnode,netnode,...) {browser()},finish=T)
repair_drone$AddChild('Flip a coin',description=function(ship,node,...) {browser()},decideOutcome=function(ship,thisnode,netnode,...) {browser()},finish=T)
# more ambitious event-tree
test_event <- Node$new('test_event'
  ,description=function(ship,node,...){
    # find nearest planet
    nearestplanet <- prepStarChart(ship,NA) %>% subset(distance==min(distance));
    # we're going to refer to this planet later on in the event, so record it in
    # the ship's eventcache 
    ship$eventcache$nearestplanetcoords <- nearestplanet[,c('x','y','z')];
    # randomly choose the type of message to receive
    messagetype <- sample(c('simple_message','complex_message','noisy_message'),1);
    messageintent <- sample(c('hostile','friendly'),1);
    ship$eventtags[[paste0(messagetype,'_',messageintent)]] <- 1;
    messagetypedisplay <- switch (
      messagetype
      ,simple_message = '  starts with a sequence of prime numbers, then some universal mathematical constants, eventually working its way up to the basics of decoding the language of its authors'
      ,complex_message = ' is clearly artificial and and intended to convey information. However it is so alien and complex that you cannot decode most of it'
      ,noisy_message = ' is clearly artificial and and intended to convey information but is fragmented and corrupted so you only get a few snippets of contiguous data'
    );
    # randomly roll whether to append a hostile or friendly intent, or nothing at all.
    # the probability of the wrong interpretation is higher for complex message and of a missing interpretation if higher for a noisy message
    # use valueModFromTags() for this
    # make a message with interpolated values 
    out <-
"After taveling ${10*ship$traveled} light year, the ${name} detects a 
transmission coming from a system at ${formatCoords(ship$eventcache$nearestplanetcoords)}.
It ${messagetypedisplay}."
    str_interp(out) %>% str_wrap(width = 80);
  }
  ,choices=function(ship,node,...){
    out <- thisnode$children;
    # generate a dynamic choice
    # randomly remove one of the choices
    }
  ,outcome=function(ship,node,...){
    browser();
    # add permanent tags to ship
    # modify the nearest planet to have a high-tech civ
    # tag that planet hostile or friendly
  }
);


# small helper functions ----
# To get values nested several levels deep, e.g. from a list-valued column
excavateList <- function(xx){
  thisfun <- sys.function();
  if(!is.list(xx)||length(xx)>1) return(xx);
  thisfun(xx[[1]]);
}
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

stringToNameVal <- function(xx,outersep=',',innersep='=',valtransform=as.numeric,nameformat=str_trim,format=identity){
  str_split_1(xx,outersep) %>% str_split(innersep) %>%
    sapply(function(yy) setNames(as.numeric(yy[2]),nameformat(yy[1]))) %>% 
    format;
}

# ... is a series of atomic name-value pairs alternatively
# the newtags argument can be used with a named vector,
# or both
# updateTags will check for each name in obj[[taglist]]
# if it doesn't exist, it initializes it to that 
# tags's value, if it exists, it adds that tag's value
# unless the modfun is set to some other binary 
# operator than `+` in which case it uses that
# limits sets the cap on the number of tag instances
updateTags <- function(taglist,newtags=c(),mod=`+`,limits=c(0,Inf),trimzeros=T,...){
  if(is.null(names(newtags)) && length(newtags)==1 && 
     is.character(newtags) && grepl('=',newtags)){
    newtags <- stringToNameVal(newtags)};
  newtags <- c(as.list(newtags),list(...));
  if(is.null(names(newtags))) return(taglist);
  for(ii in intersect(names(newtags),names(taglist))){
    taglist[[ii]] <- mod(taglist[[ii]],newtags[[ii]])};
  for(ii in setdiff(names(newtags),names(taglist))){
    taglist[[ii]] <- newtags[[ii]]};
  taglist <- lapply(taglist,function(xx) pmin(pmax(xx,limits[1]),limits[2]));
  if(trimzeros) taglist <- Filter(function(xx) xx!=0,taglist);
  return(taglist);
};

# weights is a named vector of how much each occurrence 
# of a tag will influence the base value. 
# For example c(ugly = 2,pretty=0.5) each occurrence of
# an ugly tag doubles the baseline value while each 
# occurence of a pretty tag halves it. Tags is a named
# vector of tag occurences. For example, c(ugly=1,pretty=2)
# represents one 'ugly' tag and two 'pretty' tags. A 
# different binary operator can be specified in mod so that 
# additional instances of a tag have an e.g. additive 
# effect. Likewise aggmod-- except that one needs to be a
# function that takes a numeric vector and returns a scalar
valueModFromTags <- function(weights,tags,mod=`^`,aggmod=prod){
  tagnamesfound <- intersect(names(weights),names(tags));
  if(length(tagnamesfound)==0) return(1);
  aggmod(mod(weights[tagnamesfound],tags[tagnamesfound]));
}

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
           description<-'${name} departs from $[.3f]{coords}';
           tags<- c(tags,'transit')}
         ,arrive={
           description<-'${name} arrives at $[.3f]{coords}';
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
  if(NROW(ship$planetLocalDB)==0) scanPlanets(ship);
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
  data <- prepStarChart(ship,NA)[,c('x','y','z')];
  ngrid <-apply(data,2,function(xx) c(range(xx)+c(-1,1),median(range(xx)))) %>% as.data.frame() %>% expand.grid();
  #ngrid <- expand.grid(x=c(-1:1),y=c(-1:1),z=c(-1:1)) + c(ship$coords) %>% setNames(c('x','y','z'));
  combineddata <- rbind(data,ngrid);
  rglids<-with(combineddata,plot3d(x=x,y=y,z=z,size=1,alpha=0.1));
  with(data,points3d(x=x,y=y,z=z,size=size,alpha=1,color='black'));
  with(ngrid,points3d(x=x,y=y,z=z,col='purple',size=8,alpha=0.2));
  points3d(x=ship$coords[1],y=ship$coords[2],z=ship$coords[3],size=size*2,color=herecol);
  sensornames <- grep('_sensor$',names(ship),val=T);
  sensorlevels <- mget(sensornames,ship);
  # calculate rr, the distance at which the currently best sensor's accuracy drops to 0.
  rr <- sapply(sensorlevels,sensorMaxRange) %>% max(na.rm=T);
  # navigation grid, to travel toward empty space if there are no visible planets
  #points3d(ngrid,col='purple',size=3,alpha=0.5);
  spheres3d(ship$coords[1],ship$coords[2],ship$coords[3],col='lightgreen',alpha=0.2,radius=rr);
  # TODO: planets can actually be plotted using spheres3d and given a texture argument which 
  #       points to a path to a .png file. Type = 'rgb' or 'rgba' or 'luminance' or 'alpha'
  # 
  with(ship$log,lines3d(x=x,y=y,z=z,col='blue',lwd=2));
  if(!is.null(ship$lastdestination)){
    points3d(x=ship$lastdestination[[1]],y=ship$lastdestination[[2]],z=ship$lastdestination[[3]],size=size*1.5,color='green')};
  bg3d(fogtype='linear',fogScale=1.3,color='white');
  rglids;
}

rglCoordCapture <- function(rglids){
  out <- c(0,0,0); thisenv <- environment();
  selectpoints3d(rglids['data'],multiple=function(xx){
    print(xx);
    thisenv$out <- rbind(thisenv$out,xx);
  });
  out[2,]; 
}

parseVectorStrings <- function(xx,match=c()){
  sapply(xx,function(xx) eval(parse(text=paste0('c(',xx,')'))),simplify=F) %>% 
    sapply(function(xx) if(is.null(xx)) 1 else prod(xx[intersect(names(xx),match)]));
}

nudgeShip <- function(ship,tolerance=1e-5){
  almosthere <- currentPlanet(ship,tolerance);
  #almosthere <- prepStarChart(ship) %>% filter(distance>0 & distance < tolerance);
  if(NROW(almosthere)==0 || almosthere$distance==0) return(almosthere);
  shipLog(str_interp("Minor orbital correction to $[.5f]{almosthere[1,c('x','y','z')]}"));
  moveShip(ship,almosthere[1,c('x','y','z')],eventweight = 100,dologs=F,doscans=F);
  ship$state <- 'planet';
  return(almosthere);
  };

damageShip <- function(ship,max_systems=3
                       # all of these arguments can be named vectors with one
                       # item named 'default'. In those cases, damageable items
                       # matching a name get that value, the rest get the 
                       # default value. If there is no default specified,
                       # 0, 10, and 1 are used for min_damage, max_damage, and 
                       # probs, respectively. You can also set min_damages to 
                       # negative and max_damage to 0, so use this function to
                       # undo damage (i.e. repair or improve) the ship. Or set
                       # min_damage to negative and max_damage to positive if
                       # it might go either way.
                       ,min_damage=0,max_damage=10,probs=1
                       ,damageable=c('planetLocalDB','probes','landing_gear'
                                    ,'equipment','dbase','colonists'
                                    ,'resources_sensor','temperature_sensor'
                                    ,'gravity_sensor','atmosphere_sensor'
                                    ,'water_sensor','colonists')
                       ){
  # number of different systems damaged
  n_systems <- sample(1:min(max_systems,length(damageable)),1);
  n_actual_systems <- 
  # Expand min_damage argument to the length of damageable. If no values named 
  # 'default', use 0. Then overwrite any matching named values.
  if(!is.null(names(min_damage))){
    instance_min_damage <- replicate(length(damageable),coalesce(min_damage['default'],0)) %>% 
      setNames(damageable);
    instance_min_damage <- intersect(damageable,names(min_damage)) %>% 
      {instance_min_damage[.] <- min_damage[.]; instance_min_damage};
  } else {
    if(length(min_damage)>1 & length(min_damage) < length(damageable)) warning('In damageShip() the length of the min_damage argument is > 1 without specifying any system names. Will use the first value and ignore all the rest.');
    instance_min_damage <- replicate(length(damageable),min_damage) %>% setNames(damageable);
  };
  # Do the same for max_damage, using 10 if default missing.
  if(!is.null(names(max_damage))){
    instance_max_damage <- replicate(length(damageable),coalesce(max_damage['default'],10)) %>% 
      setNames(damageable);
    instance_max_damage <- intersect(damageable,names(max_damage)) %>% 
      {instance_max_damage[.] <- max_damage[.]; instance_max_damage};
  } else {
    if(length(max_damage)>1 & length(max_damage) < length(damageable)) warning('In damageShip() the length of the max_damage argument is > 1 without specifying any system names. Will use the first value and ignore all the rest.');
    instance_max_damage <- replicate(length(damageable),max_damage) %>% setNames(damageable);
  };
  if(any(instance_max_damage < instance_min_damage)){
    stop('In the damageShip() function, the min_damage and max_damage were specified such that the max_damage was lower than the min_damage.')};
  # Do the same for probs, using 1 if default missing
  if(!is.null(names(probs))){
    instance_probs <- replicate(length(damageable),coalesce(probs['default'],1)) %>% 
    setNames(damageable);
    instance_probs <- intersect(damageable,names(probs)) %>% 
      {instance_probs[.] <- probs[.]; instance_probs};
  } else {
    if(length(probs)>1 & length(probs) < length(damageable)) warning('In damageShip() the length of the probs argument is > 1 without specifying any system names. Will use the first value and ignore all the rest.');
    instance_probs <- replicate(length(damageable),probs) %>% setNames(damageable);
  };
  # sample the actual systems that will be damaged from damageable using probs
  # for each system, roll damage using its min_damage, max_damage
  damaged <- sample(damageable,n_systems,prob=instance_probs) %>% 
    {.x <- (.); 
    runif(length(.x),instance_min_damage[.x],instance_max_damage[.x]) %>% 
      setNames(.x)};
  # if probes is one of the systems damaged, set damage to 1
  if('probes' %in% names(damaged)) damaged['probes'] <- sign(damaged['probes']);
  # if planetLocalDB is damaged, calculate damage as a fraction of its max_damage
  # and randomly remove that fraction of entries from planetLocalDB
  if('colonists' %in% names(damaged)) damaged['colonists'] <- round(damaged['colonists']);
  if('planetLocalDB' %in% names(damaged)){
    .planetLocalDBdmg <- pmin(damaged['planetLocalDB']/instance_max_damage['planetLocalDB'],1);
    if(.planetLocalDBdmg>0){
      ship$planetLocalDB <- slice_sample(ship$planetLocalDB
                                         , prop=1-.planetLocalDBdmg);
      shipLog(str_interp('The planet database was damaged and lost $[.2f]{.planetLocalDBdmg*100} of its data')
              ,tags='damage');
    }
    damaged <- damaged[c(setdiff(names(damaged),'planetLocalDB'))];
  };
  damaged <- damaged[damaged!=0];
  # for the other systems, subtract the damage from their current values and 
  if(length(damaged)>0){
    for(ii in names(damaged)){
      ship[[ii]] <- pmax(ship[[ii]] - damaged[ii],0);
      if(damaged[ii]>0){
        shipLog(str_interp('Damage to ${ii}: $[.2f]{damaged[ii]}'),tags='damage');
      } else {
        shipLog(str_interp('Improvement to ${ii}: $[.2f]{damaged[ii]}'),tags='upgrade');
      }
    }
  };
}

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
    ,traveled = 0
    ,planetLocalDB = data.frame(
      x = numeric(0), y = numeric(0), z = numeric(0),
      visited = integer(0), info = character(0),
      water_sensor = numeric(0), atmosphere_sensor = numeric(0),
      gravity_sensor = numeric(0), temperature_sensor = numeric(0),
      resources_sensor = numeric(0), 
      first_visited = as.POSIXct(NULL), last_visited = as.POSIXct(NULL),
      probed = as.POSIXct(NULL),
      stringsAsFactors = FALSE)
    );
  
  # Evaluate named arguments and insert them into output
  for (ii in names(args)) {
    ship[[ii]] <- args[[ii]]};
  ship <- as.environment(ship);
  class(ship) <- 'staRship';
  ship$self <- ship;
  shipLog('${name} begins its voyage!',tags='start');
  scanPlanets(ship);
  return(ship);
};


sendProbes <- function(ship){
  nearbyplanet <- nudgeShip(ship);
  #nearbyplanet <- prepStarChart(ship,NA) %>% filter(distance==0);
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
  ship$planetLocalDB[with(ship$planetLocalDB,x==nearbyplanet$x,y==nearbyplanet$y,z==nearbyplanet$z),'probed'] <- Sys.time();
  ship$probes <- ship$probes - 1;
  shipLog('${name} has sent probes to explore a new planet. ${probes} remain.',tags='probes');
  # TODO: come up with a more dramatic exposition when planet anomalies are done
  return(fullplanetinfo);
}

# The higher the eventweight, the fewer events. Think of it as the ship speed--
# the faster it goes, the less chance of running into events
moveShip <- function(ship, target,eventweight=4,dologs=T,doscans=T) {
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
  
  if(dologs) shipLog(target=target,preset = 'depart',tags='auto');
  
  # Update ship's coords and state
  if(ss < rr){
    ship$coords <- ship$coords + displacement; ship$state <- 'event';
    ship$traveled <- ship$traveled + ss;
    if(dologs) shipLog(preset='event');
    if(doscans) scanPlanets(ship);
  } else {
    ship$coords <- target; ship$state <- 'space';
    ship$traveled <- ship$traveled + rr;
    if(dologs) shipLog(preset='arrive',tags='auto');
    ship$lastdestination <- NULL;
    if(doscans) scanPlanets(ship);
  };

  # return updated object
  invisible(ship);
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

shipReport <- function(ship,...){
  # nicely formatted ship info
  #nearestplanet <- currentPlanet(ship);
  with(ship,switch(state,planet='orbiting ${currentPlanet(self)$info[[1]]$description}'
                  ,space=if(!exists('lastdestination')) 'drifting in space' else 'heading toward $[.3f]{lastdestination}'
                  ,event='experiencing an event') %>% 
         paste0('${name} is at $[.3f]{coords}, ', .) %>% str_interp() ) %>% 
    gsub('c\\(\\"','("',.) %>% message;
  mget(c('probes','landing_gear','equipment','dbase','colonists','resources_sensor','temperature_sensor','gravity_sensor','atmosphere_sensor','water_sensor','traveled'),ship) %>% 
    cbind %>% data.frame() %>% setNames('status') %>% print;
  cat('\n');
}

planetReport <- function(planetrow,...){
  # nicely formatted planet info, planetrow is an entire row from ship$planetLocalDB
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

currentPlanet <- function(ship,tolerance=1e-5,cols=NA){
  prepStarChart(ship,cols) %>% filter(distance <= tolerance);
}

scanPlanets <- function(ship,tolerance=1e-5,autonudge=T,...){
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
  
  if(min(planets1$distance) > 0 && min(planets1$distance)<tolerance){
    nudgeShip(ship);
    return(scanPlanets(foo,...));
  };
  
  shipLog(paste0('Total systems in scanner range: ',NROW(planets0)),tags=c('auto','scan'));
  if((planets_discovered<-NROW(subset(planets0,is.na(visited))))>0){
    shipLog(paste0('Discovered ',planets_discovered,' new systems.'),tags='discovery')};
  
  
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
  currentPlanet <- with(planets2,which(distance==0));
  if(length(currentPlanet)>0){
    if(length(currentPlanet)>1) stop('Cannot be in multiple systems at the same time');
    #  Set the visited column to 1
    ship$state <- 'planet';
    planets2[currentPlanet,'first_visited'] <- coalesce(planets2[currentPlanet,'first_visited'],Sys.time());
    planets2[currentPlanet,'last_visited']<-Sys.time();
    if(planets2[currentPlanet,'visited']==0) planets2[currentPlanet,'visited'] <- 1L;  

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

# Event loop ----
#' # Adventure Node
#' 
#' Node with the following contents
#' 
#' * id: numeric(1) 
#' * choicetext: character(1) vector with optional ${...} tags to evaluate in 
#'               calling ${ship$...} and ${node$...} will be available, others 
#'               might be too, discover which ones by trial and error 
#'               (and browser)
#' * description: character vector with optional ${...} tags to evaluate as 
#'                above. Currently, multiple lines are collapsed with '\n'
#' * baseprob: numeric(1) >= 0
#' * probmods: named numeric vector >= 0
#' * effect: list containing any combo of the following values
#'   * shipstats: named list containing any combination of arguments used by damageShip (except the ship argument)
#'   * tags (named integer vector)
#'   * eventtags (named integer vector)
#'   * eventcache (named list)
#' * nchoices: integer(1) between 0 and Inf
#' * dynchoices: function returning a list of nodes
#' 
#' Any of the above static values except choicetext can be replaced by a 
#' function that returns that type of object and takes as its first two 
#' arguments a ship object and a node object.
#' 
#' The node is processed in the following steps:
#' 
#' * Process and present the description
#' * Process the effect
#' * 


# note the reversal of node and ship!
prepNodeChoiceText <- function(node,ship,...){
  # If node$choicetext is a function, call it and retain the result otherwise 
  # retain node$choicetext
  choicetext <- if(is.function(node$choicetext)){
    node$choicetext(ship,node,...) } else node$choicetext; 
  if(is.null(choicetext)) choicetext <- node$name;
  str_interp(choicetext);
}

# note the reversal of node and ship!
calcNodeProb <- function(node,ship,...){
  # If node$baseprob is a function, call that function and retain the result 
  # otherwise just retain node$baseprob.
  baseprob <- if(is.function(node$baseprob)){
    node$baseprob(ship,node,...)} else node$baseprob; 
  if(is.null(baseprob)) baseprob <- 1;
  # If node$probmods is a function, call that function and retain the result 
  # otherwise just retain node$probmods
  probmods <- if(is.function(node$probmods)){
    node$probmods(ship,node,...)} else node$probmods; 
  # Obtain a tags list from ship$tags and ship$eventtags
  activetags <- unlist(c(ship$tags,ship$eventtags));
  # obtain the final probability of selection
  finalprob <- baseprob * valueModFromTags(probmods,activetags);
}

navigateEvent <- function(ship,node,...){
  # If description is a function, call that function and replace description 
  # with the result of that call.
  description <- if(is.function(node$description)){
    node$description(ship,node,...) } else node$description; 
  # interpolate dynamic values
  description <- paste0(description,collapse='\n') %>% str_interp();
  # this part may vary depending on UI
  message(description);
  #' If effect is a function, call that function and replace effect with the 
  #' result of that call.
  effect <- if(is.function(node$effect)){
    node$effect(ship,node,...) } else node$effect;
  # For each item in effect, modify the corresponding item in ship accordingly
  # tags affecting the ship only for the duration of this event
  ship$eventtags <- updateTags(ship$eventtags,newtags=effect$eventtags);
  # tags permanently affecting the ship (until negated by some future tag)
  ship$tags <- updateTags(ship$tags,newtags=effect$tags);
  # damage (or repair) ship
  do.call(damageShip,c(ship,effect$shipstats));
  # TODO: update ship$eventcache
  # If nchoices is a function, call that function replace nchoices with the 
  # result of that call
  nchoices <- if(is.function(node$nchoices)){
    node$nchoices(ship,node,...)} else node$nchoices;
  choices <- c(node$children
               ,if(is.function(node$dynchoices)){
                 node$dynchoices(ship,node,...)} else c());
  if(is.null(nchoices)) nchoices <- length(choices);
  # probabilistically limit choices if necessary
  if(nchoices < length(choices)){
    probs <- sapply(choices,calcNodeProb,ship=ship);
    choices <- sample(choices,nchoices,prob=probs);
  }
  # detect whether this is a terminal node
  if(length(choices)==0) {message('Done'); return()};
  # prepare choice names
  choicenames <- sapply(choices,prepNodeChoiceText,ship=ship);
  # this part varies depending on UI
  chosen <- 0;
  while(!chosen %in% seq_along(choicenames)){
    chosen <- menu(choicenames);
  }
  return(choices[[chosen]]);
}

shipEventLoop <- function(ship,node,...){
  cur_node <- node;
  while(!is.null(cur_node)){
    prev_node <- cur_node;
    cur_node <- navigateEvent(ship,cur_node,...);
  };
  # reset state
  if(ship$state == 'event') ship$state <- 'space';
  # clear event-specific objects
  ship$eventtags <- ship$eventcache <- NULL;
  message('Encounter concluded');
}

shipLoop <- function(ship,doStarChart=T,...){
  donext <- 'init';
  while(!is.null(donext) && donext!=''){
    if(ship$state=='event'){
      shipEventLoop(ship,repair_drone,...)};
    if(doStarChart) ids <- rglStarChart(ship);
    generalchoices <- c('Consult star database'
                        ,'Select new destination from 3D starmap'
                        ,'Ship report');
    if(!is.null(ship$lastdestination)){
      generalchoices <- c('Continue to current destination',generalchoices)};
    if(ship$state == 'planet' && nudgeShip(ship)$visited<2){
      generalchoices <- c(generalchoices,'Send probes to nearby planet')};
    donext <- select.list(generalchoices,title='What should we do next?');
    #browser();
    switch(donext
           ,`Continue to current destination`=moveShip(ship) # continue to current
           ,`Consult star database`=promptNav(ship) # star db
           ,`Select new destination from 3D starmap`={
              ids <- rglStarChart(ship);
              message('Click and drag on star-chart, trying to select only one point');
              #browser();
              newtarget <- rglCoordCapture(ids);
              moveShip(ship,newtarget);
              ids<-rglStarChart(ship);}
           ,`Ship report`=shipReport(ship)
           ,`Send probes to nearby planet`=sendProbes(ship) # probes
           );
    #browser();
  }
}

# import into the events data.frame
# TODO: ...but what will happen when I populate shipstats and friends with
#       named lists and vectors?
events <- import('events_test.xlsx') %>%
  mutate(across(!any_of(c('pathString','choicetext','parent','package','notes','shipstats','tags','eventtags','eventcache'))
                ,~sapply(.x,function(xx){
                  if(grepl('^function',as.character(xx))) eval(str2lang(xx)) else xx
                },simplify=F))
         ,across(all_of(c('shipstats','tags','eventtags','eventcache'))
                 ,~sapply(sprintf('list(%s)',coalesce(.x,'')),function(xx) eval(str2lang(xx)))));
events$effect <- rowwise(events) %>%
  mutate(effect=list(list(shipstats=shipstats,tags=tags
                          ,eventtags=eventtags,eventcache=eventcache))) %>%
  select(effect);




eventnodes <- select(events,all_of(c('pathString','choicetext','baseprob'
                                     ,'probmods','description'
                                     ,'effect_function','effect','nchoices'
                                     ,'dynchoices','package'))) %>% as.Node();
eventnodes$Do(function(node){
  iilist <- list();
  for(ii in intersect(node$attributes,names(events))) {
    iilist[[ii]] <- excavateList(node[[ii]])};
  if(is.function(iilist$effect_function)){
    iilist$effect <- iilist$effect_function};
  iilist$effect_function <- NULL;
  for(ii in names(iilist)) node[[ii]] <- iilist[[ii]];
  node$RemoveAttribute('effect_function',stopIfNotAvailable=F)});
# top-level nodes are chosen randomly one per event
eventnodes$nchoices <- 1;





foo <- newShip();
shipLoop(foo);
# basic event-loop sketch ----
# to get a 3d starmap
#ids <- rglStarChart(foo);
# ids can then be used to catch mouse clicks
#moveShip(foo,suppressWarnings(rglCoordCapture(ids)));ids<-rglStarChart(foo);
# To keep moving after event interruption...
#moveShip(foo);ids<-rglStarChart(foo);
#promptNav(foo);


# TODO 
# DONE launch probes
# DONE events
# * Create actual event-trees and top-level event-selector
# * colonization
# * shiny integration

# Design notes ----


NULL
