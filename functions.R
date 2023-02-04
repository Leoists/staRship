library(dplyr);
library(stringr);
library(rgl);
library(rio);
library(data.tree);
library(geometry);

# global params ----
statnames <- c('atmosphere','gravity','temperature','water','resources');

anomalies <- import('data/anomalies.tsv');

planetparams <- list(
  atmosphere = c("Breathable", "Marginal", "Non-breathable", "Toxic", "Corrosive", "No atmosphere")
  ,gravity = c(0.1, 5)
  ,temperature = c(0, 4000)
  ,water = c("Oceans", "Ice caps", "Planet-wide ocean", "Ice-covered surface", "Trace", "No water")
  ,resources = c("Rich", "Poor", "No resources")
);

# initialize the global planet database
planetGlobalDB <- data.frame(x=numeric(0),y=numeric(0),z=numeric(0),info=I(list()),sector=character(0));

sector_name_grid <- import('data/constellation_names.tsv',header=F);

# small helper functions ----

# 
distanceToDest <- function(ship){c(dist(rbind(ship$coords,ship$lastdestination)))};

# Takes the midpoint between two locations and randomly perturbs it
randomMidPoint <- function(coords,destination,scale=10){
  points <- rbind(coords,destination);
  midpoint <- colSums(points)/2;
  distance <- dist(points);
  deviate <- runif(3,-1,1)*scale/distance;
  midpoint+deviate;
}

# Give sphCellNames a matrix or data.frame of string values as its 
# sectors argument. Based on a set of
# Cartesian 3D coordinates, it will tell you what sector those coordinates are 
# in. If include_subsectors (default) it will also append a letter designation 
# for a subsector based on radial distance from 0,0,0. Non-zero origin not yet
# implemented
sphCellNames <- function(coordinates,sectors = sector_name_grid
                         ,include_subsectors = TRUE
                         ,warnings=FALSE, origin = c(0, 0, 0)) {
  # insure coordinates are a 3-column matrix
  if(is.data.frame(coordinates)){
    coordinates <- if(all(c('x','y','z') %in% names(coordinates))){
      coordinates[,c('x','y','z')]} else {
        coordinates[,1:3]
      };
    coordinates <- as.matrix(coordinates)};
  if(!is.matrix(coordinates) && length(coordinates)==3){
    coordinates <- unlist(coordinates) %>% matrix(nrow=1)}
  sph_coord <- geometry::cart2sph(coordinates);
  phi_cuts <- coalesce(as.numeric(colnames(sectors)),Inf);
  theta_cuts <- coalesce(as.numeric(rownames(sectors)),Inf);
  if(!all(between(phi_cuts,-pi,pi))){
    if(warnings) warning('No reliable column names in xy_cutpoints, auto-assigning');
    phi_cuts <- seq(-pi,pi,len=ncol(sectors)+1)[-1]};
  if(!all(between(theta_cuts,-1,1))){
    if(warnings) warning('No reliable row names in xy_cutpoints, auto-assigning');
    theta_cuts <- seq(-1,1,len=nrow(sectors)+1)[-1]};
  sector_names <- mapply(function(pp,tt){
    sectors[which.min(abs(tt-theta_cuts)),which.min(abs(pp-phi_cuts))]
  },sph_coord[,1],sph_coord[,2]);
  if(include_subsectors){
    sector_names <- paste(sector_names
                          ,LETTERS[sapply(sph_coord[,3],function(xx){
                            sum(xx > c(5,10^c(1:25)))})])};
  sector_names;
}


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

formatCoords <- function(coords,template='(%s)',numformat='%.3f'){
  sprintf(template,str_flatten_comma(sprintf(numformat,coords)))};

# formatCoords <- function(coords,template='("%s")',sep='", "',numformat='%.3f'){
#   sprintf(template,paste0(sprintf(numformat,coords),collapse=sep))};

# conditionalUpdate <- function(data,valname,oldsuffix='_sensor',newsuffix='_sensor_current',finalsuffix='_sensor_final',infocol='info'){
#   oldquality <- paste0(valname,oldsuffix);
#   newquality <- paste0(valname,newsuffix);
#   finalquality <- paste0(valname,finalsuffix);
#   data[[finalquality]] <- pmax(coalesce(data[[oldquality]],0),data[[newquality]]);
#   updates <- which(coalesce(data[[oldquality]],0) != data[[finalquality]]);
# }

shipLog <- function(description='${name} is at ${formatCoords(coords)}, carrying ${colonists} colonists and ${probes} probes.
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
           description<-'${name} departs from ${formatCoords(coords)}';
           tags<- c(tags,'transit')}
         ,arrive={
           description<-'${name} arrives at ${formatCoords(coords)}';
           tags<- c(tags,'arrival')}
         ,event={
           description<-'${name} experiences an event';
           tags<- c(tags,'event')}
  );
  # If a target is specified for a custom or depart log entry, append those 
  # coordinates
  if(preset %in% c('custom','depart') && !missing(target)){
    description <- paste(description, 'toward',formatCoords(target))}
  
  # Using the with(...) strategy instead of str_interp's env argument because
  # otherwise it can't find either formatCoords() or the description object
  description <- with(c(mget(ls(ship),ship),description=description)
                      ,str_interp(description));
  # description <- str_interp(description,mget(ls(ship),ship)) %>% 
  #   gsub('c\\(\\"','("',.);
  
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
  if(length(lastd<-ship$lastdestination)==0) knownsystems$dst <- '' else {
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
    # TODO: print a proper planet report here
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
  if(NROW(almosthere)==0 || almosthere$distance==0) return(almosthere);
  shipLog(str_interp("Minor orbital correction to ${formatCoords(almosthere[1,c('x','y','z')],numformat='%.5f')}"));
  #shipLog(str_interp("Minor orbital correction to $[.5f]{almosthere[1,c('x','y','z')]}"));
  moveShip(ship,almosthere[1,c('x','y','z')],eventweight = 100,dologs=F,doscans=F);
  ship$state <- 'planet';
  return(almosthere);
  };

# Convenient way to access groups of ship stats
getShipNumericStats <- function(ship,stats=c('sensors','dbs','lgeq','colonists','probes'),onlygt0=F,hideNA=F){
  if('sensors' %in% stats) stats <- c(setdiff(stats,'sensors'),grep('_sensor$',names(ship),val=T));
  if('dbs' %in% stats) stats <- c(setdiff(stats,'dbs'),c('dbase','planetLocalDB'));
  if('lgeq' %in% stats) stats <- c(setdiff(stats,'lgeq'),c('landing_gear','equipment'));
  stats <- unique(stats) %>% sapply(function(ii) if(is.data.frame(ship[[ii]])){
    nrow(ship[[ii]])} else as.numeric(unname(ship[[ii]])));
  if(onlygt0) stats <- stats[stats>0];
  if(hideNA) stats <- na.omit(stats) %>% c;
  stats;
}

damageShip <- function(ship,max_systems=3,min_systems=1
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
                       ,damageable=names(getShipNumericStats(ship))
                       ){
  if(length(damageable)==0){
    shipLog(str_interp('Nothing left to damage!'),tags='damage');
    # TODO: have damage spill over into hull instead
    return(c(nothing=0));
  }
  if(min_systems>max_systems) max_systems <- min_systems;
  if(length(intersect(damageable,c('sensors','dbs','lgeq')))>0){
    damageable <- names(getShipNumericStats(ship,damageable,hideNA = TRUE))};
  # number of different systems damaged
  n_systems <- sample(max(0,min_systems):min(max_systems,length(damageable)),1);
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
  .planetLocalDBdmg <- NULL;
  if('planetLocalDB' %in% names(damaged)){
    .planetLocalDBdmg <- pmin(damaged['planetLocalDB']/instance_max_damage['planetLocalDB'],1);
    if(.planetLocalDBdmg>0){
      ship$planetLocalDB <- slice_sample(ship$planetLocalDB
                                         , prop=1-.planetLocalDBdmg);
      shipLog(str_interp('The planet database was damaged and lost $[.2f]{.planetLocalDBdmg*100}% of its data')
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
  if(!is.null(.planetLocalDBdmg)) damaged['planetLocalDB'] <- .planetLocalDBdmg;
  return(damaged);
}

updateShip <- function(ship,tagsdb){
  # apply any tags that are dependent solely on the ship data 
  # e.g. traveled, 
}

# big functions ----
newShip <- function(name='Earthseed',...) {
  # Create list to store ship variables
  args <- list(...);
  ship <- list(
    # Set default values for variables
    shipid = digest::sha1(name,Sys.time())
    ,water_sensor = 50,atmosphere_sensor = 50,gravity_sensor = 50,temperature_sensor = 50,resources_sensor = 50
    ,colonists = 10000
    ,dbase = 50,equipment = 100,landing_gear = 100,probes = 10
    ,coords = c(0, 0, 0)
    ,state = 'space'
    ,name = name
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
  # TODO: give user opportunity to rename the planet by changing its name param
  #       ...including in planetGlobalDB!
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
  # always scanPlanets first, to catch the edge cases caused by the ship's 
  # planetLocalDB taking damage
  scanPlanets(ship);
  with(ship,switch(state,planet='orbiting ${currentPlanet(self)$info[[1]]$description}'
                  ,space=if(!exists('lastdestination')) 'drifting in space' else 'heading toward ${formatCoords(lastdestination)} $[.1f]{distanceToDest(self)} LY away'
                  ,event='experiencing an event') %>% 
         paste0('${name} is in the ${sphCellNames(coords)} sector at ${formatCoords(coords)}, ', .) %>% str_interp() ) %>% 
    gsub('c\\(\\"','("',.) %>% message;
  out <- cbind(getShipNumericStats(ship,c('colonists','probes','sensors','dbs','lgeq','traveled'))) %>% 
    as.data.frame %>% setNames('status') %>% print;
  cat('\n');
  out;
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
  sector <- apply(new_planets,1,sphCellNames);
  planet_names <- with(new_planets,abs(digest::digest2int(paste0(x,y,z)))) %>% 
    paste(sector,.);
  new_planets$info <- I(sapply(planet_names,function(xx) generatePlanet(name=xx),simplify=F));
  #new_planets$info <- I(replicate(nrow(new_planets),generatePlanet(name=accession_no),simplify=F));
  new_planets$sector <- sector;
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

planetReport <- function(ship,coords){
  if(!missing(coords)){
    if(length(coords)!=3||!is.numeric(coords)){warning('Invalid coordinates, ignored');return(NULL)};
    coords <- rbind(coords) %>% data.frame %>% setNames(c('x','y','z'));
    starchart <- inner_join(ship$planetLocalDB,coords);
    if(NROW(starchart) == 0){warning('No planet found at these coordinates, ignored'); return(NULL)};
  } else {
    nudgeShip(ship);
    starchart <- prepStarChart(ship,NA) %>% subset(distance==0);
  }
  print(starchart);
}

renamePlanet <- function(coords,name='New Name',...){
  # screw it... for now, if you're colonizing a planet you get to rename that planet
  # validate name -- no cuss words, no code injection
  # pull out planets from planetGlobalDB and planetLocalDB.
  # localplanet$name <- globalplanet$name <- name;
}

colonizePlanet <- function(ship,...){
  planet <- currentPlanet(ship);
  #if(nrow(planet)==0)
  # If specified, add planet name to planetGlobalDB (and planetLocalDB probably)
  # Add a note to planetGlobalDB saying whom it was settled by and when
  # max_damage <- 100 - ship$landing_gear, with min_systems = all of them
  # Output text regarding landing and description of new planet
  # Kill off some colonists during construction 
  # based on how harsh the planet is and how badly damaged the construction system is.
  # Diminish lethality with good anomalies
  # Output text.
  # Kill even more colonists with bad anomalies
  # Output text.
  # If planet and/or construction system are bad enough, 
  # kill some extra colonists with construction failure
  # Output text.
  # Colonists go exploring ruins, with good and bad outcomes
  # Output text.
  # Do native relations
  # Output text
  # Any other specials
  # Calculate final number of surviving colonists
  # Calculate final tech level
  # Calculate final culture level
  # Write final description of colony
  # Prompt to rename planet
  # Calculate score and present
  # Update planetGlobalDB with a natives or ruins anomaly basing stats and flavor on player
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
  # Note that the description is returned after processing all the effects 
  # in case some of them create interpolatable obects (e.g. 'damaged').
  
  # If effect is a function, call that function and replace effect with the 
  # result of that call.
  .effect_debug<-try(effect <- if(is.function(node$effect)){
    node$effect(ship,node,...) } else node$effect);
  if(is(.effect_debug,'try-error')) browser();
  # For each item in effect, modify the corresponding item in ship accordingly
  # tags affecting the ship only for the duration of this event
  .eventtags_debug<- try(ship$eventtags <- updateTags(ship$eventtags,newtags=effect$eventtags));
  if(is(.eventtags_debug,'try-error')){
    warning('Launching debugger for updateTags in navigateEvent');
    browser();
  }
  # tags permanently affecting the ship (until negated by some future tag)
  ship$tags <- updateTags(ship$tags,newtags=effect$tags);
  # damage (or repair) ship if that effect is needed
  if(length(effect$shipstats)>0) damaged <- do.call(damageShip,c(ship,effect$shipstats));
  # update ship$eventcache
  for(ii in names(effect$eventcache)) ship$eventcache[[ii]] <- effect$eventcache[[ii]];
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
  # interpolate dynamic values
  description <- paste0(description,collapse='\n') %>% str_interp();
  #if(grepl('character.0.',description)) browser();
  # this part may vary depending on UI
  message(description);
  
  # detect whether this is a terminal node
  if(length(choices)==0) {message('Done'); return()};
  # detect whether this is a node that autoadvances if there is only one choice
  if(length(choices)==1 && isTRUE(node$autoadvance)) return(choices[[1]]);
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
      shipEventLoop(ship,eventnodes,...)};
    if(doStarChart) ids <- rglStarChart(ship);
    generalchoices <- c('Consult star database'
                        ,'Select new destination from 3D starmap'
                        ,'Ship report');
    if(!is.null(ship$lastdestination)){
      generalchoices <- c('Continue to current destination',generalchoices)};
    curplnt <- currentPlanet(ship);
    if(nrow(curplnt)==0) ship$state <- 'space';
    if(ship$state == 'planet' && nudgeShip(ship)$visited<2){
      generalchoices <- c(generalchoices,'Send probes to nearby planet')
      } else generalchoices <- c(generalchoices,'Rename nearby planet');
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
           ,`Rename nearby planet`=renamePlanet(curplnt)
           );
    #browser();
  }
}


NULL
