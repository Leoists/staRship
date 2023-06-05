library(dplyr);
library(stringr);
library(rgl);
library(rio);
library(data.tree);
library(geometry);

source('functions.R');

# import into the events data.frame
events <- import('data/events.xlsx') %>%
  mutate(
    across(is.character,~gsub('…','...',.x))
    ,across(!any_of(c('pathString','choicetext','parent','package','notes','shipstats','tags','eventtags','eventcache'))
            ,~sapply(.x,function(xx){
              if(grepl('^function',as.character(xx))) {eval(str2lang(xx))} else if(is.character(xx) && !is.na(as.numeric(xx)[1])) as.numeric(xx) else xx
            },simplify=F))
    ,across(all_of(c('shipstats','tags','eventtags','eventcache'))
            ,~sapply(sprintf('list(%s)',coalesce(.x,'')),function(xx) eval(str2lang(xx))))
  );

events$effect <- rowwise(events) %>%
  mutate(effect=list(list(shipstats=shipstats,tags=tags
                          ,eventtags=eventtags,eventcache=eventcache))) %>%
  select(effect);

eventnodes <- select(events,all_of(c('pathString','choicetext','baseprob'
                                     ,'probmods','description'
                                     ,'effect_function','effect','nchoices'
                                     ,'dynchoices','autoadvance','package'))) %>% as.Node();

eventnodes$Do(function(node){
  iilist <- list();
  for(ii in intersect(node$attributes,names(events))) {
    iilist[[ii]] <- excavateList(node[[ii]])};
  #if(node$name=='scanner') browser();
  if(is.function(iilist$effect_function)){
    iilist$effect <- iilist$effect_function} else{
      iilist$effect <- iilist$effect;
    }
  iilist$effect_function <- NULL;
  for(ii in names(iilist)) node[[ii]] <- iilist[[ii]];
  node$RemoveAttribute('effect_function',stopIfNotAvailable=F)});

# top-level nodes are chosen randomly one per event
eventnodes$nchoices <- 1; 
# we prompt to advance to the top-level event
eventnodes$autoadvance <- TRUE;
# no description to show
eventnodes$description <- '';

# generate a test-ship
foo <- newShip();

# shipLoop is basically the rest of the game
shipLoop(foo);

# seedship clone, for inspiration:
# https://github.com/kat1248/seedship/tree/master
# seedship wiki
# https://seedship.fandom.com/wiki/Special:AllPages

# TODO 
# DONE launch probes
# DONE events
# DONE Create actual event-trees and top-level event-selector
# * colonization
# * shiny integration

