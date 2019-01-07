#' Feature Extraction

maxstreak <- function(bool) {
  r <- rle(bool)
  streak <- 0
  if(any(bool)) {
    streak <- max(r$lengths[r$values])
  }
  return(streak)
}

team_elos <- function(matches) {
  # gecmisten gelecege siralamak lazim
  teams = copy(matches)[order(Match_Date)][, c("Home", "Away", "Home_Score", "Away_Score", "Match_Date")]
  teams[,Match_Year := as.numeric(format(strptime(Match_Date, "%Y-%m-%d"), '%Y'))]
  # yeari bilince her sezonun sonunda skorlari 1500e regress ediyor
  elomatrix = as.matrix(elo.run(score(teams$Home_Score, teams$Away_Score) ~ adjust(teams$Home, 57.9) + teams$Away + regress(teams$Match_Year, 1500, 0.2), data = teams, k = 20))
  # latest elolar
  elovector = data.frame(Team = colnames(elomatrix), latestelo = elomatrix[nrow(elomatrix),])
  return(elovector)
}
team_features <- function(matches, recnum = 30, byrecentyear = 2017) {
  
  teams = copy(matches)[order(-Match_Date)][, c("Home", "Away", "Home_Score", "Away_Score", "Match_Date")]
  
  teamshome = copy(teams)
  teamsaway = copy(teams)
  
  # as many games as exist below recnum
  teamsbyrecentgameshome = na.omit(copy(teams)[, .SD[1:recnum], by=Home])
  teamsbyrecentgamesaway = na.omit(copy(teams)[, .SD[1:recnum], by=Away])
  
  teamsbyrecentyearhome = copy(teams)[year(Match_Date) >= byrecentyear]
  teamsbyrecentyearaway = copy(teams)[year(Match_Date) >= byrecentyear]
  
  teamshome[, c("winratehome", "loseratehome", "drawratehome") := list(mean(Home_Score>Away_Score), mean(Home_Score<Away_Score), mean(Home_Score == Away_Score)), by=Home]
  teamshome[, c("avgdiffhome", "avgscoredhome", "avgconchome") := list(mean(Home_Score-Away_Score), mean(Home_Score), mean(Away_Score)), by=Home]
  teamshome[, c("curstreakhome", "maxstreakhome") := list(which.min((Home_Score-Away_Score)>0)-1, as.numeric(maxstreak((Home_Score-Away_Score)>0))), by=Home]
  teamshome = unique(teamshome[, c("Away", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamshome, "Home", "Team")
  
  #maxstreak çıkabilir mi?
  #iki maç arası vakit
  
  
  teamsaway[, c("winrateaway", "loserateaway", "drawrateaway") := list(mean(Home_Score<Away_Score), mean(Home_Score>Away_Score), mean(Home_Score == Away_Score)), by=Away]
  teamsaway[, c("avgdiffaway", "avgscoredaway", "avgconcaway") := list(mean(Away_Score-Home_Score), mean(Away_Score), mean(Home_Score)), by=Away]
  teamsaway[, c("curstreakaway", "maxstreakaway") := list(which.min((Home_Score-Away_Score)<0)-1, as.numeric(maxstreak((Home_Score-Away_Score)<0))), by=Away]
  teamsaway = unique(teamsaway[, c("Home", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamsaway, "Away", "Team")
  
  teamsbyrecentgameshome[, c("recwinratehome", "recloseratehome", "recdrawratehome") := list(mean(Home_Score>Away_Score), mean(Home_Score<Away_Score), mean(Home_Score == Away_Score)), by=Home]
  teamsbyrecentgameshome[, c("recavgdiffhome", "recavgscoredhome", "recavgconchome") := list(mean(Home_Score-Away_Score), mean(Home_Score), mean(Away_Score)), by=Home]
  teamsbyrecentgameshome = unique(teamsbyrecentgameshome[, c("Away", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamsbyrecentgameshome, "Home", "Team")
  
  teamsbyrecentgamesaway[, c("recwinrateaway", "recloserateaway", "recdrawrateaway") := list(mean(Home_Score<Away_Score), mean(Home_Score>Away_Score), mean(Home_Score == Away_Score)), by=Away]
  teamsbyrecentgamesaway[, c("recavgdiffaway", "recavgscoredaway", "recavgconcaway") := list(mean(Away_Score-Home_Score), mean(Away_Score), mean(Home_Score)), by=Away]
  teamsbyrecentgamesaway = unique(teamsbyrecentgamesaway[, c("Home", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamsbyrecentgamesaway, "Away", "Team")
  
  teamsbyrecentyearhome[, c("recyearwinratehome", "recyearloseratehome", "recyeardrawratehome") := list(mean(Home_Score>Away_Score), mean(Home_Score<Away_Score), mean(Home_Score == Away_Score)), by=Home]
  teamsbyrecentyearhome[, c("recyearavgdiffhome", "recyearavgscoredhome", "recyearavgconchome") := list(mean(Home_Score-Away_Score), mean(Home_Score), mean(Away_Score)), by=Home]
  teamsbyrecentyearhome = unique(teamsbyrecentyearhome[, c("Away", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamsbyrecentyearhome, "Home", "Team")
  
  teamsbyrecentyearaway[, c("recyearwinrateaway", "recyearloserateaway", "recyeardrawrateaway") := list(mean(Home_Score<Away_Score), mean(Home_Score>Away_Score), mean(Home_Score == Away_Score)), by=Away]
  teamsbyrecentyearaway[, c("recyearavgdiffaway", "recyearavgscoredaway", "recyearavgconcaway") := list(mean(Away_Score-Home_Score), mean(Away_Score), mean(Home_Score)), by=Away]
  teamsbyrecentyearaway = unique(teamsbyrecentyearaway[, c("Home", "Home_Score", "Away_Score", "Match_Date") := NULL])
  setnames(teamsbyrecentyearaway, "Away", "Team")
  
  teams_home_total = Reduce(function(x, y) merge(x, y, by="Team", all=TRUE), list(teamshome, teamsbyrecentgameshome, teamsbyrecentyearhome))
  teams_home_total[!complete.cases(teams_home_total), c("recyearwinratehome", "recyearloseratehome", "recyeardrawratehome", "recyearavgdiffhome", "recyearavgscoredhome", "recyearavgconchome") := list(winratehome, loseratehome, drawratehome, avgdiffhome, avgscoredhome, avgconchome)]
  
  teams_away_total = Reduce(function(x, y) merge(x, y, by="Team", all=TRUE), list(teamsaway, teamsbyrecentgamesaway, teamsbyrecentyearaway))
  teams_away_total[!complete.cases(teams_away_total), c("recyearwinrateaway", "recyearloserateaway", "recyeardrawrateaway", "recyearavgdiffaway", "recyearavgscoredaway", "recyearavgconcaway") := list(winrateaway, loserateaway, drawrateaway, avgdiffaway, avgscoredaway, avgconcaway)]
  
  return(list(home = teams_home_total, away = teams_away_total))
}

extract_features.openclose <- function(matches,odd_details,pMissThreshold=0.01,trainStart,testStart){

  details = copy(odd_details)
  matches = copy(matches)
  
  details=details[order(OddChangeDateTime)]
  feature_odd_details=details[,list(Odd_Open=odd[1],Odd_Close=odd[.N]),list(matchId,betType,oddtype,bookmaker)]

  feature_odd_details = merge(matches[,list(matchId,Match_Date)], feature_odd_details,by="matchId")

  #HANDLE MISSINGS
  details_temp = dcast(feature_odd_details, matchId+betType ~ paste0("Odd_Close_",bookmaker)+oddtype, value.var = c("Odd_Close"))
  details_melt = melt(details_temp, id.vars = c("matchId","betType"), measure.vars = names(details_temp)[names(details_temp) %like% "Odd_Close"], value.name = "odd")
  details_melt[,c("OpenClose","bookmaker","oddtype"):=tstrsplit(variable,split="_",keep=c(2:4))]
  details_melt[,variable:=NULL]
  details_melt = merge(matches[,list(matchId,Match_Date)], details_melt,by="matchId",all=T)
  
  bookieMissingness = details_melt[Match_Date >= trainStart,list(.N,percMiss=sum(is.na(odd))/.N),by=list(bookmaker,betType)]
  bookiesToKeep = unique(bookieMissingness[percMiss <= pMissThreshold]$bookmaker)
  cat("Number of bookmakers with proportion of missings below",pMissThreshold,"since",as.character(trainStart),":",length(bookiesToKeep),"\n")

  nonmissingBookmakers_sinceTestStart = unique(details_melt[Match_Date >= testStart, list(.N,NA_SUM=sum(is.na(odd))),by=list(bookmaker,betType)][NA_SUM==0]$bookmaker)
  bookiesToKeep = intersect(bookiesToKeep,nonmissingBookmakers_sinceTestStart)
  cat("Number of bookmakers with no missings since testStart", as.character(testStart), ":", length(bookiesToKeep), "\n")

  details = dcast(feature_odd_details,matchId~oddtype+bookmaker,value.var = c("Odd_Open","Odd_Close"))
  columnsToKeep = grep(paste(bookiesToKeep,collapse="|"),names(details),value=T)
  details = details[,c('matchId',columnsToKeep),with=F]
  #HANDLE MISSINGS END

  details = merge(matches[,-c('Home_Score', 'Away_Score', 'Total_Score','Result_Home','Result_Tie','Result_Away'),with=F],
                  details,by="matchId")


  return(features = details)
}
