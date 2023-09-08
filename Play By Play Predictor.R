#https://xgboost.readthedocs.io/en/stable/R-package/xgboostPresentation.html
options(
  digits = 4,
  scipen = 99
)
rm(list = ls())
invisible(gc())
set.seed(407)
library(xgboost)
library(Matrix)
library(data.table)
library(mltools)
library(sqldf)
library(caret)
library(dplyr)
library(Ckmeans.1d.dp)
library(pROC)

PlayByPlay2013<-nflreadr::load_pbp(2013)
PlayByPlay2014<-nflreadr::load_pbp(2014)
PlayByPlay2015<-nflreadr::load_pbp(2015)
PlayByPlay2016<-nflreadr::load_pbp(2016)
PlayByPlay2017<-nflreadr::load_pbp(2017)
PlayByPlay2018<-nflreadr::load_pbp(2018)
PlayByPlay2019<-nflreadr::load_pbp(2019)
PlayByPlay2020<-nflreadr::load_pbp(2020)
PlayByPlay2021<-nflreadr::load_pbp(2021)
PlayByPlay2022<-nflreadr::load_pbp(2022)

PlayByPlay_Data_Work<-rbind(PlayByPlay2013,PlayByPlay2014
                            ,PlayByPlay2015,PlayByPlay2016
                            ,PlayByPlay2017,PlayByPlay2018
                            ,PlayByPlay2019,PlayByPlay2020
                            ,PlayByPlay2021,PlayByPlay2022)

PlayByPlay_Data <- subset(PlayByPlay_Data_Work, down!=4)
PlayByPlay_Data <- subset(PlayByPlay_Data, season_type=="REG")
PlayByPlay_Data <- subset(PlayByPlay_Data, play_type!="no_play")
PlayByPlay_Data <- subset(PlayByPlay_Data, qb_kneel!=1)
PlayByPlay_Data <- subset(PlayByPlay_Data, qb_spike!=1)

PlayByPlay_Data_pass <- subset(PlayByPlay_Data, play_type=="pass")
PlayByPlay_Data_pass<-PlayByPlay_Data_pass[!(PlayByPlay_Data_pass$fumble==1 & PlayByPlay_Data_pass$complete_pass==0)]
PlayByPlay_Data_run <- subset(PlayByPlay_Data, play_type=="run")
PlayByPlay_Data_run <- PlayByPlay_Data_run[!(PlayByPlay_Data_run$fumble==1 & is.na(PlayByPlay_Data_run$run_location))]
PlayByPlay_Data<-rbind(PlayByPlay_Data_pass,PlayByPlay_Data_run)
rm(PlayByPlay_Data_pass,PlayByPlay_Data_run,PlayByPlay2013,PlayByPlay2014,PlayByPlay2015,PlayByPlay2016,
   PlayByPlay2017,PlayByPlay2018,PlayByPlay2019,PlayByPlay2020,PlayByPlay2021,PlayByPlay2022)
#PlayByPlay_Data$play_type = as.factor(factor(PlayByPlay_Data$play_type,
#                                             levels=c('pass', 'run'), labels =c(1,0)))
#PlayByPlay_Data$play_type = ifelse(PlayByPlay_Data$play_type == 1,0,1)
PlayByPlay_Data <- subset(PlayByPlay_Data, qb_scramble!=1)

PlayByPlay_Data$old_game_id<-as.numeric(PlayByPlay_Data$old_game_id)
PlayByPlay_Data$week<-as.factor(PlayByPlay_Data$week)
PlayByPlay_Data$game_date<-as.Date(PlayByPlay_Data$game_date)

PlayByPlay_Data$game_half<-gsub("Overtime","3",PlayByPlay_Data$game_half)
PlayByPlay_Data$game_half<-as.factor(gsub("Half","",PlayByPlay_Data$game_half))
PlayByPlay_Data$div_game<-as.factor(PlayByPlay_Data$div_game)
PlayByPlay_Data$day_of_week<-as.factor(lubridate::wday(PlayByPlay_Data$game_date))
PlayByPlay_Data$drive<-as.factor(PlayByPlay_Data$drive)
PlayByPlay_Data$qtr<-as.factor(PlayByPlay_Data$qtr)
PlayByPlay_Data$down<-as.factor(PlayByPlay_Data$down)
PlayByPlay_Data$goal_to_go<-as.factor(PlayByPlay_Data$goal_to_go)
PlayByPlay_Data$posteam_timeouts_remaining<-as.factor(PlayByPlay_Data$posteam_timeouts_remaining)
PlayByPlay_Data$defteam_timeouts_remaining<-as.factor(PlayByPlay_Data$defteam_timeouts_remaining)

PlayByPlay_Data<-PlayByPlay_Data[,c(3, 4, 5, 7, 8, 10, 12, 15, 17, 19, 21, 22, 23, 26, 27, 28, 29, 30, 37, 38, 39, 41, 42,
                                    54, 55, 58, 59, 60, 289, 293, 295, 296, 300, 320, 326, 331, 332, 333, 334, 335, 336, 
                                    337, 338, 339, 373)]

#writexl::write_xlsx(PlayByPlay_Data,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/PlayByPlay_Data.xlsx")
#rm(list = ls())
#library(readxl)
#PlayByPlay_Data<-readxl::read_xlsx("F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/PlayByPlay_Data.xlsx")

PlayByPlay_Datah<-head(PlayByPlay_Data)
#writexl::write_xlsx(PlayByPlay_Datah,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/PlayByPlay_Datah.xlsx")

# team_str<- c('ARI' = '20001', 'ATL' = '20002', 'BAL' = '20003', 'BUF' = '20004', 'CAR' = '20005', 'CHI' = '20006',
#             'CIN' = '20007', 'CLE' = '20008', 'DAL' = '20009', 'DEN' = '20010', 'DET' = '20011', 'GB' = '20012',
#             'HOU' = '20013', 'IND' = '20014', 'JAX' = '20015', 'KC' = '20016', 'LAC' = '20017', 'LA' = '20018',
#             'LV' = '20019', 'MIA' = '20020', 'MIN' = '20021', 'NE' = '20022', 'NO' = '20023', 'NYG' = '20024',
#             'NYJ' = '20025', 'PHI' = '20026', 'PIT' = '20027', 'SEA' = '20028', 'SF' = '20029', 'TB' = '20030',
#             'TEN' = '20031', 'WAS' = '20032')
# PlayByPlay_Data$home_team <-factor(stringr::str_replace_all(PlayByPlay_Data$home_team, team_str))
# PlayByPlay_Data$away_team <-factor(stringr::str_replace_all(PlayByPlay_Data$away_team, team_str))
# PlayByPlay_Data$posteam <-factor(stringr::str_replace_all(PlayByPlay_Data$posteam, team_str))
# PlayByPlay_Data$defteam <-factor(stringr::str_replace_all(PlayByPlay_Data$defteam, team_str))
PlayByPlay_Data$home_team <-as.factor(PlayByPlay_Data$home_team)
PlayByPlay_Data$away_team <-as.factor(PlayByPlay_Data$away_team)
PlayByPlay_Data$posteam <-as.factor(PlayByPlay_Data$posteam)
PlayByPlay_Data$defteam <-as.factor(PlayByPlay_Data$defteam)
PlayByPlay_Data$week<-as.factor(PlayByPlay_Data$week)
#PlayByPlay_Data$pass_length <- as.numeric(PlayByPlay_Data$pass_length)
#PlayByPlay_Data$drive_start_yard_line <- as.numeric(PlayByPlay_Data$drive_start_yard_line)
#PlayByPlay_Data$drive_start_yard_line <- stringr::str_replace_all(PlayByPlay_Data$drive_start_yard_line, " ","")
PlayByPlay_Data<- PlayByPlay_Data[!grepl("kneels", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("sacked", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("13-D.Thomas to CIN 35 for -8 yards", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("13-D.Thomas to KC 21 for -7 yards.", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("21-J.McKinnon to CAR 14 for -4 yards ", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("9-M.Stafford up the middle, dead ball declared at ATL 25 for -1 yards.", 
                                         PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("31-M.Jones to CLE 40 for -1 yards", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("29-R.Helu to NYG 42 for 2 yards", PlayByPlay_Data$desc),]
PlayByPlay_Data<- PlayByPlay_Data[!grepl("28-L.Murray to OAK 11 for 2 yards", PlayByPlay_Data$desc),]

time_str<-c('09:30:00' = 'Early', '9:30:00' = 'Early', '12:30:00' = 'Early', '13:00:00' = 'Early', '15:40:00' = 'Late', 
            '16:05:00' = 'Late', '16:25:00' = 'Late', '16:30:00' = 'Late', '17:00:00' = 'Late', '19:00:00' = 'Night', 
            '19:05:00' = 'Night', '19:10:00' = 'Night', '19:15:00' = 'Night', '20:05:00' = 'Night', '20:15:00' = 'Night', 
            '20:20:00' = 'Night', '20:25:00' = 'Night', '20:30:00' = 'Night', '20:40:00' = 'Night', '20:50:00' = 'Night', 
            '22:20:00' = 'Night', '23:35:00' = 'Night')
PlayByPlay_Data$start_time <-factor(stringr::str_replace_all(PlayByPlay_Data$start_time, time_str))

# coach_str<- c('Jay Rosburg' = '1001', 'Wade Phillips' = '1001', 'Steve Spagnuolo' = '1001', 'Gregg Williams' = '1001', 
#               'Jeff Saturday' = '1001', 'Darrell Bevell' = '1001', 'Bill Callahan' = '1001', 'Raheem Morris' = '1001', 
#               'Romeo Crennel' = '1001', 'Rich Bisaccia' = '1001', 'Tony Sparano' = '1001', 'Urban Meyer' = '1001', 
#               'Nathaniel Hackett' = '1002', 'Matt Eberflus' = '1002', 'Freddie Kitchens' = '1002', 'Greg Schiano' = '1002', 
#               'Jim Tomsula' = '1002', 'Mike Munchak' = '1002', 'Mike McDaniel' = '1002', 'Mike Shanahan' = '1002', 
#               'David Culley' = '1002', 'Brian Daboll' = '1002', 'Jim Schwartz' = '1002', 'Leslie Frazier' = '1002', 
#               'Josh McDaniels' = '1002', 'Rob Chudzinski' = '1002', 'Kevin O\'Connell' = '1002', 'Steve Wilks' = '1003', 
#               'Ben McAdoo' = '1004', 'Jim Harbaugh' = '1005', 'Ken Whisenhunt' = '1006', 'Marc Trestman' = '1007', 
#               'Vance Joseph' = '1008', 'Mike Mularkey' = '1009', 'Joe Judge' = '1010', 'Mike Smith' = '1011', 
#               'Mike Pettine' = '1012', 'Nick Sirianni' = '1013', 'Arthur Smith' = '1014', 'Pat Shurmur' = '1015', 
#               'Dan Campbell' = '1016', 'Robert Saleh' = '1017', 'Brandon Staley' = '1018', 'Dennis Allen' = '1019', 
#               'Matt Rhule' = '1020', 'Hue Jackson' = '1021', 'Matt Patricia' = '1022', 'Gary Kubiak' = '1023', 
#               'Vic Fangio' = '1024', 'Dirk Koetter' = '1025', 'Jack Del Rio' = '1026', 'Lovie Smith' = '1027', 
#               'Brian Flores' = '1028', 'Kevin Stefanski' = '1029', 'Tom Coughlin' = '1030', 'Jon Gruden' = '1031', 
#               'Joe Philbin' = '1032', 'Rex Ryan' = '1033', 'Jeff Fisher' = '1034', 'Jim Caldwell' = '1035', 
#               'Matt LaFleur' = '1036', 'Gus Bradley' = '1037', 'Matt Nagy' = '1038', 'Anthony Lynn' = '1039', 
#               'Mike McCoy' = '1040', 'Zac Taylor' = '1041', 'Kliff Kingsbury' = '1042', 'Chip Kelly' = '1043', 
#               'Frank Reich' = '1044', 'Adam Gase' = '1045', 'Mike Vrabel' = '1046', 'Chuck Pagano' = '1047', 
#               'John Fox' = '1048', 'Todd Bowles' = '1049', 'Jay Gruden' = '1050', 'Dan Quinn' = '1051', 
#               'Sean McDermott' = '1052', 'Kyle Shanahan' = '1053', 'Doug Pederson' = '1054', 'Sean McVay' = '1055', 
#               'Marvin Lewis' = '1056', 'Doug Marrone' = '1057', 'Bill O\'Brien' = '1058', 'Jason Garrett' = '1059', 
#               'Mike Zimmer' = '1060', 'Bruce Arians' = '1061', 'Mike McCarthy' = '1062', 'Sean Payton' = '1063', 
#               'Pete Carroll' = '1064', 'Ron Rivera' = '1065', 'Andy Reid' = '1066', 'Mike Tomlin' = '1067', 
#               'John Harbaugh' = '1068', 'Bill Belichick' = '1069')
# PlayByPlay_Data$home_coach <-factor(stringr::str_replace_all(PlayByPlay_Data$home_coach, coach_str))
# PlayByPlay_Data$away_coach <-factor(stringr::str_replace_all(PlayByPlay_Data$away_coach, coach_str))
PlayByPlay_Data$home_coach <-as.factor(PlayByPlay_Data$home_coach)
PlayByPlay_Data$away_coach <-as.factor(PlayByPlay_Data$away_coach)


# stad_str<- c('ATL00' = '3001', 'ATL97' = '3001', 'BAL00' = '3002', 'BOS00' = '3003', 'BUF00' = '3004', 'BUF01' = '3004', 
#               'CAR00' = '3005', 'CHI98' = '3006', 'CIN00' = '3007', 'CLE00' = '3008', 'DAL00' = '3009', 'DEN00' = '3010', 
#               'DET00' = '3011', 'GER00' = '3012', 'GNB00' = '3013', 'HOU00' = '3014', 'IND00' = '3015', 'JAX00' = '3016', 
#               'KAN00' = '3017', 'LAX01' = '3018', 'LAX97' = '3018', 'LAX99' = '3018', 'LON00' = '3019', 'LON01' = '3019', 
#               'LON02' = '3019', 'MEX00' = '3020', 'MIA00' = '3021', 'MIN00' = '3022', 'MIN01' = '3022', 'MIN98' = '3022', 
#               'NAS00' = '3023', 'NOR00' = '3024', 'NYC01' = '3025', 'OAK00' = '3026', 'PHI00' = '3027', 'PHO00' = '3028', 
#               'PIT00' = '3029', 'SDG00' = '3030', 'SEA00' = '3031', 'SFO00' = '3032', 'SFO01' = '3032', 'STL00' = '3033', 
#               'TAM00' = '3034', 'VEG00' = '3035', 'WAS00' = '3036')
PlayByPlay_Data$stadium_id <-as.factor(PlayByPlay_Data$stadium_id)

clist<-sapply(PlayByPlay_Data_Work, class)
#write.table(clist,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/list.txt")

#rm(clist)
invisible(gc())

PlayByPlay_Data_pass <- subset(PlayByPlay_Data, play_type=="pass")
PlayByPlay_Data_run <- subset(PlayByPlay_Data, play_type=="run")
PlayByPlay_Data_pass <- subset(PlayByPlay_Data_pass,!is.na(PlayByPlay_Data_pass$pass_length))
PlayByPlay_Data_run$run_gap[is.na(PlayByPlay_Data_run$run_gap)] <- "center"
PlayByPlay_Data_pass$play_full <-stringr::str_c(PlayByPlay_Data_pass$play_type,'-',PlayByPlay_Data_pass$pass_length,'-',
                                                PlayByPlay_Data_pass$pass_location)
PlayByPlay_Data_run$play_full <-stringr::str_c(PlayByPlay_Data_run$play_type,'-',PlayByPlay_Data_run$run_location,'-',
                                                PlayByPlay_Data_run$run_gap)
PlayByPlay_Data$goal_to_go <- as.factor(PlayByPlay_Data$goal_to_go)

#writexl::write_xlsx(PlayByPlay_Data_pass,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/PlayByPlay_Data_pass.xlsx")
#writexl::write_xlsx(PlayByPlay_Data_run,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/PlayByPlay_Data_run.xlsx")

PlayByPlay_Data<-rbind(PlayByPlay_Data_pass,PlayByPlay_Data_run)
df<-PlayByPlay_Data[,-c('desc','pass_length','pass_location','run_location', 'run_gap','game_stadium','play_type',
                            'play_type_nfl','ydsnet','air_yards', 'series', 'location', 'yards_gained',
                            'drive_start_yard_line')]
df<-within(df,surface[stadium =='Tottenham Hotspur Stadium']<-'turf_nation')
df<-within(df,surface[stadium =='Levi\'s Stadium']<-'grass')
df<-within(df,surface[stadium =='Wembley Stadium']<-'grass')
df<-within(df,surface[stadium =='Allianz Arena']<-'grass')
df<-within(df,surface[stadium =='Azteca Stadium']<-'grass')
#df<-within(df,roof[(stadium =='Lucas Oil Stadium' & is.na(roof))]<-'open')
df<-within(df,roof[(stadium =='Mercedes-Benz Stadium' & is.na(roof) & home_team=='20002' & away_team=='20022' 
                    & week=='11')]<-'closed')
df<-within(df,roof[(stadium =='Mercedes-Benz Stadium' & is.na(roof) & home_team=='20002' & away_team=='20030' 
                      & week=='13')]<-'closed')
df<-within(df,roof[(stadium =='Mercedes-Benz Stadium' & is.na(roof))]<-'open')
df<-data.table(df)
df$roof<-as.factor(df$roof)
df$surface<-as.factor(df$surface)


clist<-sapply(df, class)
#write.table(clist,"F:/School/UCF/Spring 2023/Disertation/Data/Play by Play/list.txt")
rm(clist, time_str,PlayByPlay_Data_pass,PlayByPlay_Data_run)
invisible(gc())

df<-within(df,temp[roof =='dome' & is.na(temp)]<-72)
df<-within(df,temp[roof =='closed' & is.na(temp)]<-72)
#df<-within(df,temp[roof =='open' & is.na(temp)]<-72)
df<-within(df,wind[roof =='dome' & is.na(wind)]<-0)
df<-within(df,wind[roof =='closed' & is.na(wind)]<-0)
#df<-within(df,wind[roof =='open' & is.na(wind)]<-0)

tempadd<-sqldf('SELECT * FROM df WHERE temp is null;')
testing<-tempadd
tempfine<-sqldf('SELECT * FROM df WHERE temp is not null;')

tempadd<-suppressWarnings(sqldf(c(
  "UPDATE tempadd SET temp = 11 WHERE weather LIKE '%Temp: 11%';","SELECT * 
  FROM main.tempadd;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 24 WHERE weather LIKE '%Temp: 24%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 25 WHERE weather LIKE '%Temp: 25%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 26 WHERE weather LIKE '%Temp: 26%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 27 WHERE weather LIKE '%Temp: 27%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 28 WHERE weather LIKE '%Temp: 28%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 29 WHERE weather LIKE '%Temp: 29%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 33 WHERE weather LIKE '%Temp: 33%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 34 WHERE weather LIKE '%Temp: 34%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 35 WHERE weather LIKE '%Temp: 35%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 36 WHERE weather LIKE '%Temp: 36%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 37 WHERE weather LIKE '%Temp: 37%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 38 WHERE weather LIKE '%Temp: 38%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 39 WHERE weather LIKE '%Temp: 39%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 40 WHERE weather LIKE '%Temp: 40%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 41 WHERE weather LIKE '%Temp: 41%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 43 WHERE weather LIKE '%Temp: 43%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 44 WHERE weather LIKE '%Temp: 44%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 45 WHERE weather LIKE '%Temp: 45%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 46 WHERE weather LIKE '%Temp: 46%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 47 WHERE weather LIKE '%Temp: 47%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 48 WHERE weather LIKE '%Temp: 48%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 49 WHERE weather LIKE '%Temp: 49%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 50 WHERE weather LIKE '%Temp: 50%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 51 WHERE weather LIKE '%Temp: 51%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 52 WHERE weather LIKE '%Temp: 52%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 53 WHERE weather LIKE '%Temp: 53%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 54 WHERE weather LIKE '%Temp: 54%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 55 WHERE weather LIKE '%Temp: 55%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 56 WHERE weather LIKE '%Temp: 56%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 57 WHERE weather LIKE '%Temp: 57%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 58 WHERE weather LIKE '%Temp: 58%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 59 WHERE weather LIKE '%Temp: 59%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 60 WHERE weather LIKE '%Temp: 60%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 61 WHERE weather LIKE '%Temp: 61%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 62 WHERE weather LIKE '%Temp: 62%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 63 WHERE weather LIKE '%Temp: 63%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 64 WHERE weather LIKE '%Temp: 64%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 65 WHERE weather LIKE '%Temp: 65%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 66 WHERE weather LIKE '%Temp: 66%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 67 WHERE weather LIKE '%Temp: 67%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 68 WHERE weather LIKE '%Temp: 68%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 69 WHERE weather LIKE '%Temp: 69%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 70 WHERE weather LIKE '%Temp: 70%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 71 WHERE weather LIKE '%Temp: 71%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 72 WHERE weather LIKE '%Temp: 72%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 73 WHERE weather LIKE '%Temp: 73%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 74 WHERE weather LIKE '%Temp: 74%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 75 WHERE weather LIKE '%Temp: 75%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 76 WHERE weather LIKE '%Temp: 76%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 77 WHERE weather LIKE '%Temp: 77%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 78 WHERE weather LIKE '%Temp: 78%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 79 WHERE weather LIKE '%Temp: 79%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 80 WHERE weather LIKE '%Temp: 80%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 81 WHERE weather LIKE '%Temp: 81%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 82 WHERE weather LIKE '%Temp: 82%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 83 WHERE weather LIKE '%Temp: 83%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 84 WHERE weather LIKE '%Temp: 84%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 85 WHERE weather LIKE '%Temp: 85%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 86 WHERE weather LIKE '%Temp: 86%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 87 WHERE weather LIKE '%Temp: 87%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 88 WHERE weather LIKE '%Temp: 88%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 89 WHERE weather LIKE '%Temp: 89%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 92 WHERE weather LIKE '%Temp: 92%';","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 74 
                                  WHERE old_game_id = 2013091506;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 54 
                                  WHERE old_game_id = 2013102012;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 80 
                                  WHERE old_game_id = 2014092803;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 54 
                                  WHERE old_game_id = 2014100503;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 56 
                                  WHERE old_game_id = 2014101905;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 80 
                                  WHERE old_game_id = 2018101400;","SELECT * FROM main.testing;")))
testing<-tempadd

tempadd<-suppressWarnings(sqldf(c("UPDATE testing SET temp = 84 
                                  WHERE old_game_id = 2019091513;","SELECT * FROM main.testing;")))
testing<-tempadd

df<-rbind(tempadd,tempfine)
rm(tempadd,tempfine,testing)
invisible(gc())

snow<-sqldf('SELECT * FROM df WHERE weather LIKE "%snow%";')
nosnow<-sqldf('SELECT * FROM df WHERE weather NOT LIKE "%snow%";')
snow$snow<-1
nosnow$snow<-0
blankweather<-sqldf('SELECT * FROM df WHERE weather is null;')
blankweather$snow<-0
df<-rbind(snow,nosnow,blankweather)

rain<-sqldf('SELECT * FROM df WHERE weather LIKE "%rain%";')
chrain<-sqldf('SELECT * FROM rain WHERE weather LIKE "%chance%";')
rain1<-sqldf('SELECT * FROM rain WHERE weather NOT LIKE "%chance%";')
norain<-sqldf('SELECT * FROM df WHERE weather NOT LIKE "%rain%";')
rain1$rain<-1
chrain$rain<-0
norain$rain<-0
blankweather$rain<-0
df<-rbind(rain1,norain,blankweather,chrain)

rm(rain,rain1,norain,blankweather,snow,nosnow,chrain)
invisible(gc())
df$rain<-as.factor(df$rain)
df$snow<-as.factor(df$snow)

df$roof<-as.factor(df$roof)
df<-df[,-c(1,21,22)]
df$yardline_100 <- scale(df$yardline_100)
df$half_seconds_remaining<- scale(df$half_seconds_remaining)
df$ydstogo<- scale(df$ydstogo)
df$score_differential<- scale(df$score_differential)
df$posteam_score<- scale(df$posteam_score)
df$defteam_score<- scale(df$defteam_score)
df$temp<- scale(df$temp)
df$wind<- scale(df$wind)

summary(df)
sd(df$yardline_100)
sd(df$half_seconds_remaining)
sd(df$ydstogo)
sd(df$score_differential)
sd(df$posteam_score)
sd(df$defteam_score)
sd(df$temp)
sd(df$wind)

#df %>% mutate(across(where(is.numeric), scale))
df<-data.table(df)
sparse_matrix <- one_hot(df)
PlayByPlay_Data<-sparse_matrix

rm(df,sparse_matrix,PlayByPlay_Data_Work)
invisible(gc())
PlayByPlay_Data<-PlayByPlay_Data %>% relocate(play_full)

#Split<-sample(1:nrow(PlayByPlay_Data), nrow(PlayByPlay_Data)*0.7)
Split <-sample(c(TRUE, FALSE), nrow(PlayByPlay_Data), replace=TRUE, prob=c(0.7,0.3))

Variable_PlayByPlay<-as.matrix(PlayByPlay_Data[,-1])
Label_PlayByPlay<-PlayByPlay_Data[,play_full]
test_str<- c('pass-deep-left'='0', 
             'pass-deep-middle'='1',
             'pass-deep-right'='2', 
             'pass-short-left'='3', 
             'pass-short-middle'='4', 
             'pass-short-right'='5', 
             'run-left-end'='6',
             'run-left-tackle'='6',
             'run-right-guard'='7', 
             'run-middle-center'='7', 
             'run-left-center'='7', 
             'run-left-guard'='7',
             'run-right-end'='8', 
             'run-right-tackle'='8')
Label_PlayByPlay <-stringr::str_replace_all(Label_PlayByPlay, test_str)
Label_PlayByPlay<-as.numeric(Label_PlayByPlay)
XGB_Matrix_PlayByPlay<-xgb.DMatrix(data=Variable_PlayByPlay, label=Label_PlayByPlay)

# split train data and make xgb.DMatrix
Train_data   <- Variable_PlayByPlay[Split,]
Train_label  <- Label_PlayByPlay[Split]
Train_matrix <- xgb.DMatrix(data = Train_data, label = Train_label)
# split test data and make xgb.DMatrix
Test_data   <- Variable_PlayByPlay[!Split,]
Test_label  <- Label_PlayByPlay[!Split]
Test_matrix <- xgb.DMatrix(data = Test_data, label = Test_label)

numberOfClasses <- length(unique(Label_PlayByPlay))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 100 # number of XGBoost rounds
cv.nfold  <- 10

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = Train_data,
                   label = Train_label,
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = Train_label+1)

head(OOF_prediction)

confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

it = which.max(cv_model$evaluation_log$test_auc_mean)
best_iter = cv_model$evaluation_log$iter[it]

#plot(multiclass.roc(Train_label, OOF_prediction$max_prob))



bst_model <- xgb.train(params = xgb_params,
                       data = Train_matrix,
                       nrounds = 100)


# Predict hold-out test set
test_pred <- predict(bst_model, newdata = Test_matrix)

test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
                          t() %>%
                          data.frame() %>%
                          mutate(label = Test_label + 1,
                                 max_prob = max.col(., "last"))

test<-confusionMatrix(factor(test_prediction$max_prob),
                      factor(test_prediction$label),
                      mode = "everything")

test
names <-  colnames(PlayByPlay_Data[,-1])

importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)
gp = xgb.ggplot.importance(importance_matrix, top_n = 40)
print(gp)
gp = xgb.ggplot.importance(importance_matrix, top_n = 20)
print(gp)
gp = xgb.ggplot.importance(importance_matrix, top_n = 10)
print(gp)


                          
  
  
  
  gp = xgb.ggplot.importance(importance_matrix, top_n = 10)
                          print(gp)