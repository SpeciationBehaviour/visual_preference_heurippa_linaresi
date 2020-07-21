#Written by Alexander Hausmann (alexander_hausmann@gmx.net) 
#in ~ May 2018 for a Windows machine

#This file is executed automatically by the batch script CUT_VIDEO_BY_CONDITION.bat.

#This R script executes the following tasks:
#a) Create a video-information table which informs about the time each video was created.
#b) Create essential output used by C++ to create final supercut.


#Take arguments from command line giving the path information
params<-commandArgs(trailingOnly = TRUE)

#Split the parameter information into three variables.
#From now on, the directory will always be called following these variables.
main_dir<-as.character(params[1])
date<-as.character(params[2])
species<-as.character(params[3])

#Get list of files from extract folder
all_files<-list.files(paste0(main_dir,"/",date,"/",species))
#Get position of original mp4 names in list
first_char<-sapply(1:length(all_files),function(x) substr(all_files[x],1,1))
file_endings<-sapply(1:length(all_files),function(x) substr(all_files[x],nchar(all_files[x])-3,nchar(all_files[x])))
number_mp4s<-(1:length(all_files))[first_char=="a"&(file_endings==".mp4"|file_endings==".MP4")]
mp4_names<-all_files[number_mp4s]
mp4_names<-sapply(1:length(mp4_names),function(x) strsplit(mp4_names[x],"_")[[1]][2])
#Find if we trust GoPro system time.
trustGP<-read.table(paste0(main_dir,"/cut_video_by_condition/directories/trust_gopro.txt"),header=F)[1,1]

#Order videos according to their title.
first4<-sapply(1:length(mp4_names),function(x) substr(mp4_names[x],1,4))
next4<-sapply(1:length(mp4_names),function(x) substr(mp4_names[x],5,8))
mp4_names<-mp4_names[order(next4,first4)]


# #SAFETY STEP:
# #Let' make sure that the initiator table, as well as the vector of mp4 file names is sorted
# #asecendingly by recording time.
# recording<-NULL
# for(i in 1:length(number_mp4s)){
#   #Read in information we have on each video as provided by GoPro system time.
#   starter<-read.table(paste0(main_dir,"/",date,"/",species,"/forR_",mp4_names[i],"_info_new_info"),header=F)
#   #The following lines split the strings according to separators such that
#   #we can retrieve the relevant information.
#   str1<-strsplit(as.character(starter[1,3]),"-")
#   str2<-strsplit(str1[[1]],"T")
#   str3<-strsplit(str2[[3]][2],":")
#   str4<-substr(str3[[1]][3],1,nchar(str3[[1]][3])-1)
#   #This uses the split string to fill in information about recording time.
#   #All gets pastet together in one "number
#   
#   recording<-c(recording,as.numeric(paste0(str1[[1]][1],str1[[1]][2],str2[[3]][1],str3[[1]][1],str3[[1]][2],str4)))
# } 
# 
# #Order the name list.
# #But only if all videos were recorded on the same day (if not, it may indicate problems of the system time of the camera).
# if(length(unique(substr(recording,1,8)))==1){
#   mp4_names<-mp4_names[order(recording)]
# }


#Get names of initiator videos
initiator<-mp4_names[grepl("GOPR",mp4_names)]


#Get IDs (i.e. to which video unit each initiator/any video belongs to).
initiator_ID<-sapply(1:length(initiator),function(x) substr(initiator[x],nchar(initiator[x])-7,nchar(initiator[x])-4))
all_ID<-sapply(1:length(mp4_names),function(x) substr(mp4_names[x],nchar(mp4_names[x])-7,nchar(mp4_names[x])-4))

#Find which of the mp4s is associated with which initiator video
VIDEO_ID<-sapply(1:length(mp4_names),function(x) (1:length(initiator))[initiator_ID%in%all_ID[x]])

if("video_info.txt"%in%all_files){
  rec_time<-read.table(paste0(main_dir,"/",date,"/",species,"/video_info.txt"),header=T,row.names=NULL)
  for(i in 1:length(rec_time[1,])){
    if(is.factor(rec_time[,i])){
      rec_time[,i]<-levels(rec_time[,i])[rec_time[,i]]
    }
  }
} else{
  #Create empty table. Column 1 will contain the file name of the first chunk, column 2-4 the 
  #date when the video was recorded, column 5-7 the time of day when it was created
  #column 8 the time of day converted into seconds.
  #Column 9 gives each video an ID, whereas 1 is the first one recorded during the day
  #and the last one is the length of the table.
  #REMARK: We trust here that the sorting in starter.txt is according to recording time, which it should be.
  #If we though reach video number 10000, this is not the case anymore!
  #Column 10 calculates the duration of each video.
  rec_time<-data.frame(video=rep(NA,length(number_mp4s)),
                       year=rep(NA,length(number_mp4s)),
                       month=rep(NA,length(number_mp4s)),
                       day=rep(NA,length(number_mp4s)),
                       hour=rep(NA,length(number_mp4s)),
                       min=rep(NA,length(number_mp4s)),
                       sec=rep(NA,length(number_mp4s)),
                       converted=rep(NA,length(number_mp4s)),
                       ID=VIDEO_ID,
                       duration=rep(NA,length(number_mp4s)),
                       fps=rep(NA,length(number_mp4s)))
  
  if(trustGP!=1){
    #Read in file where user set beginnings of videos.
    beginnings<-read.table(paste0(main_dir,"/",date,"/",species,"/beginnings.txt"),header=F,row.names=NULL)
    for(i in 1:length(beginnings[1,])){
      if(is.factor(beginnings[,i])){
        beginnings[,i]<-levels(beginnings[,i])[beginnings[,i]]
      }
    }
  } else{
    #If we trust GoPro system time, we only read in the information we have on the first chunk of a video, since creation
    #time of a video is always a second integer and we loose several milliseconds of precision each time we do this.
    #Here, we extract information of only the first chunks.
    beginnings<-NULL
    for(i in initiator){
      starter<-read.table(paste0(main_dir,"/",date,"/",species,"/forR_",i,"_info_new_info"),header=F)
      #The following lines split the strings according to separators such that
      #we can retrieve the relevant information.
      str1<-strsplit(as.character(starter[1,3]),"-")
      str2<-strsplit(str1[[1]],"T")
      str3<-strsplit(str2[[3]][2],":")
      str4<-substr(str3[[1]][3],1,nchar(str3[[1]][3])-1)
      beginnings<-c(beginnings,paste0(str3[[1]][1],"_",str3[[1]][2],"_",as.numeric(str4)))
    }
    beginnings<-matrix(beginnings,ncol=1)
  }
  
  
  
  #Loop through the information files of all videos and extract relevant information.
  for(i in 1:length(number_mp4s)){
    #Read in relevant frames table.
    significant_frames<-read.table(paste0(main_dir,"/",date,"/",species,"/raw_a_",mp4_names[i],".txt"),header=F)
    #This calculates duration of video in seconds.
    rec_time[i,10]<-significant_frames[length(significant_frames[,1])-1,1]/significant_frames[length(significant_frames[,1]),1]
    #This pastes fps of video into table.
    rec_time[i,11]<-significant_frames[length(significant_frames[,1]),1]
    #File name of chunk.
    rec_time[i,1]<-as.character(mp4_names[i])
    if(trustGP==1){
      starter<-read.table(paste0(main_dir,"/",date,"/",species,"/forR_",mp4_names[i],"_info_new_info"),header=F)
      #The following lines split the strings according to separators such that
      #we can retrieve the relevant information (only recording date in this case.
      str1<-strsplit(as.character(starter[1,3]),"-")
      str2<-strsplit(str1[[1]],"T")
      #This uses the split string to fill in information about date.
      rec_time[i,2]<-as.numeric(str1[[1]][1])
      rec_time[i,3]<-as.numeric(str1[[1]][2])
      rec_time[i,4]<-as.numeric(str2[[3]][1])
    } else{
      #split the date into yy mm dd
      rec.date<-strsplit(date,"\\.| |_|/")
      #Put a "20" in front of year if user didn't do so.
      if(nchar(rec.date[[1]][1])==2){
        rec.date[[1]][1]<-paste0("20",rec.date[[1]][1])
      }
      rec_time[i,2]<-as.numeric(rec.date[[1]][1])
      rec_time[i,3]<-as.numeric(rec.date[[1]][2])
      rec_time[i,4]<-as.numeric(rec.date[[1]][3])
    }
    
  }
  
  #Split beginning of single videos into hh mm ss
  split_beg<-strsplit(beginnings[,1],"_")
  #Translate into seconds
  beg_sec<-sapply(1:length(split_beg),function(x) as.numeric(split_beg[[x]][1])*3600+as.numeric(split_beg[[x]][2])*60+as.numeric(split_beg[[x]][3]))
  #Now, we go through the table and calculate the beginning of each chunk.
  #If it is an initiator chunk, it gets the initiator time. All other get their times
  #by summing up the videos from the same recording that occured before.
  #First table entry is a separate case, as we would otherwise loop over 1:0.
  beg_sec_each_chunk<-beg_sec[VIDEO_ID[1]]
  if(length(rec_time[,1])>1){
    beg_sec_each_chunk<-c(beg_sec_each_chunk,sapply(2:length(rec_time[,1]),function(x) beg_sec[VIDEO_ID[x]]+sum(rec_time[(1:length(VIDEO_ID[1:(x-1)]))[VIDEO_ID[1:(x-1)]==VIDEO_ID[x]],10])))
  }
  
  #Enter them split up into hh, mm, ss into the table
  rec_time[,5]<-floor(beg_sec_each_chunk/3600)
  rec_time[,6]<-floor((beg_sec_each_chunk-(rec_time[,5]*3600))/60)
  rec_time[,7]<-round(beg_sec_each_chunk-(rec_time[,5]*3600+rec_time[,6]*60),2)
  
  
  for(i in 1:length(number_mp4s)){
    #This translates recording time during day into seconds after midnight.
    rec_time[i,8]<-rec_time[i,5]*3600+rec_time[i,6]*60+rec_time[i,7]
  }
  
  #Write the video-information table
  write.table(rec_time,paste0(main_dir,"/",date,"/",species,"/video_info.txt"),row.names=F)
  
}


###Now we smooth the raw file with the frame numbers that fulfill the condition (motion/color).
#We try to overcome gaps of few frames that the program does not notice to get a perfect continuous motion/color
#supercut. Moreover, chunks that overlap or are close to each will be merged.
#A timestamps file will be created as well.
#A file giving a percentage bar for duration of each chunk will also be created.

#On this variable, we will safe whether there was any motion or not. 
with_motion<-NULL
#Loop through the information files of all videos and extract relevant information.
for(i in 1:length(number_mp4s)){
  #Read in relevant frames table.
  significant_frames<-read.table(paste0(main_dir,"/",date,"/",species,"/raw_a_",mp4_names[i],".txt"),header=F)
  
  #If there is more in the table than just the fps and the total number of frames.
  if(length(significant_frames[,1])>2){
    with_motion<-c(with_motion,i)
    
    #Retrieve number of frames and frames per second.
    number_f<-significant_frames[length(significant_frames[,1])-1,1]
    fps<-significant_frames[length(significant_frames[,1]),1]
    
    frametable<-NULL
    timetable<-NULL
    
    #Get 3/4 of a second before and 3/4 of a second after each frame. 
    for(framo in 1:(length(significant_frames[,1])-2)){
      frametable<-c(frametable,max(significant_frames[framo,1]-round(0.75*fps),1):min(significant_frames[framo,1]+round(0.75*fps),number_f))
    }
    #Then discard all duplicates.
    #Now frames that were close together got merged into one chunk (if max. 1 sec apart) 
    #and also, there is a bit of a buffer before and after each motion/color appearance.
    frametable<-frametable[!duplicated(frametable)]
    
    #This whole code is only to give a bar for the length of each chunk in the supercut.
    #First, find where new chunk begins.
    chunk_pos<-1
    for(bar in 2:length(frametable)){
      if(frametable[bar]>frametable[bar-1]+1){
        chunk_pos<-c(chunk_pos,bar)
      }
    }
    
    #This calculates the length of each bar in each frame.
    bar<-NULL
    for(barlength in 1:length(chunk_pos)){
      if(barlength!=length(chunk_pos)){
        diff_fr<-chunk_pos[barlength+1]-chunk_pos[barlength]-1
        bar<-c(bar,0,(1:diff_fr)*(1/diff_fr))
      } else{
        diff_fr<-length(frametable)-chunk_pos[barlength]
        bar<-c(bar,0,(1:diff_fr)*(1/diff_fr))
      }
    }
    #Find out in which row of the rec_time table (created way before) we are.
    current<-(1:length(rec_time[,1]))[rec_time[,1]==mp4_names[i]]
    
    #Transform beginning of video again into absolute seconds of day.
    sec_of_day<-floor(rec_time[current,5]*3600+rec_time[current,6]*60+rec_time[current,7])
    #This takes several asthetic steps.
    timetable<-sapply(1:length(frametable),function(x) sec_of_day+floor(frametable[x]/fps))
    timetable_h<-sapply(1:length(timetable),function(x) floor(timetable[x]/3600))
    timetable_m<-sapply(1:length(timetable),function(x) floor((timetable[x]-timetable_h[x]*3600)/60))
    timetable_s<-sapply(1:length(timetable),function(x) timetable[x]-timetable_h[x]*3600-timetable_m[x]*60)
    timetable_h<-as.character(timetable_h)
    timetable_m<-as.character(timetable_m)
    timetable_s<-as.character(timetable_s)
    for(put0 in 1:length(timetable)){
      if(nchar(timetable_h[put0])==1){
        timetable_h[put0]<-paste0(0,timetable_h[put0])
      }
      if(nchar(timetable_m[put0])==1){
        timetable_m[put0]<-paste0(0,timetable_m[put0])
      }
      if(nchar(timetable_s[put0])==1){
        timetable_s[put0]<-paste0(0,timetable_s[put0])
      }
    }
    timetable<-sapply(1:length(timetable),function(x) paste0(timetable_h[x],":",timetable_m[x],":",timetable_s[x]))
    
    #Output the files for later use in C++ program.
    write.table(timetable,paste0(main_dir,"/",date,"/",species,"/time_a_",mp4_names[i],".txt"),row.names=F,col.names=F,quote=F)
    write.table(frametable,paste0(main_dir,"/",date,"/",species,"/frame_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    write.table(bar,paste0(main_dir,"/",date,"/",species,"/bar_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    #This creates the table that shows in which video unit we are.
    current_video_unit<-rec_time[rec_time[,1]==mp4_names[i],9]
    write.table(rep(current_video_unit,length(frametable)),paste0(main_dir,"/",date,"/",species,"/unit_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    
    
  } else{
    #If nothing happened, output empty tables.
    write.table(NULL,paste0(main_dir,"/",date,"/",species,"/time_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    write.table(NULL,paste0(main_dir,"/",date,"/",species,"/frame_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    write.table(NULL,paste0(main_dir,"/",date,"/",species,"/bar_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
    write.table(NULL,paste0(main_dir,"/",date,"/",species,"/unit_a_",mp4_names[i],".txt"),row.names=F,col.names=F)
  }
}


#This outputs the list of video files ordered by recording time.
write.table(sapply(with_motion,function(x) paste0("a_",rec_time[x,1])),paste0(main_dir,"/",date,"/",species,"/all_files.txt"),row.names=F,col.names=F,quote=F)



### END