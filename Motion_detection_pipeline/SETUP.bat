@ECHO OFF
REM Let Windows not echo anything unless it is asked to do so

REM Written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~ May 2018 for a Windows machine

REM This batch file can be executed either by double-clicking on the file name or calling it from the console.

REM This file "installs" the whole program, i.e. it sets the system directories.
REM If you made a mistake, just go into the folder \Directories and change the settings in the respective txt files.
REM Make SURE that the folder in which all these files, including SETUP.bat, are safed is called cut_video_by_condition.

REM This allows variables to change over the course of the script. Further on, variables have to be called with an exclamation mark in the beginning and at the end
setlocal enabledelayedexpansion

REM Make sure this folder is called cut_video_by_condition
rename (for %%* in (.) do echo %%~nx*) cut_video_by_condition

REM Create folder directories
IF NOT EXIST directories MKDIR directories

cd..

echo Notice that settings can be changed again later on by opening the SETUP again or by changing the settings manually in the folder "directories" (recommended). 
REM Ask the user which program he/she wants to use
set /p motion_or_color="Do you want to track movement by detection of objects moving into frame or by a certain color spotted in the frame? Type 1 for motion or 2 for color detection: "
REM Safe decision in file
(echo !motion_or_color!)>cut_video_by_condition\directories\motion_or_color.txt


REM Ask if GoPro time will be trusted
set /p trust_gopro="Do you want to trust the GoPro system time or do you want to enter the recording times manually? We experienced that GoPro has problems keeping the system time precise (ever day it will go about 3-4sec ahead), meaning that over time, the system time gets more and more different to the true time. The system time can though be synced to the computer/cellphone time with the GoPro app. If you cannot sync for several days or need very precise time, we recommend manual entry. Type 1 for GoPro system time and 2 for always manually entering."
(echo !trust_gopro!)>cut_video_by_condition\directories\trust_gopro.txt


IF !motion_or_color!==1 (
REM Ask the user for the sensitivity for motion and for intensity of blurring.
set /p sensitivity="Set the sensitivity of the program (e.g. 80): "
set /p blur="Set the intensity of blur to reduce noise (e.g. 10): "
REM Ask the user for the size of the ignored area.
set /p bottom="This and the following parameters specify the area of interest in the videos by defining a rectangular shaped area in the frame that gets ignored. The region of interest is thus not an included are, but it excludes an area inside of it. The setting for this and the following 3 parameters has to lie between 0 and 1. The sum of the proportion of left and right as well as the sum of the proportion of bottom and top cannot exceed 1. If at least one of the sums equals 1, no area in the video will be ignored (e.g. 0.5, 0.5, 0.5, 0.5). If both sums are 0, the whole video will be ignored. For this current parameter, select the area on the bottom of the video that is of interest, e.g. a proportion 0.25 of the y-dimension pixels is of interest, looking from the lower side: "
set /p left="Set the proportion of pixels on left edge that are of interest, e.g. 0.25: "
set /p top="Set the proportion of pixels on top edge that are of interest, e.g. 0.25: "
set /p right="Set the proportion of pixels on right edge that are of interest, e.g. 0.25: "
set /p plot_RONI="Now decide whether for each video that gets processed by the program, if a visualization of the area of no interest should be stored in the folder in which the respective video is stored. Type y for yes and n for n: "
)
REM Save as txt file (no space before > , otherwise there is a space in the txt file; IMPORTANT also for all the next pipings). Do this in a new IF clause, as inside the other IF, echo is off.
IF !motion_or_color!==1 (
echo !sensitivity! !blur! !bottom! !left! !top! !right! !plot_RONI!>cut_video_by_condition\directories\exe_program_parameters.txt
)

IF !motion_or_color!==2 (
REM Ask the user for the BGR minimums and maximums
set /p B_min="Specify the lowest value for Blue in RGB code: "
set /p G_min="Specify the lowest value for Green in RGB code: "
set /p R_min="Specify the lowest value for Red in RGB code: "
set /p B_max="Specify the highest value for Blue in RGB code: "
set /p G_max="Specify the highest value for Green in RGB code: "
set /p R_max="Specify the highest value for Red in RGB code: "
)
IF !motion_or_color!==2 (
REM Save as txt file
echo !B_min! !G_min! !R_min! !B_max! !G_max! !R_max!>cut_video_by_condition\directories\exe_program_parameters.txt
)

REM Set directory to ffmpeg
REM Pipe result into file 
set /p ffmpeg_dir="Enter directory to ffmpeg (e.g. C:\Users\xxx\Desktop\ffmpeg\bin\ffmpeg.exe or just ffmpeg, if set as path variable): "
echo !ffmpeg_dir!>cut_video_by_condition\directories\ffmpeg_dir.txt

REM Set directory to R
set /p R_dir="Enter directory to R (e.g. C:\Program Files\R\R-3.4.2\bin\R.exe or just R, if set as path variable): "
echo !R_dir!>cut_video_by_condition\directories\R_dir.txt

REM Ask user if video data will be extracted from SD card or is on computer
set /p SD_or_PC="Shall the program extract videos from user defined locations (e.g. from a SD card) or are the files inside the same folders where the program will produce its output in? Type 1 for external path to videos and 2 for in-folder videos: "

REM Safe decision in file
(echo !SD_or_PC!)>cut_video_by_condition\directories\SD_or_PC.txt

REM If the data comes from external, the user has to say how many units of video material he/she has.
IF !SD_or_PC!==1 (
set /p number_SD="From how many directories shall video material be extracted (e.g. how many SD cards are plugged in): "
) 

REM Ask the user to set directories for extraction and respective folder names where output should be stored.
REM Delete previous versions of these files.
IF !SD_or_PC!==1 (
IF EXIST cut_video_by_condition\directories\output_folder.txt (
del cut_video_by_condition\directories\output_folder.txt
)
IF EXIST cut_video_by_condition\directories\extract_folder.txt (
del cut_video_by_condition\directories\extract_folder.txt
)
FOR /L %%g IN (1,1,!number_SD!) DO (
set /p folder_name="Give the folder name of the output directory number %%g in your local results folder, e.g. in the imaginative folder 17_12_18, the first subfolder could be called Species%%g: "
set /p ext_dir_name="Give the directory from where video material should in the future ALWAYS be extracted into the previously defined subfolder, e.g. G:\DCIM\100GOPRO: "
(echo !folder_name!)>>cut_video_by_condition\directories\output_folder.txt
(echo !ext_dir_name!)>>cut_video_by_condition\directories\extract_folder.txt
)
)

REM Ask user if raw videos should be deleted.
set /p delete_raw="Do you want the program to automatically delete your raw video files (the muted files will still exist if you decide so during the next question!) (as well as LRV and THM files)? Press 1 for yes and 2 for no: "
REM Safe decision in file
(echo !delete_raw!)>cut_video_by_condition\directories\delete_or_keep.txt

REM Ask user if he/she plans on rerunning the code.
set /p rerun_yn="Do you plan to potentially rerun the code over the videos with different parameter settings after you saw the supercut video (in each folder, a batch script for rerunning will be produced for you. Furthermore, the muted videos will NOT be deleted, which are necessary for rerunning under new parameters. Put 1 for potential rerunning and 2 for no rerunning: "
REM Safe decision in file
(echo !rerun_yn!)>cut_video_by_condition\directories\rerun_yn.txt

REM Ask in which codec the output should be produced.
set /p fourcc="With which codec do you want to encode your output supercut video? We recommend x264 (which is similar to the format GoPro videos (at least those with high resolution) are encoded in). Make sure that the codec you select is installed on your system: either set in the system or the dll is safed in you cut_video_by_motion folder (we had to download the x264 codec, for instance). Please type the four characters (fourcc code) with spaces in between and letters as capitals(e.g. X 2 6 4): "
(echo !fourcc!)>cut_video_by_condition\directories\fourcc.txt

REM Ask user for the fps rate he wants his supercut to be in.
set /p fps="Which frame rate per second should the produced supercut have?"
(echo !fps!)>cut_video_by_condition\directories\fps.txt


REM Ask the user for dimension of video in pixels.
set /p width="What is the width of your videos in pixels? E.g. 1920: "
set /p height="What is the height of your videos in pixels? E.g. 1080: "
(echo !width!)>cut_video_by_condition\directories\width.txt
(echo !height!)>cut_video_by_condition\directories\height.txt

REM Ask the user for font size of timestamp.
set /p font_size="What is the font size (floating point number) of the timecode on the supercut? Sorry, we didn't manage to fit the size independent of video dimensions. You have to try a bit around. Enter a 3 if your video is 1920x1080 (this fits perfectly): "
(echo !font_size!)>cut_video_by_condition\directories\font_size.txt



REM Extract main batch script to main folder.
copy cut_video_by_condition\copy_main_program.bat CUT_VIDEO_BY_CONDITION.bat




REM END