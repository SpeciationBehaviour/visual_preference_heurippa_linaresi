@ECHO OFF
REM Let Windows not echo anything unless it is asked to do so

REM Written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~ May 2018 for a Windows machine

REM This batch file can be executed either by double-clicking on the file name or calling it from the console.

REM This script executes parts of the main batch script. It can be called by the user if a new supercut under new parameter settings should be created.

REM As typical for GoPRO, videos will be stored by the camera as split into small chunks which all have the same identifier at the end of the filename. 
REM The goal: Get a video file in which all chunks which fullfill certain criteria (movement or color) of the whole recording day are clipped together.

REM This batch script executes the following tasks (by itself or by calling other programs):
REM IMPORTANT: To make this program work, you should not delete any of the files that were produced in the run before!
REM a) It cuts out chunks with movements/colors, clips them together and adds timestamps (time of day), the number of recording unit and a progress bar.



REM This allows variables to change over the course of the script. Further on, variables have to be called with an exclamation mark in the beginning and at the end
setlocal enableextensions enabledelayedexpansion


IF NOT EXIST a_* echo ERROR No muted video in this folder    EXIT
IF NOT EXIST video_info.txt echo ERROR No file video_info.txt in this folder    EXIT
IF NOT EXIST a_* EXIT [/B]
IF NOT EXIST video_info.txt EXIT [/B]





REM Set current directory
set current_dir=!cd!

REM Safe folder name (species) in which are right now.
for %%F in ("!cd!") do set "species=%%~nxF"

REM Go into main directory and safe date on the way.
cd..
for %%F in ("!cd!") do set "current_date=%%~nxF"
cd..


REM Update main directory file
cd > cut_video_by_condition\directories\main_dir.txt

REM Read in directories and save them on variables.
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\main_dir.txt`) DO (
SET main_dir=%%F
)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\ffmpeg_dir.txt`) DO (
SET ffmpeg_dir=%%F
)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\R_dir.txt`) DO (
SET R_dir=%%F
)

REM Read in the decision the user has made (motion or color detection) and read in user set parameter
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\motion_or_color.txt`) DO (
SET motion_or_color=%%F
)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\exe_program_parameters.txt`) DO (
SET program_parameters=%%F
)

REM Read in the decision the user has made (trusting GoPro system time, yes or no)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\trust_gopro.txt`) DO (
SET trust_gopro=%%F
)

REM Read in the fourcc code the user wishes the output to be in
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\fourcc.txt`) DO (
SET fourcc=%%F
)

REM Read in the decision the user has made (frame rate of supercut)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\fps.txt`) DO (
SET fps=%%F
)

REM Read in the decision the user has made (font size of timestamps in supercut)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\font_size.txt`) DO (
SET font_size=%%F
)

REM Read in the decision the user has made (height of video material in pixel)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\height.txt`) DO (
SET height=%%F
)

REM Read in the decision the user has made (width of video material in pixel)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\width.txt`) DO (
SET width=%%F
)



IF !motion_or_color!==1 (
REM Ask the user for the sensitivity for motion and for intensity of blurring.
set /p sensitivity="Set the sensitivity of the program (e.g. 80): "
set /p blur="Set the intensity of blur to reduce noise (e.g. 10): "
REM Ask the user for the size of the ignored area.
set /p bottom="This and the following parameters specify the area of interest in the videos by defining a rectangular shaped area in the frame that gets ignored. The region of interest is thus not an included are, but it excludes an area inside of it. The setting for this and the following 3 parameters has to lie between 0 and 1. The sum of the proportion of left and right as well as the sum of the proportion of bottom and top cannot exceed 1. If both sums are 1, no area in the video will be ignored. If both sums are 0, the whole video will be ignored. For this current parameter, select the area on the bottom of the video that is of interest, e.g. a proportion 0.25 of the y-dimension pixels is of interest, looking from the lower side: "
set /p left="Set the proportion of pixels on left edge that are of interest, e.g. 0.25: "
set /p top="Set the proportion of pixels on top edge that are of interest, e.g. 0.25: "
set /p right="Set the proportion of pixels on right edge that are of interest, e.g. 0.25: "
set /p plot_RONI="Now decide whether for each video that gets processed by the program, if a visualization of the area of no interest should be stored in the folder in which the respective video is stored. Type y for yes and n for n: "
)
IF !motion_or_color!==1 (
REM Safe on one variable
set program_parameters=!sensitivity! !blur! !bottom! !left! !top! !right! !plot_RONI!
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
REM Safe on one variable
set program_parameters=!B_min! !G_min! !R_min! !B_max! !G_max! !R_max!
)
)





REM Move into the right folder.
cd !current_dir!




echo !date!  !time!
echo ------------------------
echo ------------------------
echo BEGIN!
echo Extract frames that match conditions defined by new parameter settings
echo ------------------------
echo ------------------------


REM Apparently this makes it easier for OpenCV to read in a video.
set str_for_c=!main_dir!\!current_date!\!species!\
set str_for_c=!str_for_c:\=\\!

REM Safe which videos we have in the folder to be still analyzed.
for %%a in (a_*.mp4) do (echo %%~nxa)>>remaining.txt

REM Call the C++/OpenCV program to output a table for each video which contains all frames that match a certain given condition (either
REM color signal or motion into frame). The second last entry in the created table is the number of frames and the last the frame rate.
REM Depending on the decision of the user, either the motion or color detection program will be called.
REM Since the program may crash on a certain video, we have to make sure that it gets run again.

if %motion_or_color%==1 (
REM This is a while loop which ends once all videos were analyzed. If the code successfully runs through a video, it gives out a "done" in the end of the table.
REM This "done" we have to then remove again.
REM Check at the end of the document for the code to the while loop. This has to be solved with a subroutine in the end of the script since for loops with
REM such while statement fail looping.
set continue_run=1
CALL :while1
for %%a in (raw_*) do (
set row=
for /F "delims=" %%j in (%%a) do (
if defined row echo.!row!>> new_%%a
set row=%%j
)
del %%a
ren new_%%a %%a
)
del remaining.txt
)


if %motion_or_color%==2 (
REM This is a while loop which ends once all videos were analyzed. If the code successfully runs through a video, it gives out a "done" in the end of the table.
REM This "done" we have to then remove again.
REM Check at the end of the document for the code to the while loop. This has to be solved with a subroutine in the end of the script since for loops with
REM such while statement fail looping.
set continue_run=1
CALL :while2
for %%a in (raw_*) do (
set row=
for /F "delims=" %%j in (%%a) do (
if defined row echo.!row!>> new_%%a
set row=%%j
)
del %%a
ren new_%%a %%a
)
del remaining.txt
)





set main_dir_R=!main_dir!
REM Replace \ in directory by / for R.
set main_dir_R=!main_dir_R:\=/!

REM Call R.
REM This script collects all video information together in one txt file. It then produces output for the next C++ program according to which it will cut together the supercut. For details, check comments on the R script.
"!R_dir!" CMD BATCH --vanilla --slave "--args !main_dir_R! !current_date! !species!" !main_dir!\cut_video_by_condition\Extract_video_info1.R



echo ------------------------
echo ------------------------
echo !date!  !time!
echo All relevant frames with new parameter settings found
echo The program produces a video with the relevant chunks of the video being clipped together and presented with the time of day and the video recording number shown on it 
echo This will take a while
echo ------------------------
echo ------------------------


REM Run OpenCV program to cut new video together.
REM The program will output a supercut for each video which will be pasted together by ffmpeg later.
REM Save old all_files.txt (produced by R before) in a separate txt.
copy all_files.txt all_files_corr_order.txt

REM This while loop is another step to make sure the code does not crash before the final supercut is being done.
REM Run OpenCV until every video was processed. Those video files that could not be cut together will be cut together again until they work.

REM Check at the end of the document for the code to the while loop. This has to be solved with a subroutine in the end of the script since for loops with
REM such while statement fail looping.
set continue_run=1
CALL :while3

REM Delete leftovers
del all_files.txt
del done_supercut.txt

REM Now create a table with the names of all supercuts of each video.
for /f "delims=" %%a in (all_files_corr_order.txt) do (echo file 'supercut_!current_date!_!species!_%%~nxa_chunk.mp4')>>names.txt

REM Clip those together into one supercut
!ffmpeg_dir! -f concat -safe 0 -i names.txt -c copy NEW_supercut_!current_date!_!species!.mp4

REM Delete leftovers
del names.txt
del *chunk.mp4
del *info
del all_files_corr_order.txt
del unit_*
del bar_*
del frame_*
del raw_*
del time_*



REM Delete all files that are not needed anymore or are just unnecessary leftovers.
del !main_dir!\cut_video_by_condition\Extract_video_info1.Rout



echo !date!  !time!
echo ------------------------
echo ------------------------
echo DONE
echo You can find your new supercut under the name NEW_supercut_!current_date!_!species!.mp4 in this folder 
echo In case you want to rerun the program again afterwards, make sure to rename this supercut, as it would be overwritten otherwise
echo ------------------------
echo ------------------------


REM Echo settings
echo %program_parameters% > param_rerun.txt



REM End of main script.
REM Here come the subroutines




GOTO :EOF

		:while1
		for %%R in (remaining.txt) do if %%~zR lss 1 set continue_run=0
		if !continue_run! ==1 (
				for /f "delims=" %%a in (remaining.txt) do (
					REM These weird bracket settings seem to be key for the program to run on my computer
					((!main_dir!\cut_video_by_condition\MovementIntoFrame.exe !str_for_c! %%a !program_parameters!) > raw_%%a.txt)
				)
				del remaining.txt
				echo. 2>remaining.txt
				for %%a in (raw_*) do (
					FindStr "done" %%a >Nul:
					If !ErrorLevel!==1 (
						set line=%%a
						echo !line:~4,-4!>>remaining.txt
						echo Error reading muted file !line:~4,-4! in folder number !loop_counter! during frame extraction
						echo DO NOT WORRY. The same step will be executed again until it works
					)
				)
		)
		if !continue_run!==1 goto :while1
		EXIT /B

		:while2
		for %%R in (remaining.txt) do if %%~zR lss 1 set continue_run=0
		if !continue_run! ==1 (
				for /f "delims=" %%a in (remaining.txt) do (
					REM These weird bracket settings seem to be key for the program to run on my computer
					((!main_dir!\cut_video_by_condition\CheckMotion.exe !str_for_c! %%a !program_parameters!) > raw_%%a.txt)
				)
				del remaining.txt
				echo. 2>remaining.txt
				for %%a in (raw_*) do (
					FindStr "done" %%a >Nul:
					If !ErrorLevel!==1 (
						set line=%%a
						echo !line:~4,-4!>>remaining.txt
						echo Error reading muted file !line:~4,-4! in folder number !loop_counter! during frame extraction
						echo DO NOT WORRY. The same step will be executed again until it works
					)
				)
		)
		if !continue_run!==1 goto :while2
		EXIT /B

		:while3
		for %%R in (all_files.txt) do if %%~zR lss 1 set continue_run=0
		if !continue_run! ==1 (
			!main_dir!\cut_video_by_condition\CutVideoByCondition.exe all_files.txt !str_for_c! !current_date! !species! !height! !width! !fps! !font_size! !fourcc!>> done_supercut.txt
			for /f "delims=" %%a in (done_supercut.txt) do (
				findstr /V "%%a" all_files.txt > all_files_temp.txt
				del all_files.txt
				ren all_files_temp.txt all_files.txt
			)
		)
		if !continue_run!==1 goto :while3
		EXIT /B