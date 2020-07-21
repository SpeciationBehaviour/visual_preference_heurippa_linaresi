@ECHO OFF
REM Let Windows not echo anything unless it is asked to do so

REM Written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~ May 2018 for a Windows machine

REM This batch file can be executed either by double-clicking on the file name or calling it from the console.

REM To make this batch script and all other scripts and programs called by it work, you have to click and follow the SETUP.bat according
REM to your the settings on your system.
REM Videos are split up by dates of recording and in each such folder, videos are split into subfolders.
REM It is important that the main folder name follows the format yy_mm_dd or yyyy_mm_dd !!!

REM This program can only analyze one such folder, if clicked. If you want to run it over multiple folders, you have to write a batch file that does this.
REM If you run the program with raw video files being stored at the same location where the output will be produced in, then those videos need to
REM be ordered in subfolders by video class, e.g. inside 17_12_18 are two subfolders, species1 and species 2 with video data on both of them.
REM Even if there is only one set of videos, it still needs to go into a subfolder.
REM If data is being extracted from external location, program will create those subfolders in case they don't exist yet.
REM Attention: Extraction location as well as output location can never contain any other .mp4 files that are of no current interest.
REM This program will use ALL available videos (but only .mp4s!). 

REM As typical for GoPRO, videos will be stored by the camera as split into small chunks which all have the same identifier at the end of the filename. 
REM The goal: Get a video file in which all chunks which fullfill certain criteria (movement or color) of the whole recording day are clipped together.

REM This batch script executes the following tasks (by itself or by calling other programs):
REM a) It removes audio from our GoPRO video files (this is necessary for the code to work)
REM b) It cuts out chunks with movements/colors, clips them together and adds timestamps (time of day), the number of recording unit and a progress bar.



REM This allows variables to change over the course of the script. Further on, variables have to be called with an exclamation mark in the beginning and at the end
setlocal enableextensions enabledelayedexpansion

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

REM Read in the decision the user has made (external data or internal PC)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\SD_or_PC.txt`) DO (
SET SD_or_PC=%%F
)

REM Read in the decision the user has made (deleting raw files, yes or no)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\delete_or_keep.txt`) DO (
SET delete_raw=%%F
)

REM Read in the decision the user has made (potentially rerunning code, yes or no)
FOR /F "tokens=* USEBACKQ" %%F IN (`more cut_video_by_condition\directories\rerun_yn.txt`) DO (
SET rerun_yn=%%F
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



REM This asks the user in the console to enter the folder name of which he wishes the videos to be processed. As used as example before, this could be "18_05_17" or "2018_05_17"
REM and saves it as the variable current_date. This guides any further step to the right directories.
set /p current_date="Enter name of folder to be processed (the folder has to be named after the date of recording in the format yy_mm_dd yyyy_mm_dd): "

REM If this folder does not exist, close program.
IF NOT EXIST !current_date! echo ERROR Folder does not exist EXIT
IF NOT EXIST !current_date! EXIT [/B]


REM Move into the right folder.
cd !current_date!


REM If the user selected not to give out- and input folders, create those. First check if there are old ones and delete.
IF !SD_or_PC!==2 (
IF EXIST !main_dir!\cut_video_by_condition\directories\output_folder.txt (
del !main_dir!\cut_video_by_condition\directories\output_folder.txt
)
IF EXIST !main_dir!\cut_video_by_condition\directories\extract_folder.txt (
del !main_dir!\cut_video_by_condition\directories\extract_folder.txt
)
for /d %%a in ("*") do (echo !main_dir!\!current_date!\%%~nxa)>>!main_dir!\cut_video_by_condition\directories\extract_folder.txt
for /d %%a in ("*") do (echo %%~nxa)>>!main_dir!\cut_video_by_condition\directories\output_folder.txt
)

REM If extracting from external source: If a folder from the given list doesn't exist yet, create it.
IF !SD_or_PC!==1 (
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\output_folder.txt) do (
IF NOT EXIST %%a (
MKDIR %%a
)
)
)


REM Count the number of folders that exist for creating output
set folder_count=0
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\output_folder.txt) do (
set /a folder_count=!folder_count!+1
)



REM If we don't trust the GoPro time. In the extract folders, check how many x starter videos we have. Then, ask the user to give the x 
REM beginnings of recording.
IF !trust_gopro!==2 (

REM Get loop counter
set extract_counter=1


REM The following commands are executed for each subfolder in the folder selected by the user.
for /L %%x in (1,1,!folder_count!) do (

REM Get current folder name of the loop
set extract_det_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\output_folder.txt) do (
IF !extract_det_counter!==!extract_counter! (
set species=%%a
)
set /a extract_det_counter=!extract_det_counter!+1
)

REM Get current extraction location of the loop
set extract_det_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\extract_folder.txt) do (
IF !extract_det_counter!==!extract_counter! (
set extract=%%a
)
set /a extract_det_counter=!extract_det_counter!+1
)

REM Update loop counter
set /a extract_counter=!extract_counter!+1

REM Access subfolder.
cd !species!

set currently=!cd!
cd !extract!
REM Count how many starters are in extract folder
set /a count=0
for /F "delims=" %%i in ('dir/s/b/a-d "!extract!\GOPR*"') do (set /a count=count+1)

cd !currently!

FOR /L %%g IN (1,1,!count!) DO (
set /p time_video="Give beginning of recording number %%g of type "!species!" in the format hh_mm_ss (hh goes from 00 to 23, no am/pm! E.g. 13_05_45)."
(echo !time_video!)>>beginnings.txt
)

cd ..

)
)



echo !date!  !time!
echo ------------------------
echo ------------------------
echo BEGIN!
echo Mute the videos
echo ------------------------
echo ------------------------



REM This first big loop covers the tasks until the point where the raw video files were copied to the folder in which the program will strip off sound.
REM Running these two loops separately allows to eject all external media before program is finished, if data gets extracted from external memory device.

REM Get loop counter
set extract_counter=1

REM The following commands are executed for each subfolder in the folder selected by the user.
for /L %%x in (1,1,!folder_count!) do (

REM Get current folder name of the loop
set extract_det_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\output_folder.txt) do (
IF !extract_det_counter!==!extract_counter! (
set species=%%a
)
set /a extract_det_counter=!extract_det_counter!+1
)


REM Get current extraction location of the loop
set extract_det_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\extract_folder.txt) do (
IF !extract_det_counter!==!extract_counter! (
set extract=%%a
)
set /a extract_det_counter=!extract_det_counter!+1
)

REM Update loop counter
set /a extract_counter=!extract_counter!+1


REM Access the subfolder
cd !species!

REM This is a safety step: If this script was already run before, this mp4 file should already exist in the directory. Thus the program is automatically closed.
IF EXIST supercut* EXIT [/B]

REM Execute this NO MATTER if we trust GoPro system time or not. If we don't trust it, we still make use of the fact that it would most likely not suddently record reversly.
REM Call ffmpeg to extract all video information from ALL mp4 files.
for %%a in (!extract!\G*.mp4) do !ffmpeg_dir! -i %%~a > %%~nxa_info 2>&1
REM Extract the line that contains the string "creation_time" and pipe it into a txt file. This contains the date and the time when the recording was started.
for %%i in (*info) do findstr /R [a-z]*creation_time  %%i > forR_%%i_new_info

REM Strip the audio off from each .mp4 video and save it as a new video under the same name with "a_" in front. 
REM ffmpeg does not offer to overwrite files, therefore it has to be done more complicatedly.
REM If wished in setup, delete all raw files inside folder immediately after ffmpeg output.
IF !delete_raw!==1 (
for %%a in (!extract!\*.mp4) do (
!ffmpeg_dir! -i %%~a -vcodec copy -an a_%%~nxa -hide_banner
REM Get file name without MP4
set before=%%~a
REM Delete last characters
set after=!before:~,-3!
REM Add video with audio and the respective LRV and THM file to delete list.
echo !before!>>del_list.txt 
echo !after!LRV>>del_list.txt 
echo !after!THM>>del_list.txt 
)
for /f "delims=" %%a in (del_list.txt) do del %%a
del del_list.txt
)
REM Else keep them
IF !delete_raw!==2 (
for %%a in (!extract!\*.mp4) do !ffmpeg_dir! -i %%~a -vcodec copy -an a_%%~nxa -hide_banner
)


REM Move out of the subfolder again
cd ..

)




REM Get loop counter
set loop_counter=1


REM The following commands are executed for each subfolder in the folder selected by the user.
for /L %%x in (1,1,!folder_count!) do (


REM Get current folder name of the loop
set detect_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\output_folder.txt) do (
IF !detect_counter!==!loop_counter! (
set species=%%a
)
set /a detect_counter=!detect_counter!+1
)


REM Get current extraction location of the loop
set detect_counter=1
for /f "delims=" %%a in (!main_dir!\cut_video_by_condition\directories\extract_folder.txt) do (
IF !detect_counter!==!loop_counter! (
set extract=%%a
)
set /a detect_counter=!detect_counter!+1
)



REM Access the subfolder
cd !species!



echo ------------------------
echo ------------------------
echo !date!  !time!
echo Extraction of all video files from all given folders completed
echo External devices can be ejected if videos were extracted from those
echo The program currently runs through video footage of folder number !loop_counter! and searches for relevant frames. This will take a while
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
echo Extraction of all video files from all given folders completed and all relevant frames found
echo Folder number !loop_counter!. The program produces a video with the relevant chunks of the video being clipped together and presented with the time of day and the video recording number shown on it 
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
!ffmpeg_dir! -f concat -safe 0 -i names.txt -c copy supercut_!current_date!_!species!.mp4

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

REM Delete muted videos if no rerunning is planed
if !rerun_yn!==2 (
del a_*
)
REM If it is planned, copy rerun script.
if !rerun_yn!==1 (
copy !main_dir!\cut_video_by_condition\copy_rerun_opencv_code.bat 000_rerun_opencv_code.bat
)

REM Copy Parameter settings
copy !main_dir!\cut_video_by_condition\directories\exe_program_parameters.txt used_parameters.txt

REM Leave the subfolder.
cd .. 

REM Delete all files that are not needed anymore or are just unnecessary leftovers.
del !main_dir!\cut_video_by_condition\Extract_video_info1.Rout


REM Update loop counter
set /a loop_counter=!loop_counter!+1

)


echo !date!  !time!
echo ------------------------
echo ------------------------
echo DONE
echo ------------------------
echo ------------------------



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
						echo Error reading muted file !line:~4,-4! originating from folder number !loop_counter! during frame extraction
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
						echo Error reading muted file !line:~4,-4! originating from folder number !loop_counter! during frame extraction
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