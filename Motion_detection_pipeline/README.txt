**README**
Written by Alexander E. Hausmann (alexander_hausmann@gmx.net) in ~May 2018 (and updated later a couple of times)

The code in this folder was written specifically for a Windows machine. Single parts of the pipeline
could quite easily be modified to run on other machines as well (if you are interested in that, contact us). 
The pipeline was specifically written for analyses of videos shot with a GoPro camera, which has the problem that it can only 
shoot with sound line switched on (which doesn't work with OpenCV). Again, it would probably not be too much work to make the pipeline
work also on material from other video cameras.


**Installation and workflow:**


This folder contains necessary files to cut together a "supercut" from GoPro videos according to motion/colour on a Windows computer. 
You are required to build CutVideoByCondition.exe and **either** MovementIntoFrame.exe **or** CheckMotion.exe on your own computer to make this pipeline work.
MovementIntoFrame.exe relies on motion detection, CheckMotion.exe relies on colour detection. CheckMotion.exe was **not** used for this manuscript (we provide our code here, if you are interested, but this code is **not** necessary to reproduce our pipeline used for the manuscript).
For creating MovementIntoFrame.exe and CheckMotion.exe, please check out the respective .md files in their respective folders inside the Frame_Detection_OpenCV directory.
For detailed information, read comments in each script/program!
Mind that MovementIntoFrame.exe and CheckMotion.exe can also be run as stand-alone programs. Please check out the above mentioned .md files for more on that.

Create a main folder for your analyses (can be named however you want). Inside this folder, create a folder called cut_video_by_condition. If you call it 
differently, the program will change the name to cut_video_by_condition automatically.
Copy all files provided in this repository into that cut_video_by_condition folder, except CutVideoByCondition.cpp. 
This file you will use to build the C++ program CutVideoByCondition.exe by compiling CutVideoByCondition.cpp. 
Follow exactly the same pipeline as discribed in 
Frame_Detection_OpenCV/MovementIntoFrame/MovementIntoFrame/How_to_build_and_use_MovementIntoFrame.md. 
**Only difference**: You additionally need to include "videoio" as OpenCV library in Eclipse.
Copy the exe you built into the cut_video_by_condition folder.

Furthermore, build the two C++/OpenCV programs provided in Frame_Detection_OpenCV in your IDS and copy the executables into the cut_video_by_condition folder
(**OR** only build one of the two programs, if you only want to use one version of frame detection; for our manuscript, we only used MovementIntoFrame.exe). 
All this is described in the md's in the folders of the respective code.
ATTENTION: these executables have to have the following names: "MovementIntoFrame.exe" and/or "CheckMotion.exe".
Read the installation guide in the OpenCV repository on how to build them.

To make the program run, you need to have ffmpeg and R installed on your machine.
Also, you have to have a h264 codec file in your folder cut_video_by_condition.
I downloaded *ffmpeg-20171111-3af2bf0-win64-static.zip* from https://ffmpeg.zeranoe.com/builds/win64/static/
Unpack with *7zip*.
Follow the installation guide in the following link, but it is easier not to do step 3, unless you want to use 
ffmpeg in the future independently of this program:
http://adaptivesamples.com/how-to-install-ffmpeg-on-windows/
For installation of R, follow any common guide. 
Download the windows codec from here. 
(Attention, you must unpack it, such that it doesn't have the bz2 ending; depending on what version of OpenCV you are using, you have to decide the right version to download. For OpenCVversion 3.4.1, codec version 1.7.0 works. 
If you downloaded the wrong version, the batch script will give an error in red telling you which version you need. To see the error, run the batch script over the console):
https://github.com/cisco/openh264/releases

Inside your folder cut_video_by_condition, double-click the "SETUP" file. Choose which method of detection to use, set parameters for the
program and provide paths to ffmpeg and R.

In your main folder (the parent folder of cut_video_by_condition), a results sheet and a batch file "CUT_VIDEO_BY_CONDITION" will appear.

Inside the main folder, you will create subfolders named after the date, whereas they need to be named in a year_month_day style and with months as numbers
as the computer will then order them descendingly. Possible styles: 2018_05_29 or 18_05_29
If you decided in the setup to have input and output location inside the same folders, you need to create these result folders inside your
date folders, and copy the video material into them. At least one new subfolder in this folder is necessary! It can though be as many subfolders as you want.
For example, if on the the 19th of December, you shot video footage of two different species, create the folder 17_12_19 and inside, create a subfolder for the footage of one 
and another subfolder for the footage of the other species. No more subfolders inside the subfolders! And don't change the video file names!
The program will run independently over them. 
If you decided to extract videos from external folder (e.g. another location on your computer or
a SD card, you don't need to create any subfolders inside the date folder, as the program will do so automatically. 

Now you can move back to the main folder and double-click on CUT_VIDEO_BY_CONDITION.bat. The program will ask you for the
name of the folder in which output will be produced (and where potentially also your raw video files are being saved, if you chose that option). 
In case you select to not trust GoPro system time:
It will ask you furthermore for how many units of video material you recorded that day (how often did you start a recording by clicking on record).
For each unit you will hve to provide the precise time when you started recording. Hit enter and the program will do the rest.

The program will now automatically strip the sound off from all video files. 
It will retrieve video information on all videos. It will feed the videos into the selected frame-detection-OpenCV program
and will transform the output of this program into video-timecode which represents the beginning of each movement/color recognition. According to these codes,
the videos will be pasted together into one video which only contains chunks of movement/color, with a box in which the time of day will be written down, a box with the recording unit and a red progress bar expanding the duration of one chunk.
Finally, the rerun batch script will be copied into each of those subfolders.

If you are not happy with the results from the OpenCV program, you can run the code again separetly over a video
by clicking 000_rerun_opencv_code.bat. You can select new parameter settings. A new supercut will be produced, but the previous one NOT overwritten.


**Files:**

- Folder: ***Frame_Detection_OpenCV:***
Contains all important OpenCV code for frame detection (read details within the folders)

- ***CutVideoByCondition.cpp:***
This is the source code to build CutVideoByCondition.exe.

- ***Extract_video_info1.R:***
R script to extract video information and transform the frames detected by OpenCV to nice "chunks" of frames.

- ***copy_main_program.bat:*** 
Transforms videos into a readable format for the C++ code *MovementIntoFrame.exe* or *CheckMotion.exe* 
which give information on the frames which contain a moving object or a certain colour. Moreover, this batch script collects information about the different videos (recording time and duration), which is then processed together with the frame tables in the R scripts ***Extract_video_info1.R***. 
All this is then fed to the C++ program CutVideoByCondition and the supercut will be created. Will be extracted to your parent directory as CUT_VIDEO_BY_CONDITION.bat.

- ***copy_rerun_opencv_code.bat:***
This batch script is a short version of CUT_VIDEO_BY_CONDITION.bat which can be called after CUT_VIDEO_BY_CONDITION.bat if you are
not satisfied with the supercut. This batch script calls the R script ***Extract_video_info1.R***. This batch script will be extracted to each subfolder which contains video material as 000_rerun_opencv.bat.

- ***SETUP.bat:***
The setup program for the pipeline


Appendix:

**Analyze a video:**
Download *MPCHC player* to navigate through videos. Use CTRL+G to extract current frame ID. To have a better user experience, we recommend that
you customize the hotkeys and the displayed information.
Attention: Other players may not play the produced supercut.
Alternatively, also the Windows Media Player can be used, where the option to go frame by frame can also be activated by right-clicking on the actual video, "Enhancements", "Play Speed Settings".
In Media Player, you have to jump by frame with the mouse, which is probably more comfortable for most people.
The VLC player is another alternative.


To read more about stripping sound off: http://www.bugcodemaster.com/article/transforming-video-file-remove-sound 
and about concatenation: https://trac.ffmpeg.org/wiki/Concatenate
