CheckMotion is an OpenCV program written in C++. It was written to detect butterflies in a frame by identifying the blue acrylic 
paint on the thorax of the butterflies. We have decided to use a different approach (MovementIntoFrame) to detect butterflies but still save the codes and 
instructions here for any future reference and use. This folder contains the same logic of documents as the folder MovementIntoFrame.


** This is a step-by-step guide to running CheckMotion, an OpenCV code writen in C++ **

**Step 1: Installing and configuring OpenCV on Windows**


This section was written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~November 2017 (and edited afterwards a couple of times)
This describes steps that we recommend to be taken to build *CheckMotion.exe* on a Windows machine.

**1a) Install softwares necessary to build CheckMotion**

Download the *Eclipse* IDS, the compiler *MinGW*, *OpenCV*, and *Cmake* to get the C++ running. Also make sure that you have Java installed (not sure whether Runtime Environment or Developer's Kit, I had both). 

Please download Eclipse (e.g. Oxygen, CDT), MinGW, OpenCV and Cmake (under latest release) from here: 

https://www.eclipse.org/downloads/download.php?file=/oomph/epp/oxygen/R/eclipse-inst-win64.exe 

https://sourceforge.net/projects/mingw-w64/

https://opencv.org/releases.html 

https://cmake.org/download/

Follow the instructions in this video until (and including) the use of CMake-GUI (but find more on CMake under b), because this is one of the trickiest steps): 
https://www.youtube.com/watch?v=BBamKg5_JKI 

Mind: to get Cmake working, put the following as path variable (and adapt directory to your PC directory): 
C:\Program Files\CMake\bin

The person in the video forgets to mention two things: 

1) After you successfully unpacked your opencv, you have to give the path to your opencv bin as path variable to your system, just as you have to do it with the bin of MinGW. Put it e.g. to C:\opencv_src\build\bin 

The path to MinGW can look like this:

C:\Program Files\mingw-w64\x86_64-7.2.0-posix-seh-rt_v5-rev1\mingw64\bin

You can read more about the setting of the path variable here:

https://www.ics.uci.edu/~pattis/common/handouts/mingweclipse/mingw.html

But attention, this link is sort of outdated. For MinGW, it is enough to just give the build\bin.

2) Furthermore, after you configure with Cmake-Gui, but before you generate with Cmake-GUI, there will be lots of red notifications appearing in the upper half of Cmake. Depending on your computer, you may have to go to the ENABLE area and **un**check ENABLE_PRECOMPILED_HEADERS, as well as go to CPU_DISPATCH and select **no** (empty) setting!
We faced different problems on different machines. Once you get an error in the Cmake command line step, just copy your error to the internet and see what people suggest. There may be more specific changes to be done in the Cmake configuration on your computer (finding and changing some settings may require checking "Advanced")!


**1b) Build OpenCV and build CheckMotion**

For building opencv, follow the instructions on this website (the later link is specifically on the recent OpenCV versions):

http://jeanvitor.com/installing-cpp-opencv-3-2/
http://jeanvitor.com/cpp-opencv-windonws10-installing/

**Don't forget** putting the following as path variable. The path must end on **bin** and not **lib**, which could be a common mistake leading to error at execution of program (building though would work just fine).
C:\opencv_src\Mingw_build\install\x64\mingw\bin 

Further path variables that were being added on the machine this was tested on are the following (They all are probably not necessary though and paths need to be changed to the paths used on the machine the user is working on): 
C:\opencv_src\build\bin 
C:\opencv_src\Mingw_build\3rdparty\ffmpeg

In Eclipse, create new C++ project called CheckMotion with *Hello world!* code and set MinGW as toolchain. 
Delete existing code and paste CheckMotion.cpp code in. Declare OpenCV libraries as Eclipse libraries. Follow this link: 

http://jeanvitor.com/installing-cpp-opencv-3-2/ 

Also check out the screenshots of the things to change in the Eclipse properties under screenshots_properties_CheckMotion.pdf.

Built the project! You have now an .exe file in the folder where you saved the project.

If everything works well, you can also build the project as release. Under Project - Build Configurations - Set Active.
Building requires again changing the property settings.

**1c) Appendix:** 

Using OpenCV in Eclipse: 

https://docs.opencv.org/2.4/doc/tutorials/introduction/linux_eclipse/linux_eclipse.html



** Step 2: Running CheckMotion as a stand alone program (on Windows)**

This section was written by Chi-Yun Kuo and Alexander Hausmann (alexander_hausmann@gmx.net) in ~November 2017 (and edited afterwards a couple of times)


Steps for running the program:
Navigate to where CheckMotion.exe is in Terminal. To do so, open Terminal and then type cd\folder1\folder2\...

Once in the correct directory, type CheckMotion.exe [arg1] [arg2] [arg3] [arg4] [arg5] [arg6] [arg7] [arg8] (**WITHOUT** the brackets)

For arg1, provide the path to the folder where the video file is located at  
For arg2, provide the video file name.  
For arg3, provide the minimum value of blue in RGB code.
For arg4, provide the minimum value of green in RGB code.
For arg5, provide the minimum value of red in RGB code.
For arg6, provide the maximum value of blue in RGB code.
For arg7, provide the maximum value of green in RGB code.
For arg8, provide the maximum value of red in RGB code.

As an example: CheckMotion.exe C:/Users/melro/Desktop/Preference_Mel_Cyd/171208/Melpomene 3.mp4 10 20 30 100 150 200
Additional output information can be added. See comments in CheckMotion.cpp for details.
