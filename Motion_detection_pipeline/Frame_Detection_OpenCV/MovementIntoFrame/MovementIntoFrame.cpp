/**
 * @MovementIntoFrame
 * @Motion detection in video footage using surveillance camera logic
 */

//Code written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~December 2017
//Code incorporates several modified fragments of Kyle Hounslow's DIYsc.cpp code which can be retrieved here:
// https://www.dropbox.com/s/mzqk0eg8u82rgef/surveillanceCam.cpp?dl=0
//For issues of copyright, Mr. Hounslow asks to copy the following text from his depository:
// "
//DIYsc.cpp
//Written by  Kyle Hounslow, March 2014
//"Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
//to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
//and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
//The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// "
//A lot of fragments in this code also come from CheckMotion_Clean_and_Final.cpp, written by Chi-Yun Kuo and Alexander Hausmann
#include <opencv/cv.hpp>

//C++
#include <iostream>
#include <sstream>
#include <stdlib.h>

//#include <chrono>
//#include <thread>

//Necessary to later disable producing crash dialogue on windows
#include <windows.h>

//I read this shall help opening video files.
#include <opencv/ml.h>

using namespace cv;
using namespace std;

//    Note for program MovementIntoFrame:
//
//    MovementIntoFrame does the following.
//    1) It goes through input video file frame by frame
//    2) If selected (i.e. if parameters 5-8 are not all set to 0.5):
//       It ignores center of video and makes the outer area of each frame the region of interest,
//       such that objects in the center can move without being noticed
//    3) It compares frame of previous iteration to current frame and notices change in pixels
//       which it interprets as motion. Thresholds for what to be called a motion are set by
//       applying a user-specified blur and sensitivity.
//    4) If motion is detected, number of current frame is printed into console.
//    5) Finally, the total number of frames, the frame rate and a "done" is printed into the console.

//This function is the actual motion detection function. It uses the given threshold image.
bool detectMotion(Mat thresholdImage) {

	//Initialize motionDetected variable.
	bool motionDetected = false;

	//Search for white pixels. If they are detected, return true
	int whitepix = countNonZero(thresholdImage);
	if (whitepix>0) {
		motionDetected = true;
	}

	return motionDetected;

}

//This is the main function calling the detectMotion function. parameters are provided via command line by the user.
int main(int argc, char* argv[])
//9 arguments: argv[1]: folder in which execution takes place; argv[2]: filename (e.g. 1.mp4)
//argv[3]: sensitivity (the higher the more sensitive); argv[4]: level of blurriness (the higher the blurrier);
//argv[5-8]: the thickness of the ROI frame (region of interest) relative to width and height.
//argv[5]: refers to the bottom side. If set to e.g. 0.25, then 25%*height of frame are of interest starting from bottom.
//argv[6]: refers to the left side. If set to e.g. 0.25, then 25%*width of frame are of interest starting from left side.
//argv[7]: refers to top side. If you are interested in the top 25%, then also set this to 0.25 and not 0.75.
//argv[8]: refers to right side. Same applies as to argv[7].
//argv[9]: if y, then the user gets the first frame with RONI square pasted into the folder with the video.
//argv[0] is, by the way, the program's name.

		{

	//Disable error windows popping open.
	SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);

	//Read-in command-line arguments

	//Get file directory
	char* directory = argv[1];
	char* file = argv[2];
	char* filelocation = strcat(directory, file);

	//Get user specified sensitivity, blur and RONI (region of no interest) values. Transform command-line arguments to integers with atoi

	//The sensitivity value given by the user over the command line. Necessary for absdiff() function
	//The lower value, the higher the sensitivity
	int SENSITIVITY;
	//Size of blur used to smooth the intensity image output from absdiff() function
	int BLUR_SIZE;
	//The thickness of the "frame"/outline relative to width and height
	float RONI_size_bottom;
	float RONI_size_left;
	float RONI_size_top;
	float RONI_size_right;

	//Transfer command line arguments to integers/floating-point numbers.
	SENSITIVITY = atoi(argv[3]);
	BLUR_SIZE = atoi(argv[4]);

	RONI_size_bottom = atof(argv[5]);
	RONI_size_left = atof(argv[6]);
	RONI_size_top = atof(argv[7]);
	RONI_size_right = atof(argv[8]);

	//The decision whether to get the first frame with RONI pasted into folder
	string RONI_example = argv[9];

	//If motion is detected in the video feed, this will change to true.
	bool motionDetected = false;

	//Read-in video
	VideoCapture cap(filelocation);
//	cap.set(CV_CAP_PROP_FOURCC, CV_FOURCC('H', '2', '6', '4'));

	//This checks if the video could be read in.
	if (!cap.isOpened()) {
		cout << "ERROR READING VIDEO" << endl;
		getchar();
		return -1;
	}

	if (cap.isOpened()) {

		//set up the matrices that will be of need
		//The region that will later be ignored (mask) and the frame read in each iteration
		Mat mask, read_in;
		//Matrices their grayscale images (needed for absdiff() function)
		Mat grayImage1, grayImage2;
		//resulting difference image
		Mat differenceImage;
		//thresholded difference image (for use in findContours() function)
		Mat thresholdImage;

		//Retrieve fps value
		double fps;
		fps = cap.get(CV_CAP_PROP_FPS);

		//Retrieve total number of frames in video
		int total_frame_number;
		total_frame_number = cap.get(CAP_PROP_FRAME_COUNT);

		//Retrieve dimensions of video
		int width = cap.get(CV_CAP_PROP_FRAME_WIDTH);
		int height = cap.get(CV_CAP_PROP_FRAME_HEIGHT);

		//Define the RONI (region of no interest), that will later be ignored in each frame
		Rect RONI(int(RONI_size_left * width), int(RONI_size_top * height),
				int((1 - (RONI_size_left + RONI_size_right)) * width),
				int((1 - (RONI_size_bottom + RONI_size_top)) * height));

		//Retrieve the number of frames per second
		int ss;

		//Initialize a loop-counter (of importance when it comes to exiting the program)
		int loop_counter = 0;

		//For loop looking through frames
		for (;;) {

			//Add 1 to the loop counter
			loop_counter = loop_counter + 1;

			//If loop-counter equals frame-rate, output number of frames and fps and exit program.
			//Without the exit here, the program would crash, as the last frame is always
			//an empty frame on which the program cannot operate. Its frame number is though the same
			//as of the actual last frame. This the program can exit on the actual last frame due to this.
			if (loop_counter == total_frame_number) {

				cout << total_frame_number << endl;
				cout << fps << endl;
				cout << "done" << endl;
				cout.flush();
				return (0);

			} else {

				//Read in current frame
				cap >> read_in;

				//If we are in the first frame, there is no previous frame to compare this one to.
				//I.e. there is no motion to be detected.
				if (loop_counter == 1) {

					//Read in current frame
					cap >> read_in;

					//Convert the current frame to gray scale for frame differencing. This can
					//be then used in the next iteration to compare the active frame to.
					//This is why it is directly saved under grayImage1, and not grayImage2.
					cvtColor(read_in, grayImage1, COLOR_BGR2GRAY);

					//Make first the whole frame as a region of interest. This has to have the
					//same size as grayImage and is also of grayscale picture type.
					//We set this matrix to 255 and the mask later on to 0, resulting in the center
					//being ignored and the outside not being ignored. Usually, areas of interest work the opposite way around.
					Mat mask(grayImage1.size(), CV_8U, Scalar::all(255));
					//Put the RONI to black.
					mask(RONI).setTo(Scalar::all(0));

					//Copy the RONI onto the grayscaled frame.
					bitwise_and(grayImage1, mask, grayImage1);

					//If user selected yes, first frame is safed with RONI in folder where video came from.
					if (RONI_example == "y") {

						stringstream filename;
						filename << filelocation << "_frame1_excluded_area.png";
						rectangle(read_in, RONI, Scalar(255), 3, 8, 0);
						imwrite(filename.str(), read_in);

					}

					//In any other frame, this is executed. Code that repeats from before will not be commented.
				} else {

					cvtColor(read_in, grayImage2, COLOR_BGR2GRAY);

					//This needs to get repeated here even though it should actually be the same as
					//in iteration 1. The program otherwise crashes.
					Mat mask(grayImage2.size(), CV_8U, Scalar::all(255));
					mask(RONI).setTo(Scalar::all(0));

					bitwise_and(grayImage2, mask, grayImage2);

					//Perform frame differencing with the sequential images. This will output an "intensity image"
					absdiff(grayImage1, grayImage2, differenceImage);

					//Threshold intensity image at the given sensitivity value
					threshold(differenceImage, thresholdImage, SENSITIVITY, 255,
							THRESH_BINARY);

					//Blur the image to get rid of the noise. This will output an intensity image
					blur(thresholdImage, thresholdImage,
							Size(BLUR_SIZE, BLUR_SIZE));

					//Threshold again to obtain binary image from blur output
					threshold(thresholdImage, thresholdImage, SENSITIVITY, 255,
							THRESH_BINARY);

					//Execute detectMotion function on threshold image.
					motionDetected = detectMotion(thresholdImage);

					//if motion detected, print the current frame number
					if (motionDetected) {

						//Create a string for frame number that gets updated for each cycle of the loop
						ss = cap.get(CAP_PROP_POS_FRAMES);

						//Print the number
						cout << ss << endl;

					}
					//Copy grayImage2 on grayImage1 to compare this in the next iteration to the then current frame.
					grayImage2.copyTo(grayImage1);
				}
			}
		}

	}

	//If exiting before should not have worked before, exit here.
	return (0);

}
