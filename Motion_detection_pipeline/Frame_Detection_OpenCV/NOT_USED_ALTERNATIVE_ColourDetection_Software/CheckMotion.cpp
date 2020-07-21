/**
 * @CheckMotion
 * @Motion detection using color detection and image thresholding
 */

//opencv
#include <opencv2/videoio.hpp>
#include <opencv2/core.hpp>

//C
#include <stdio.h>

//C++
#include <iostream>
#include <sstream>
#include <stdlib.h>


//Necessary to later disable producing crash dialogue on windows. Apparently we have to write this before using namespace cv;
#include <windows.h>

using namespace cv;
using namespace std;


//I read this shall help opening video files.
//#include <opencv/ml.h>

//    Note for program CheckMotion:
//
//    CheckMotion does the following
//    1) It searches each frame in a video and looks for a specified range of colors in the frame
//    2) It prints the number of these frames into the console.
//    3) Finally, it prints the total number of frames and the frames per second into the console.





int main(int argc, char* argv[])
//8 arguments: argv[1]: folder in which execution takes place; argv[2]: filename (e.g. 1.mp4)
//argv[3-8] are the BGR specifications. argv[0] is the program's name.
{
     //Disable error windows popping open.
     SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);

     //Read-in command-line arguments

	 //Directory to access
     char* directory=argv[1];
     char* file=argv[2];
     char* filelocation = strcat(directory,file);

     //Get user specified GBR values. Transform command-line arguments to integers with atoi

     int B_min;
     int G_min;
     int R_min;
     int B_max;
     int G_max;
     int R_max;

     B_min = atoi(argv[3]);
     G_min = atoi(argv[4]);
     R_min = atoi(argv[5]);
     B_max = atoi(argv[6]);
     G_max = atoi(argv[7]);
     R_max = atoi(argv[8]);


     //Read-in video
VideoCapture cap(filelocation);

    //Retrieve fps value
    double fps;
    fps = cap.get(CAP_PROP_FPS);

	//Retrieve total number of frames in video
	int total_frame_number;
	total_frame_number = cap.get(CAP_PROP_FRAME_COUNT);

    if(cap.isOpened()) {

        //Define a stringstram for storing frame numbers for those frames with ss
        //Has to be outside of the for loop so it gets updated
        int ss;

        //Create an object denoting the frames
        Mat frame;

        //For loop looking through frames
        for(;;)
        {

            cap >> frame;

            ///Part II: Image thresholding and image saving

            //Create an object representing new images after thresholding
            Mat masked;

            //inRange function that convert the pixels that fall within the specified range to white and everything else to black
            //The Range is specified by a lower [Scalar(B_min,G_min,R_min)] and an upper [Scalar(B_max,G_max,R_max)] threshold
            //In openCV, a color is defined by its BGR score
            //The thresholded images will then be represented by the object "masked"
            inRange(frame, Scalar(B_min,G_min,R_min), Scalar(B_max,G_max,R_max), masked);

            //Creating integer variables for total pixel count and white pixel count for each frame
            int totalpixel;
            int whitepixel;

            //Total pixel count equals the number of rows and columns of the frame
            totalpixel = frame.rows*frame.cols;
            //Using countNonZero function to count the number of white pixels
            whitepixel = countNonZero(masked);
            //Output frame number, total pixel count and white pixel count for each frame

            //Exit the loop when reaching the last frame (i.e. pixel count drops to 0)
            //Output total number of frames and frames per second.
            if(totalpixel==0){
                cap.release();
		cout << total_frame_number << endl;
                cout << fps << endl;
		cout << "done" << endl;
		cout.flush();
                break;
            }

            else {
                if (whitepixel > 0){

                    //Retrieving frame number info from VideoCapture
                    ss = cap.get(CAP_PROP_POS_FRAMES);

                    cout
                    << ss << endl;
                }
            }
        }
    }
    return (0);
}
