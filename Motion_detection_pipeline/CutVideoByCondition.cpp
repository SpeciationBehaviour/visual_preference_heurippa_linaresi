/**
 * @CutVideoByCondition
 * @Produce supercut of a set of mp4 videos following info in tables in same folder as videos,
 * @named frame_x.txt, time_x.txt, bar_x.txt and unit_x.txt. Whereas x is the name of the respective video.
 * @List of video titles (including file ending) is provided as function argument.
 */

//Code written by Alexander Hausmann (alexander_hausmann@gmx.net) in ~May 2018

//    Note for program CutVideoByCondition:
//
//    CutVideoByCondition does the following.
//    1) It goes step by step through a list of video titles from txt, given as argument.
//    2) In each step, it reads the respective frame_, time_, bar_ and unit_ table in.
//    3) It reads the respective video in and goes through it frame by frame.
//    4) If the current frame number is found in frame_ table, it adds it to a video output file,
//		 specific for the current read-in video. Also, it plots on the frame the time of day from time_ table,
//		 the video recording unit of day from unit_ and a red bar according to info in bar_ table.
//    5) It outputs the final supercut video for the respective read-in video.



//Libraries

#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <sstream>
#include <vector>
#include <algorithm>
//#include <chrono>
//#include <thread>

#include <opencv/cv.hpp>
#include <opencv2/opencv.hpp>
#include <opencv/highgui.h>

//Necessary to later disable producing crash dialogue on windows
#include <windows.h>

//I read this shall help opening video files.
#include <opencv/ml.h>

// First define functions for reading in frame and timestamp table.
// Adapted from here : http://thispointer.com/c-how-to-read-a-file-line-by-line-into-a-vector/
// First the function to read in a table of strings (for the timestamps).
bool getFileContentStr(std::string fileName,
		std::vector<std::string> & vecOfStrs) {

	// Open the Video
	std::ifstream in(fileName.c_str());

	// Exit if video cannot be opened
	if (!in) {
		std::cerr << "Cannot open the timestamp file : " << fileName
				<< std::endl;
		return false;
	}

	std::string str;
	// Store line by line on vector
	while (std::getline(in, str)) {
		if (str.size() > 0)
			vecOfStrs.push_back(str);
	}
	//Close The File
	in.close();
	return true;
}

// This function reads in a table of integers (the frame numbers).
bool getFileContentInt(std::string fileName, std::vector<int> & vecOfInts) {

	// Open the Video
	std::ifstream in(fileName.c_str());

	// Exit if video cannot be opened
	if (!in) {
		std::cerr << "Cannot open the frame file : " << fileName << std::endl;
		return false;
	}

	std::string str;
	int content;

	// Store line by line on vector
	while (std::getline(in, str)) {
		if (str.size() > 0) {
			std::stringstream parse(str);
			parse >> content;
			vecOfInts.push_back(content);
		}
	}
	//Close The File
	in.close();
	return true;
}

// This function reads in a table of integers (the frame numbers).
bool getFileContentDoub(std::string fileName,
		std::vector<double> & vecOfDoubs) {

	// Open the Video
	std::ifstream in(fileName.c_str());

	// Exit if video cannot be opened
	if (!in) {
		std::cerr << "Cannot open the frame file : " << fileName << std::endl;
		return false;
	}

	std::string str;
	double content;

	// Store line by line on vector
	while (std::getline(in, str)) {
		if (str.size() > 0) {
			std::stringstream parse(str);
			parse >> content;
			vecOfDoubs.push_back(content);
		}
	}
	//Close The File
	in.close();
	return true;
}

// This is the main function
int main(int argc, char* argv[]) {

	//Disable error windows popping open.
	SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);

	using namespace std;
//	using namespace std::this_thread;
//	using namespace std::chrono;

	using namespace cv;

	// Later, when we go line by line through table, we need to define this first.
	string line;

	// Get arguments from command line
	char* filelist = argv[1];
	char* main_dir = argv[2];
	char* daily_date = argv[3];
	char* species = argv[4];
	float height = atof(argv[5]);
	float width = atof(argv[6]);
	float fps = atof(argv[7]);
	float font_size = atof(argv[8]);

	//Read in as string.
	string f1(argv[9]);
	string f2(argv[10]);
	string f3(argv[11]);
	string f4(argv[12]);

	char fourcc1 = f1[0];
	char fourcc2 = f2[0];
	char fourcc3 = f3[0];
	char fourcc4 = f4[0];
//
//	//transform string to char
//	char fourcc1[4];
//	for(int i=0;i<4;i++)
//	        fourcc1[i]=f1[i];
//	char fourcc2[4];
//	for(int i=0;i<4;i++)
//	        fourcc2[i]=f2[i];
//	char fourcc3[4];
//	for(int i=0;i<4;i++)
//	        fourcc3[i]=f3[i];
//	char fourcc4[4];
//	for(int i=0;i<4;i++)
//	        fourcc4[i]=f4[i];
//
	//Define dimensions of rectangle and timecode in it.
	float rect_top = height - (0.05 * height);
	float rect_right = 0.11 * width;
	float text_pos = height - (0.01 * height);

	float unit_rect_right = rect_right + 0.15 * rect_right;

	// Read in the list of video files.
	ifstream all_files(filelist);

	// If we couldn't open the output file stream for reading
	if (!all_files) {
		// Print an error and exit
		cerr << "Argument table could not be read in!" << endl;
		exit(1);
	}

	// This is to get the filename of the output.
	string supercut_date = "\\supercut_";
	supercut_date += daily_date;

	// Go line by line through file names.
	while (std::getline(all_files, line)) {

		// These are the files we need to read in.
		string timechar;
		string framechar;
		string barchar;
		string unitchar;
		timechar = "time_" + line + ".txt";
		framechar = "frame_" + line + ".txt";
		barchar = "bar_" + line + ".txt";
		unitchar = "unit_" + line + ".txt";

		// Read in bar table.
		std::vector<double> bar_array;
		getFileContentDoub(barchar, bar_array);

		// Read in unit table.
		std::vector<std::string> unit_array;
		getFileContentStr(unitchar, unit_array);

		// Read in frame number table.
		std::vector<int> frame_array;
		getFileContentInt(framechar, frame_array);

		// Read in timestamp table..
		std::vector<std::string> time_array;
		getFileContentStr(timechar, time_array);

		// Determine the length of one of the tables (they have the same length)
		//int length = frame_array.size();

		//Read-in video title
		string video_name;
		video_name = main_dir;
		video_name += "\\";
		video_name += line;

		// Out video title
		string outfile;
		outfile = main_dir;
		outfile += "\\";
		outfile += supercut_date;
		outfile += "_";
		outfile += species;
		outfile += "_";
		outfile += line;
		outfile += "_chunk.mp4";

		// This creates the new video file
		VideoWriter out_capture(outfile, CV_FOURCC(fourcc1, fourcc2, fourcc3, fourcc4), fps,
				Size(width, height));

		VideoCapture cap(video_name);

		//This checks if the video could be read in.
		if (!cap.isOpened()) {
			cout << "ERROR READING VIDEO " << line << "\n";
			getchar();
			return -1;
		}

		if (cap.isOpened()) {

			// This defines the frame of each loop.
			Mat read_in;

			//Initialize a loop-counter (needed to see if the current frame
			//is among the frames in the frame nmber table).
			int loop_counter = 1;

			//This will be used to find out if the current frame is member of the frame number table.
			int frame_counter = 0;

			//For loop looking through frames
			for (;;) {

				//Read in current frame
				cap >> read_in;

				if (read_in.empty())
					break;

				//If frame is equal to frame counter.
				if (loop_counter == frame_array[frame_counter]) {
					//Put white rectangle for unit number.
					rectangle(read_in, Point(rect_right, rect_top),
							Point(unit_rect_right, height), Scalar(255, 0, 0),
							-1);
					//Put matching unit
					putText(read_in, unit_array[frame_counter],
							Point(rect_right, text_pos), 1, font_size,
							Scalar(255, 255, 255), 2);
					//Put white rectangle for timestamp.
					rectangle(read_in, Point(0, rect_top),
							Point(rect_right, height), Scalar(255, 255, 255),
							-1);
					//Put bar rectangle.
					rectangle(read_in,
							Point(0, rect_top - 0.15 * (height - rect_top)),
							Point(bar_array[frame_counter] * rect_right,
									rect_top), Scalar(0, 0, 255), -1);
					//Put matching timestamp
					putText(read_in, time_array[frame_counter],
							Point(0, text_pos), 1, font_size, Scalar(0, 0, 0),
							2);
					//Append to video
					out_capture.write(read_in);
					//update frame_counter.
					frame_counter += 1;

				}

				//Add 1 to the loop counter
				loop_counter = loop_counter + 1;

			}
		}

		//Output the name of the file that was just finished.
		cout << line << endl;
		cout.flush();
		//Release the video
//		cap.release();

		//Let system sleep for 5 seconds
//	    sleep_for(seconds(5));
	}

	return 0;
}
