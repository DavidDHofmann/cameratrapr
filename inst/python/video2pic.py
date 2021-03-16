# Load required libraries
import sys
import argparse
import cv2
import math
import os
import numpy as np

# Function to extract desired frames
def video2pic_py(pathIn, pathOut, fps):

    # Identify original file name
    filename = os.path.basename(pathIn)
    filename = os.path.splitext(filename)[0]

    # Start video
    vidcap = cv2.VideoCapture(pathIn)

    # Read images
    success, image = vidcap.read()
    count = 0
    success = True

    # Extract frames
    while success:
        vidcap.set(cv2.CAP_PROP_POS_MSEC, (count * 1000 * fps))
        success, image = vidcap.read()
        image_last = cv2.imread(pathOut + "/" + filename + "_{}.png".format(count - 1))
        if np.array_equal(image,image_last):
            break
        print ("Read a new frame: ", success)
        cv2.imwrite(pathOut + "/" + filename + "_Frame_%d.JPG" % count, image)
        count = count + 1

# # Function call
# if __name__ == "__main__":
#     print("aba")
#     a = argparse.ArgumentParser()
#     a.add_argument("--pathIn", help = "path to video")
#     a.add_argument("--pathOut", help = "path to images")
#     a.add_argument("--fps", help = "frames per second", type = int, default = 1)
#     args = a.parse_args()
#     print(args)
#     extractImages(args.pathIn, args.pathOut, args.fps)
