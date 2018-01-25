# analytics_project
Repository housing few code samples from the projects, worked upon previously for skill development &amp; academic purposes 

Here are the details of the sample codes provided:
1) Bad Loans prediction.R: Loan Defaulter Prediction Model: Exploratory and Predictive analysis done to predict probability of loan default for a consumer financial company based on the past trends. Graphs plotted for Business Presentation using Tableau.

2) car_assignment.R: Car Pricing Prediction Model: Developed model to predict the car’s price for a new entrant in an existing competitive market, using linear regression, selecting the best model through AIC, adjusted r-square, vif, and p-values (variance <=5%).

3) casestudy.R: EDA analysis to decide on the factors that affect funding

4) HR Attrition Case Study.R: HR Attrition Model: Developed a model to determine the probability of attrition using a logistic regression with 75% accuracy; confusion matrix; gain & lift charts; AIC. Suggestions provided for the management to retain the best employees.

5) Uber_Case_Study.R: EDA analysis to interpret and visualise the supply-demand gap airport to-and-fro city

6) Tensorflow_Train_PAN Card: Contains Python scripts used for training the mobile net SSD models on PAN card with the help of TensorFlow Object Detection API, which uses Faster R-CNN for training.  
                Following are the description of the Python Files:
                
                  1)Face_recognition_with smartphone_camera_input.py: Useof OpenCV libraries to detect eyes and face, taking input from Samrtphone Camera
                  
                  2)image_recognisition_tensorflow.py: Use of the mobilenet SSD model for image recognisiton, taking input from WebCam
                  
                  3)xml_to_csv_PAN.py: When images were labelled manually using LabelImg, it generated xml files, which was fed in this script to generate a single csv file for both train and test data set.
                  
                  4)generate_tfrecord_PAN.py: Script used to generate tfrecords which was then used using Transfer Learning to train mobilenet SSD with the 'PAN Card' label with a loss of ~1, after tweaking ssd_mobilenet_v1_pets.config. 
                               The output was used to export the trained graph using export_inference_graph.py
