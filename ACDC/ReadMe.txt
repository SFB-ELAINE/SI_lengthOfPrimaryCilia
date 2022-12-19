How I have analyzed the images:
Use the stacked images with switched green and red channels from allImages/ACDC/..._zstack_histogram_equalized.tif

First, use the training set
1. set length kernel: 200
2. import images (image sequence -> all images)
3. start analysis
4. tune parameters -> Parameter combination: SNR Threshold: 1.3, Min cilia length: 4, Directional threshold: 0.01, standard deviation threshold: 0
5. Mark all found structures as FALSE that I have not found (ignore FALSE negatives)

-> establish ground truth
best combination: SNR Threshold: 1.35, Min cilia length: 0, Directional threshold: 0.16, standard deviation threshold: 0

Next, use the entire image set
1. set length kernel: 200
2. import images (image sequence -> all images)
3. start analysis
4. tune parameters -> Parameter combination: SNR Threshold: 1.35, Min cilia length: 4, Directional threshold: 0.16, standard deviation threshold: 0
5. Go through all images without changing anything. (no manual TRUE/FALSE detection)
