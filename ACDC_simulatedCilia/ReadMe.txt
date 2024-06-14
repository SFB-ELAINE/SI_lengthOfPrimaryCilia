* mean projection instead of max projection to reduce noise (-> significantly better results, fewer FALSE POSITIVES, better F1 score)
* use zstack images with changes color layers

How I have analyzed the images:
Use the stacked images with switched green and red channels from cultivationImages/ACDC/..._projection.tif

First, use the training set (one image out of each group, 7 images in total)
1. set length kernel: 200
2. import images (image sequence -> all images)
3. start analysis
4. Set parameters -> Parameter combination: SNR Threshold: 1.4, Min cilia length: 0, Directional threshold: 0.3, standard deviation threshold: 0

Cilia are detected in every image, but the ridge detection is not very good...