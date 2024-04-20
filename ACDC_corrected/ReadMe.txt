* use zstack images with changes color layers (switched green and red channels)

First, use the training set (one image out of each group, 7 images in total)
1. set length kernel: 200
2. import images (image sequence -> all images)
3. start analysis
4. tune parameters -> Parameter combination: SNR Threshold: 1.4, Min cilia length: 0, Directional threshold: 0.3, standard deviation threshold: 0
5. Mark all found structures as FALSE that I would not have found and add missed cilia

-> establish ground truth
best combination: SNR Threshold: 1.73, Min cilia length: 0, Directional threshold: 0.41, standard deviation threshold: 0
(precision: 0.804, recall: 0.650, F1 Score 0.719) (used parameter combination with highest precision)

Next, use the entire image set
1. set length kernel: 200
2. import images (image sequence -> all images)
3. start analysis with parameters values shown above.
5. For No Correction: Go through all images without changing anything. (no manual TRUE/FALSE detection)
5. For Correction: Go through all images and mainly add missed cilia (manual TRUE/FALSE detection)

ATTENTION: 190815_EV38_2_Collagen_ITSwithAsc+Dexa_63x_zstack_3_projection.tif could not be analyzed with kernel length 200 but only with length 125 (there was an error during skeletonization).
We've seperately analyzed this image.