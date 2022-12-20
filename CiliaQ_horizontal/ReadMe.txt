- use the stacked images with switched green and red channels from allImages/ACDC/histogram_equalized/..._zstack_histogram_equalized.tif
- convert the images with ImageJ to RGB stack (Image -> Type -> RGB Stack)

1. Use CiliaQ Preparator accoring to example on GitHub (find cilia)
- parameter valus: all default but: Channel Nr: 2, Segmentation method: Triangle
-> results: for every image, there is a
	- a text file with parameter choice (setting of plugin): "image_name_CQP.txt",
	- a tif file with the segmentation result (as channel 2): "image_name_CQP.tif"

2. Use CiliaQ accoring to example on GitHub (measure cilia)
- parameter values:
	- calibration parameters copied from metadata.csv -> Calibration [µm/px] = 0.219647, Voxel depth [µm] = 0.281399
	- Channels: Reconstruction: 2
	- Minimum cilium size: 10
	- Skeleton analysis: - Gauss XY sigma: 1.000000
-> results:
	- a text file with parameter choice (setting of plugin) and cilia data: "image_name_CQP_CQ.txt",
	- a text file with results of cilia detection in tab-separated format:  "image_name_CQP_CQs.txt",
	- a text file with skeleton information: "image_name_CQP_CQ_SKL_info.txt",
	- a text file with skeleton data: "image_name_CQP_CQI.txt",

	- a tif file with marked cilia: "image_name_CQP_CQ_RP.tif"
	- a tif file with 3D skeletons: "image_name_CQP_CQ_RP_3D.tif"
	- a tif file with skeleton information in z stack layers (direktion and length): "image_name_CQP_CQ_SKL.tif"