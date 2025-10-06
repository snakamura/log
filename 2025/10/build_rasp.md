# Building RASP

If you want to generate Blipmap Forecasts by yourself, using [RASP from scratch](https://github.com/sfalmo/rasp-from-scratch) is a good starting point. It allows you to build a docker image to generate forecasts. But there are a few things you need to do to build this docker image. This post explains how you can build the simplest docker image from it on AWS EC2.

## Creating an EC2 instance

First, let's create an EC2 instance. I created `m7i.xlarge` image using the latest Ubuntu 24.04 LTS AMI. I recommend you attach an EBS larger than 200GB to it. Once you've created an instance, login to it and run

```
sudo apt-get update
sudo apt-get upgrade
```

to update everything.

## Installing docker

Next, let's install docker.

```
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "${UBUNTU_CODENAME:-$VERSION_CODENAME}") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
sudo usermod -aG docker ubuntu
```

Once you've done this, reboot your instance.

## Building docker images

You're now ready to build docker images. Clone the repo and build `base`, `wrf_build` and `wrf_prod` images.

```
git clone https://github.com/sfalmo/rasp-from-scratch.git
cd rasp-from-scratch
cp .env.template .env
```

You can optionally update `.env` to use newer versions of WRF and WPS.

```
--- a/.env       2025-09-28 08:25:48.211582952 +0000
+++ b/.env        2025-10-06 12:56:02.602479862 +0000
@@ -7,8 +7,8 @@
 RASP_IMAGE=your.docker.repo/aufwinde/rasp/rasp

 # Versions of WRF and WPS that shall be used. Those must be valid tags in the corresponding GitHub repo
-WRF_VERSION=v4.5
-WPS_VERSION=v4.5
+WRF_VERSION=v4.7.1
+WPS_VERSION=v4.6.0

 # Choose the CPU architecture for the machine you are currently using for the build (native should be fine)
 WRF_MARCH_BUILD=native
```

Once you've updated (or not updated) `.env`, you're ready to build these three images.

```
docker compose build base
docker compose build wrf_build
docker compose build wrf_prod
```

Before building `rasp` image, we need to prepare geographical data. The repo is configured to use some high resolution data, but we'll use the simplest ones we can download from [WPS V4 Geographical Static Data Downloads Page](https://www2.mmm.ucar.edu/wrf/users/download/get_sources_wps_geog.html) by patching `namelist.wps`.

```
diff --git a/rasp/TIR/namelist.wps b/rasp/TIR/namelist.wps
index 4ea4532..ad755ae 100755
--- a/rasp/TIR/namelist.wps
+++ b/rasp/TIR/namelist.wps
@@ -14,7 +14,7 @@
  j_parent_start       = 1,      25,
  e_we                 = 90,     206,
  e_sn                 = 90,     206,
- geog_data_res        = 'gmted2010_30s+corine_usgs_500m+bnu_soil_30s+modis_fpar+modis_lai+2m', 'SRTM+corine_usgs_250m+bnu_soil_30s+modis_fpar+modis_lai+30s',
+ geog_data_res        = 'default', 'default',
  dx                   = 10000,
  dy                   = 10000,
  map_proj             = 'lambert',
```

Also, we need to patch `namelist.input` because these data don't contain lake physics data.

```
diff --git a/rasp/TIR/namelist.input b/rasp/TIR/namelist.input
index 8a98b1e..6c18c47 100755
--- a/rasp/TIR/namelist.input
+++ b/rasp/TIR/namelist.input
@@ -84,7 +84,7 @@
  sf_sfclay_physics                   = 1,     1,     1,
  sf_surface_physics                  = 2,     2,     2,
  sf_urban_physics                    = 0,     0,     0,
- sf_lake_physics                     = 1,     1,     1,
+ sf_lake_physics                     = 0,     0,     0,
  bl_pbl_physics                      = 1,     1,     1,
  bldt                                = 0,     0,     0,
  topo_wind                           = 1,     1,     1,
```

Let's prepare our geographic data file. We'll download some archives and put them together into `geog.tar.gz`.

```
curl -o geog_high_res_mandatory.tar.gz https://www2.mmm.ucar.edu/wrf/src/wps_files/geog_high_res_mandatory.tar.gz
curl -o landuse_30s_with_lakes.tar.bz2 https://www2.mmm.ucar.edu/wrf/src/wps_files/landuse_30s_with_lakes.tar.bz2
mkdir geog
(cd geog && tar zxf ../geog_high_res_mandatory.tar.gz --strip-components=1)
(cd geog && tar jxf ../landuse_30s_with_lakes.tar.bz2)
tar zcf rasp/geog.tar.gz geog
```

Once you've prepared `geog.tar.gz` and patched these files, you're ready to build `rasp` image.

```
docker compose build rasp
```

You can run this image now to build your forecasts.

```
docker compose run rasp
```

The results will be stored in `results/OUT` directory in your instance. Look at `results/LOG/GM.stderr` to find what was wrong when it failed.

Sometimes, building a docker image doesn't fail even when `geogrid.exe` fails. In this case, you'll find an error in `results/LOG/metgrid.out`. Run `bash` using the image to investigate it further.

```
docker compose run rasp bash
```

You'll find why `geogrid.exe` failed by checking `TIR/geogrid.out` in the container.
