# Building WRF on Ubuntu 24.04 LTS

Here is a memo when I built [WRF (Weather Research & Forecasting Model)](https://www.mmm.ucar.edu/models/wrf) on Ubuntu 24.04 LTS on EC2. Basically, I followed [the tutorial](http://www2.mmm.ucar.edu/wrf/OnLineTutorial/compilation_tutorial.php). You should also check the pages starting from [Let's Get Started](https://www2.mmm.ucar.edu/wrf/OnLineTutorial/Introduction/start.php).

First, launch your EC2 instance. I used an x86_64 instance, because it failed to build jasper when I used an arm64 instance. You'd need to update its configuration script to support arm64.

First, install these packages.

```
sudo apt-get install gcc g++ gfortran make csh m4
```

Then, download a snippet to install dependencies and run it. I'd recommend using `-e` flag to make it stop when something goes wrong.

```
wget https://gist.githubusercontent.com/islas/b76171591531d3a06e209cd1ff75840c/raw/15e9000b5092e3eb3413f7fdef40f3962473473a/build_wrf_dependencies.sh
sh -e ./build_wrf_dependencies.sh
```

You should be able to install some of these dependencies using `apt`, but this script picks proper versions and flags.

Next, export these environment variables. You can find them at the beginning of `build_wrf_dependencies.sh`.

```
DIR=$PWD/wrf_dependencies
export NETCDF=$DIR/netcdf
export LD_LIBRARY_PATH=$NETCDF/lib:$DIR/grib2/lib
export PATH=$NETCDF/bin:$DIR/mpich/bin:${PATH}
export JASPERLIB=$DIR/grib2/lib
export JASPERINC=$DIR/grib2/include
```

Now, clone WRF and run `configure`.

```
git clone https://github.com/wrf-model/WRF.git
cd WRF
./configure
```

I chose `34 (GNU (gfortran/gcc) - dmpar)` when it asked `Please select from among the following Linux x86_64 options:`, and chose `1 (basic)` to `Compile for nesting?`.

Once it's configured, build it.

```
./compile em_real > build.log 2>&1
cd ..
```

I made a mistake to run `make` instead of `./compile`, which launched too many `make` processes and ended up eating too much memory. Use the `./compile` script instead.

Once you've built WRF, build WPS.

```
git clone https://github.com/wrf-model/WPS.git
cd WPS
./configure
```

I chose `1 (Linux x86_64, gfortran (serial))` when it asked `Please select from among the following supported platforms.`.

```
./compile > build.log 2>&1
cd ..
```

Download the static geography data once you've built both WRF and WPS. You can find its details at [WPS V4 Geographical Static Data Downloads Page](https://www2.mmm.ucar.edu/wrf/users/download/get_sources_wps_geog.html#mandatory).

```
wget https://www2.mmm.ucar.edu/wrf/src/wps_files/geog_high_res_mandatory.tar.gz
tar zxf geog_high_res_mandatory.tar.gz
```

Then, patch your `namelist.wps`.

```
(cd WPS && patch -p 1 <<END
diff --git a/namelist.wps b/namelist.wps
index 8289caa..a94dd92 100644
--- a/namelist.wps
+++ b/namelist.wps
@@ -22,7 +22,7 @@
  truelat1  =  30.0,
  truelat2  =  60.0,
  stand_lon = -79.0,
- geog_data_path = '/glade/work/wrfhelp/WPS_GEOG/'
+ geog_data_path = '/home/ubuntu/WPS_GEOG/'
 /

 &ungrib
END
)
```

I assumed you were building them at `/home/ubuntu`.

Now, let's build ARWpost. First, download and extract it. Then, run `./configure`.

```
wget https://www2.mmm.ucar.edu/wrf/src/ARWpost_V3.tar.gz
tar zxf ARWpost_V3.tar.gz
cd ARWpost
./configure
```

I chose `3 (PC Linux i486 i586 i686 x86_64, gfortran compiler)` when it asked `Please select from among the following supported platforms.`.

Now, you need to patch `src/Makefile` to link `netcdff` in addition to `netcdf`. Also patch `configure.arwp` to make C pre-processor to omit C-style comments in fortran source files, and pass `-fallow-argument-mismatch` to a fortran compiler.

```
cp src/Makefile src/Makefile.orig
sed -i -e 's/-lnetcdf/-lnetcdff -lnetcdf/g' src/Makefile
cp configure.arwp configure.arwp.orig
sed -i -e 's/-C -P/-P/g' configure.arwp
sed -i -e 's/-frecord-marker=4/-frecord-marker=4 -fallow-argument-mismatch/' configure.arwp
```

Once you have done that, run `./compile` to build it.

```
./compile
```

ARWpost generates files that GrADS reads. So you'll probably want to install GrDAS to render them.

```
sudo apt-get install grads
```
