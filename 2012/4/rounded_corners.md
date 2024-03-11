# Round corners of an image with ImageMagick

You can use this commands to make an image have round corners with ImageMagick's convert.

    convert -size 512x512 xc:none -draw "roundrectangle 0,0,512,512,40,40" png:- | convert src.png -matte - -compose DstIn -composite dst.png

Of course, you have to adjust its width, height and round size.
