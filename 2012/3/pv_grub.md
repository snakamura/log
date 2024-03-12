# How to upgrade a kernel image supporting pv-grub on EC2

Sometimes, Amazon updates kernel images that support pv-grub and we need to update AKI. To find the latest kernel image, you can type:

    ec2-describe-images -o amazon --filter "manifest-location=*pv-grub-hd0*" --region us-west-1

AKI with "hd0" in its name is an image for unpartitioned disk images. AKI with "hd00" in its name is for partitioned disk images. Most disk images are unpartitioned.

Once you've found the latest AKI, modify your instance by typing:

    ec2-modify-instance-attribute --kernel aki-f77e26b2 --region us-west-1 i-xxxxxxxx

Note that aki-f77e26b2 is the current latest AKI in us-west-1 region.
