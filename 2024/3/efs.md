# Mounting EFS

It's [recommended](https://docs.aws.amazon.com/en_us/efs/latest/ug/mounting-fs-mount-helper-ec2-linux.html) to use [Amazon EFS client](https://docs.aws.amazon.com/en_us/efs/latest/ug/installing-amazon-efs-utils.html) to mount [Amazon Elastic File System](https://aws.amazon.com/efs/), but you can of course directly mount it as nfs.

```
sudo mount -t nfs -o nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2,noresvport <efs-id>.efs.<region>.amazonaws.com:/ /mnt
```
