# Migrating an ECS service from EC2 container instances to Fargate

I've recently migrated an ECS service from ECS container instances to Fargate.

I used EC2 instances for some reasons. At first, there wasn't Fargate when I started using ECS. Even when they introduced Fargate, I needed EC2 container instances to mount EFS in containers. There are some other reasons including its pricing.

But it comes to auto scaling, using Fargate is much easier than using EC2 container instances because I no longer need to think about scaling EC2 instances, but can focus on scaling ECS tasks.

So what I needed to move to Fargate when I migrated an ECS service associated with ALB?

First, I needed to recreate an ALB target group. When you associate an ECS service running on Fargate with an ALB target group, the type of the target group has to be `ip`, not `instance` which is the default.

```
aws --region $REGION elbv2 create-target-group \
  --name $TARGET_GROUP_NAME \
  --protocol HTTP \
  --port $PORT \
  --vpc-id $VPC_ID \
  --target-type ip \
  --health-check-protocol HTTP \
  --health-check-path /
```

Second, I needed to update a task definition to make it use `awsvpc` network mode. Also, I needed to specify `executionRoleArn`, `cpu` and `memory`. You also need to change `hostPort` in `portMappings` to use the same port as `containerPort`. It should have been `0` when you used EC2 container instances. In addition to that, I needed to remove `systemControls` from container definitions in the task.

Lastly, set launch type of the service to `FARGATE`, and specify network configurations. You may need to remove placement strategy if you used it.

```
aws --region $REGION ecs create-service \
  --cluster $CLUSTER_NAME \
  --service-name $SERVICE_NAME \
  --task-definition $TASK_NAME \
  --desired-count 2 \
  --launch-type FARGATE \
  --network-configuration "awsvpcConfiguration={subnets=[$SUBNET1_ID,$SUBNET2_ID],securityGroups=[$SECURITY_GROUP_ID],assignPublicIp=DISABLED}" \
  --deployment-configuration maximumPercent=200,minimumHealthyPercent=100
  --load-balancers targetGroupArn=$TARGET_GROUP_ARN,containerName=$CONTAINER_NAME,containerPort=$PORT
```

You can set `FARGATE` capacity provider as a service's capacity provider (or even set it as a default capacity provider of your cluster) instead of specifying a launch type.
