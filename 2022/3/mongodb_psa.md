# Running a mongodb replica set in two availability zones may not as stable as you might think

When you run a mongodb replica set, you need at least three instances. One of them can be an arbitor. Let's imagine that you have one instance P in the availability zone X and the other instance Q and the arbitor R in the availability zone Y.

At the start P is the primary. When Q fails, nothing happens and P continues working properly. When P fails, it fails over to Q. So far so good.

What happens when the entire X fails? Of course, it fails over to Q. Then, what happens when the entire Y fails. You may expect nothing will happen, but it's not true.

When Y fails, Q and R have gone away, and surprisingly, P resigns the primary, and no primary will be elected.

This is because an instance needs to be connected to more than half of the instances in the replica set to be a primary. Now, there are three instances in the replica set, but P connects to no instances. So P cannot be a primary.

This behavior is intended so that it won't elect multiple primaries when the network between X and Y fails but all instances are running. Without this rule, both P and Q will be primaries, and clients can write to both instances randomly.

You can move R to another availability zone Z to avoid this. In this case, P and R are connected even when Y fails, so P will be elected as a primary.

Of course, P will resign when both Y and Z fail, but it's less likely to happen.
