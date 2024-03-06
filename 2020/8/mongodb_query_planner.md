# MongoDB query planner can be very slow

A while back, I was investigating some slow queries on MongoDB which took about 1,500ms. It actually took 1,500ms when I checked that query with `db.collection.find({...}).sort({...}).limit(...).explain('executionStats')`, but the picked query plan used a proper index, and the planner estimated that it'd take less than 100ms.

Then, I gave it a hint about which index it should use, like `db.collection.find({...}).sort({...}).limit(...).hint('indexName').explain('executionStats')`. Then, the query took less than 100ms even though the specified index was the index the planner had picked.

It turned out that the reason for these slow queries was the planner tried too many query plans too deeply, which made the query itself slow. This seems to happen when a query is complex and there are many indexes, but it doesn't always happen. I tried to create a simple reproduciable case, but I had no luck so far.

I found the ticket related to this issue at [Progressive elimination of candidate plans during plan ranking trial period](https://jira.mongodb.org/browse/SERVER-24396) in the MongoDB JIRA. They've at least noticed this issue, but it's still open after 3 years.
