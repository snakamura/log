# Sharing a file in Slack via API is now asynchronous

Slack has an API to upload a file named [`files.upload`](https://api.slack.com/methods/files.upload) that has already been deprecated. You now need to call [`files.getUploadURLExternal`](https://api.slack.com/methods/files.getUploadURLExternal) to get an endpoint to upload a file, upload a file, then call [`files.completeUploadExternal`](https://api.slack.com/methods/files.completeUploadExternal) to get metadata of the uploaded file.

Unfortunately, unlike `files.upload`, metadata returned from `files.completeUploadExternal` [might not be updated](https://github.com/slackapi/python-slack-sdk/issues/1277). For example, even when you upload a file and share it to a specific channel by specifying `channel_id`, returned metadata might not contain information about it.

You need to call [`files.info`](https://api.slack.com/methods/files.info) repeatedly until you get it when you want to get updated metadata, such as `ts` of a post for the shared file.
