# Posting images with a structured message to Slack

Slack API supports posting a message, uploading a file, but it's not straightforward to post images with structured message.

First, you can post a message with [`chat.postMessage`](https://api.slack.com/methods/chat.postMessage), and build a structured message using `attachments` or `blocks`. But it doesn't allow you to post files. The best you can do is to use [Image Block](https://api.slack.com/reference/block-kit/blocks#image) and pass a URL to an image. But, this means that these images are publicly available. Also, you'll see these links in the message.

You can also include a link to an image in a text and let Slack unfurl it, but again, the image needs to be public.

Second, you can upload a file with [`files.upload`](https://api.slack.com/methods/files.upload). You can post it to a channel with `channel`, but you can only post it with a simple message using `initial_comment`.

To post a file with a structured message, you need to upload a file with `files.upload`, and update it with [`chat.update`](https://api.slack.com/methods/chat.update). When you update an existing message, you can specify `attachments` and `blocks`. It even allows you to specify `file_ids` to add more files uploaded with `files.upload`.

It looks weird that `chat.postMessage` doesn't support `file_ids`, and `files.upload` doesn't support `attachments` and `blocks`, but you can rely on `chat.update`.
