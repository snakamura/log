# Taking a photo from your app on Android

Taking a photo from your app on Android is not difficult, but it's not just starting an activity using an intent with `MediaStore.ACTION_IMAGE_CAPTURE`.


    import android.app.Activity;
    import android.content.Intent;
    import android.database.Cursor;
    import android.net.Uri;
    import android.os.Bundle;
    import android.provider.MediaStore;

    import java.io.File;
    import java.io.FileNotFoundException;
    import java.io.IOException;

    public class CameraActivity extends Activity
    {
        public void takePhoto() {
            try {
                // Some apps create a temporary row in the content provider and pass its Uri like:
                //
                //     ContentValues values = new ContentValues();
                //     values.put(MediaStore.Images.Media.TITLE, filename);
                //     values.put(MediaStore.Images.Media.MIME_TYPE, "image/jpeg");
                //     imageForCamera_ = getContentResolver().insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values);
                //
                // but that's not a good idea because a user will see the temporary image
                // if they switch to Media App before taking a photo.
                imageFileForCamera_ = File.createTempFile("camera", ".jpg", getTemporaryDirectory());

                Intent cameraIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                cameraIntent.putExtra(MediaStore.EXTRA_OUTPUT, Uri.fromFile(imageFileForCamera_));
                startActivityForResult(cameraIntent, REQUEST_CAMERA);
            }
            catch (IOException e) {
                // Show an error.
            }
        }

        @Override
        protected void onRestoreInstanceState(Bundle savedInstanceState) {
            super.onRestoreInstanceState(savedInstanceState);

            // Because this activity may be destroyed while Camera App is active,
            // restore the path of the temporary file.
            imageFileForCamera_ = (File)savedInstanceState.getSerializable(INSTANCE_STATE_IMAGE_FILE_FOR_CAMERA);
        }

        @Override
        protected void onSaveInstanceState(Bundle outInstanceState) {
            super.onSaveInstanceState(outInstanceState);

            // Because this activity may be destroyed while Camera App is active,
            // save the path of the temporary file.
            outInstanceState.putSerializable(INSTANCE_STATE_IMAGE_FILE_FOR_CAMERA, imageFileForCamera_);
        }

        @Override
        protected void onActivityResult(int requestCode, int resultCode, Intent data) {
            switch (requestCode) {
            case REQUEST_CAMERA:
                try {
                    if (resultCode == RESULT_OK) {
                        // Some devices, such as Xperia, don't save the image
                        // to the file specified with MediaStore.EXTRA_OUTPUT,
                        // and returns it via data.
                        Uri uri = data != null ? data.getData() : null;
                        if (uri == null)
                            uri = Uri.fromFile(imageFileForCamera_);

                        if (data == null) {
                            // The devices which return the image via data store it
                            // in the media store automatically.
                            try {
                                MediaStore.Images.Media.insertImage(getContentResolver(), imageFileForCamera_.getAbsolutePath(), null, null);
                            }
                            catch (FileNotFoundException e) {
                                // Show an error.
                            }
                        }

                        String path = resolveContentUriToPath(uri);

                        // Use path here.

                    }
                }
                finally {
                    imageFileForCamera_.delete();
                    imageFileForCamera_ = null;
                }
                break;
            }
        }

        private String resolveContentUriToPath(Uri uri) {
            if (uri.getScheme().equals("file")) {
                return uri.getPath();
            }
            else {
                Cursor cursor = getContentResolver().query(uri, new String[] { MediaStore.Images.Media.DATA }, null, null, null);
                return cursor != null && cursor.moveToFirst() ? cursor.getString(0) : null;
            }
        }

        private File getTemporaryDirectory() {
            // Use .tmp as a name of a temporary directory
            // so that Media App won't list files in it.
            return new File(getExternalFilesDir(null), ".tmp");
        }

        private File imageFileForCamera_;

        private static final int REQUEST_CAMERA = 1;
        private static final String INSTANCE_STATE_IMAGE_FILE_FOR_CAMERA = "imageFileForCamera";
    }
