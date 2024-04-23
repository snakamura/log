# `SecItemCopyMatching` fails when a device is locked

Assuming you're writing an iOS app and try to store a token or a password in the app. You may want to store them in the keystore to secure them.

You can store them in the keychain using [Security Framework](https://developer.apple.com/documentation/security/keychain_services/keychain_items), and you'll use [`SecItemCopyMatching(_:_:)`](https://developer.apple.com/documentation/security/1398306-secitemcopymatching) to retrieve them from the keychain.

This works well as long as the app loads a token while it's in foreground. Sometimes you may want the app to run in background and configure it to be able to run in background. Now, the app will run even after you lock your device.

When you call `SecItemCopyMatching` while your device is locked, it fails and returns [`errSecInteractionNotAllowed`](https://developer.apple.com/documentation/security/errsecinteractionnotallowed). No, you cannot read the keychain while the device is locked.

Make sure to prepare tokens while the app is in foreground if the app needs them while it's in background.
