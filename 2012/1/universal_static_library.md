# Creating a universal static library for iOS

If you want to provide a static library for others, it's convenient to create a universal static library that includes libraries for arm6, arm7 and simulator. To create a universal static library, use `lipo` command. For example,

    lipo -create Release-iphoneos/libtest.a Release-iphonesimulator/libtest.a -output libtest.a

will create libtest.a which contains libraries both for a device and a simulator.
