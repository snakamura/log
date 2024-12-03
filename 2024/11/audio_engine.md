# Tips about `AVAudioEngine`

There are several options when you want to deal with audio in your iOS app from using low-level [Core Audio](https://developer.apple.com/documentation/coreaudio) to more high-level APIs in `AVFoundation` such as [AVAudioPlayer](https://developer.apple.com/documentation/avfaudio/avaudioplayer).

[AVAudioEngine](https://developer.apple.com/documentation/avfaudio/avaudioengine) sits in the middle of them. It supports playing and recording audio at the same time, and apply effects on audio, and so on with a bit simpler APIs than Core Audio. Unfortunately, its documents don't explain its details very well, and it can be hard to use it especially if you're not familiar with audio processing like me.

I won't explain basic usage of `AVAudioEngine` here, but will explain some tips that might help you when you work with `AVAudioEngine`.

## Changing sampling rate with `AVAudioConverter`

Even though you can use the simple [`convert(to:from:)`](https://developer.apple.com/documentation/avfaudio/avaudioconverter/convert(to:from:)) to convert `AVAudioPCMBuffer`, you cannot use it when you change sampling rate. You need [`convert(to:error:withInputFrom:)`](https://developer.apple.com/documentation/avfaudio/avaudioconverter/convert(to:error:withinputfrom:)) to do it.

This function takes a callback function that feeds an input buffer. So you might use it like this.

```
// Prepare formats, input and output buffers
guard let converter = AVAudioConverter(
    from: inputFormat,
    to: outputFormat
) else {
    return
}

let outputStatus = converter.convert(
    to: outputBuffer,
    error: &error
) { _, inputStatus in
    inputStatus.pointee = .haveData
    return buffer
}
```

But this doesn't work well because `convert` calls your callback repeatedly until it fills the output buffer. After you return the input buffer you have, you need to set `inputStatus` to `.noDataNow` and return `nil`.

```
var processed = false
let outputStatus = converter.convert(
    to: outputBuffer,
    error: &error
) { _, inputStatus in
    guard !processed else {
        inputStatus.pointee = .noDataNow
        return nil
    }
    processed = true

    inputStatus.pointee = .haveData
    return buffer
}
switch outputStatus {
case .haveData, .inputRanDry:
    let data = Data(
        bytesNoCopy: outputBuffer.int16ChannelData!.pointee,
        count: Int(outputBuffer.frameLength) * MemoryLayout<Int16>.size,
        deallocator: .none
    )
case .endOfStream:
    break
case .error:
    print(error!)
@unknown default:
    fatalError()
}
```

In this case, `outputStatus` will be `.inputRanDry` instead of `.haveData`.

Also note that the output buffer might not contain all the results. `AVAudioConverter` might need another chunk of inputs to generate the next chunk of the output buffer. When you tap an `AVAudioInputNode` to get an input buffer, for example, you need to pass all the inputs to the same instance of `AVAudioConverter` to get a proper output.

Also you need to prepare for the fact that the number of samples it writes to the output buffer vary. For example, when you convert an input buffer in 48kHz to 24kHz, it'd first write 2048 samples three times, then write 4096 samples next. Make sure to create an output buffer with enough capacity.

## Playing audio and recording audio at the same time

To play and record audio at the same time, you need to pass `.playAndRecord` to [`AVAudioSession.setCategory`](https://developer.apple.com/documentation/avfaudio/avaudiosession/setcategory(_:mode:options:)). When you set `mode` to `.voiceChat` at this point, your iOS device will use a receiver instead of a speaker to play your audio. You can make it use a speaker by passing `.defaultToSpeaker` to `setCategory`, but this makes it record the audio it plays.

You can call [`AVAudioIONode.setVoiceProcessingEnabled`](https://developer.apple.com/documentation/avfaudio/avaudioionode/setvoiceprocessingenabled(_:)) to avoid this. This method enables Voice Processing I/O, which makes the input node to ignore audio from the output node.

Caution must be taken when you call this method though. When you call this method and call [`AVAudioEngine.start`](https://developer.apple.com/documentation/avfaudio/avaudioengine/start()), you'd find that the engine isn't running. This is because `AVAudioEngine` detects that its settings have changed, and stops itself. You can monitor this by observing [`AVAudioEngineConfigurationChange`](https://developer.apple.com/documentation/foundation/nsnotification/name/1389078-avaudioengineconfigurationchange) in `NotificationCenter`. Once you've observed this notification, restart your engine to make it start working.

```
init() {
    NotificationCenter.default.addObserver(
        self,
        selector: #selector(audioEngineConfigurationChange(_:)),
        name: .AVAudioEngineConfigurationChange,
        object: self._audioEngine
    )
}

@objc private func audioEngineConfigurationChange(_ notification: Notification) {
    if !self._audioEngine.isRunning {
        do {
            try self._audioEngine.start()
        } catch let e {
            print("Failed to restart AVAudioEngine: \(e)")
        }
    }
}
```

I'd recommend you take a look at [Using Voice Processing](https://developer.apple.com/documentation/avfaudio/audio_engine/audio_units/using_voice_processing) sample code when you're going to implement voice processing with `AVAudioEngine`.

## Format of [`AVAudioFile`](https://developer.apple.com/documentation/avfaudio/avaudiofile)

When you want to write audio to a file, `AVAudioFile` will help. You can specify a format of your file and write a buffer to it. One thing you need to take care of is that the format of the file and format of the buffer you need to write can be different. For example, even though you create an instance of `AVAudioFile` with 16bit 24kHz format, you'd need to write a buffer in 32bit 48kHz format to it by default.

You can check the former with [`fileFormat`](https://developer.apple.com/documentation/avfaudio/avaudiofile/fileformat), and the latter with [`processingFormat`](https://developer.apple.com/documentation/avfaudio/avaudiofile/processingformat). Also, use [`init(forWriting:settings:commonFormat:interleaved:)`](https://developer.apple.com/documentation/avfaudio/avaudiofile/init(forwriting:settings:commonformat:interleaved:)) when you create `AVAudioFile` to specify a processing format.
