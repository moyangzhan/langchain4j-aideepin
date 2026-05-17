package com.moyz.adi.common.languagemodel.data;

import lombok.Data;

import java.nio.ByteBuffer;
import java.util.function.Consumer;

@Data
public class TtsJobData {
    private StringBuilder text;
    private Consumer<String> completeCallback;
    private Consumer<String> errorCallback;
    private Consumer<ByteBuffer> processCallback;
    private String voice;
}
