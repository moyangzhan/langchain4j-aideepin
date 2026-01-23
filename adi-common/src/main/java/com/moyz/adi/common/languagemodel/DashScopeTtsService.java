package com.moyz.adi.common.languagemodel;

import com.alibaba.dashscope.audio.tts.SpeechSynthesisResult;
import com.alibaba.dashscope.audio.ttsv2.SpeechSynthesisAudioFormat;
import com.alibaba.dashscope.audio.ttsv2.SpeechSynthesisParam;
import com.alibaba.dashscope.audio.ttsv2.SpeechSynthesizer;
import com.alibaba.dashscope.common.ResultCallback;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import com.moyz.adi.common.file.FileOperatorContext;
import com.moyz.adi.common.util.LocalDateTimeUtil;
import com.moyz.adi.common.util.UuidUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import java.nio.ByteBuffer;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

import static com.alibaba.dashscope.audio.ttsv2.SpeechSynthesisAudioFormat.MP3_16000HZ_MONO_128KBPS;
import static com.moyz.adi.common.cosntant.AdiConstant.TtsConstant.DASHSCOPE_DEFAULT_VOICE;
import static com.moyz.adi.common.util.LocalDateTimeUtil.PATTERN_YYYYMMDDMMHHSS;

@Slf4j
public class DashScopeTtsService extends AbstractTtsModelService {

    private final Map<String, SpeechSynthesizer> jobToSynthesizer = new HashMap<>();
    private final Map<String, ByteBuffer> jobToAudioData = new HashMap<>();
    private static final SpeechSynthesisAudioFormat AUDIO_FORMAT = SpeechSynthesisAudioFormat.PCM_22050HZ_MONO_16BIT;

    public DashScopeTtsService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    @Override
    public void start(String jobId, String voice, Consumer<ByteBuffer> onProcess, Consumer<String> onComplete, Consumer<String> onError) {
        // Check or set default
        if (StringUtils.isBlank(voice)) {
            voice = DASHSCOPE_DEFAULT_VOICE;
        } else {
            // 音色相关参数：https://bailian.console.aliyun.com/cn-beijing/?spm=5176.28197632.console-base_help.2.58192e369wELx6&tab=api#/api/?type=model&url=2997333
            boolean match = false;
            ObjectNode node = aiModel.getProperties();
            if (node.has("voices") && node.get("voices").isArray()) {
                ArrayNode voices = node.withArray("voices");
                for (JsonNode voiceNode : voices) {
                    if (voice.equals(voiceNode.get("param_name").asText())) {
                        match = true;
                        break;
                    }
                }
            }
            if (!match) {
                voice = DASHSCOPE_DEFAULT_VOICE;
            }
        }
        log.info("开始语音合成，jobId: {}, voice: {}", jobId, voice);
        // 配置回调函数
        ResultCallback<SpeechSynthesisResult> callback = new ResultCallback<>() {
            @Override
            public void onEvent(SpeechSynthesisResult result) {
                if (result.getAudioFrame() != null) {
                    ByteBuffer audioFrame = result.getAudioFrame();
                    ByteBuffer byteBuffer = jobToAudioData.get(jobId);
                    if (null == byteBuffer) {
                        jobToAudioData.put(jobId, audioFrame);
                    } else {
                        jobToAudioData.put(jobId, combineBuffer(byteBuffer, audioFrame));
                    }
                    onProcess.accept(audioFrame);
                }
            }

            @Override
            public void onComplete() {
                log.info("收到Complete，语音合成结束,jobId:{}", jobId);
                ByteBuffer audioData = jobToAudioData.get(jobId).rewind();
                jobToSynthesizer.remove(jobId);
                jobToAudioData.remove(jobId);

                // 获取 PCM 数据字节
                byte[] pcmData = new byte[audioData.remaining()];
                audioData.get(pcmData);

                String datetime = LocalDateTimeUtil.format(LocalDateTime.now(), PATTERN_YYYYMMDDMMHHSS);
                Pair<String, String> pair = new FileOperatorContext().save(pcmData, false, aiModel.getName().toLowerCase() + "-" + datetime + "-" + UuidUtil.createShort().substring(0, 6) + ".mp3");
                log.info("保存文件成功，路径为：" + pair.getLeft());

                onComplete.accept(pair.getLeft());

            }

            @Override
            public void onError(Exception e) {
                log.error("tts error,jobId:{}", jobId, e);
                jobToSynthesizer.remove(jobId);
                jobToAudioData.remove(jobId);
                onError.accept(e.getMessage());
            }
        };
        // 请求参数
        SpeechSynthesisParam param =
                SpeechSynthesisParam.builder()
                        .apiKey(platform.getApiKey())
                        .model(aiModel.getName())
                        .voice(StringUtils.isBlank(voice) ? DASHSCOPE_DEFAULT_VOICE : voice)
                        .format(MP3_16000HZ_MONO_128KBPS) // 流式合成使用PCM或者MP3
                        .build();
        SpeechSynthesizer synthesizer = new SpeechSynthesizer(param, callback);
        jobToSynthesizer.put(jobId, synthesizer);
    }

    @Override
    public void processByStream(String jobId, String partText) {
        if (!jobToSynthesizer.containsKey(jobId)) {
            log.error("没有找到对应的jobId: {}", jobId);
            return;
        }
        log.info("开始流式合成，jobId: {}, partText: {}", jobId, partText);
        SpeechSynthesizer synthesizer = jobToSynthesizer.get(jobId);
        synthesizer.streamingCall(partText);
    }

    @Override
    public void complete(String jobId) {
        SpeechSynthesizer synthesizer = jobToSynthesizer.get(jobId);
        if (null != synthesizer) {
            synthesizer.streamingComplete();
            log.info("[Metric] requestId为：" + synthesizer.getLastRequestId() + "，首包延迟（毫秒）为：" + synthesizer.getFirstPackageDelay());
        }
    }

    public byte[] returnWavBytes(byte[] pcmData) {
        // 生成 WAV 文件头
        byte[] wavHeader = generateWavHeader(pcmData.length);

        // 合并 WAV Header 和 PCM 数据
        byte[] wavData = new byte[wavHeader.length + pcmData.length];
        System.arraycopy(wavHeader, 0, wavData, 0, wavHeader.length);
        System.arraycopy(pcmData, 0, wavData, wavHeader.length, pcmData.length);
        return wavData;
    }

    /**
     * 生成WAV格式的文件头
     *
     * @param totalAudioSize PCM数据的大小（不包含头部）
     * @return byte[] WAV头部字节数组
     */
    private byte[] generateWavHeader(int totalAudioSize) {
        long totalLength = totalAudioSize + 36;
        long sampleRate = AUDIO_FORMAT.getSampleRate();
        short channels = 1;
        short bitsPerSample = 16;
        short blockAlign = (short) (channels * (bitsPerSample / 8));
        int byteRate = (int) (sampleRate * blockAlign);

        ByteBuffer headerBuffer = ByteBuffer.allocate(44);

        // RIFF 头部
        headerBuffer.put("RIFF".getBytes()); // ChunkID
        headerBuffer.putInt((int) (totalLength - 8)); // ChunkSize
        headerBuffer.put("WAVE".getBytes()); // Format

        // fmt 子块
        headerBuffer.put("fmt ".getBytes()); // Subchunk1ID
        headerBuffer.putInt(16); // Subchunk1Size
        headerBuffer.putShort((short) 1); // AudioFormat (PCM = 1)
        headerBuffer.putShort(channels);
        headerBuffer.putInt((int) sampleRate);
        headerBuffer.putInt(byteRate);
        headerBuffer.putShort(blockAlign);
        headerBuffer.putShort(bitsPerSample);

        // data 子块
        headerBuffer.put("data".getBytes()); // Subchunk2ID
        headerBuffer.putInt(totalAudioSize); // Subchunk2Size

        return headerBuffer.array();
    }

    private static ByteBuffer cloneBuffer(ByteBuffer original) {
        ByteBuffer clone = ByteBuffer.allocate(original.capacity());
        original.rewind(); // copy from the beginning
        clone.put(original);
        original.rewind();
        clone.flip();
        return clone;
    }

    private static ByteBuffer combineBuffer(ByteBuffer original, ByteBuffer toAppend) {
        original.rewind();
        toAppend.rewind();
        ByteBuffer combined = ByteBuffer.allocate(original.capacity() + toAppend.capacity());
        combined.put(original);
        combined.put(toAppend);
        original.rewind();
        toAppend.rewind();
        combined.flip();
        return combined;
    }
}
