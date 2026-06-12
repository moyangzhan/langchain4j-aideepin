package com.moyz.adi.common.languagemodel;

import com.moyz.adi.common.entity.AiModel;
import com.moyz.adi.common.entity.ModelPlatform;
import lombok.extern.slf4j.Slf4j;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * TTS 模型服务抽象基类。
 * <p>
 * 通过 latch + 回调包装的双模板方法统一同步语义：保证 {@link #complete(String)} 返回时，
 * 子类异步回调（onComplete/onError）已经执行完毕，调用方可以安全地读取由回调回填的状态
 * （例如 filePath）。子类对 latch 完全无感知，只需实现 {@link #doStart} 与 {@link #doComplete}。
 * <p>
 * Abstract base class for TTS model services.
 * <p>
 * Provides latch-based + callback-wrapping template methods to unify completion semantics: when
 * {@link #complete(String)} returns, asynchronous callbacks (onComplete/onError) supplied by
 * subclasses are guaranteed to have finished, so callers can safely read state populated by those
 * callbacks (e.g. filePath). Subclasses are entirely unaware of the latch — they only need to
 * implement {@link #doStart} and {@link #doComplete}.
 */
@Slf4j
public abstract class AbstractTtsModelService extends CommonModelService {

    /**
     * 默认完成等待超时时间（秒）。超时后 {@link #complete(String)} 仍会返回，但 filePath 可能为空。
     * <p>
     * Default timeout (in seconds) for awaiting completion. On timeout, {@link #complete(String)}
     * still returns but filePath may be null.
     */
    private static final long COMPLETE_TIMEOUT_SECONDS = 30;

    private final Map<String, CountDownLatch> jobToCompleteLatch = new ConcurrentHashMap<>();

    public AbstractTtsModelService(AiModel model, ModelPlatform modelPlatform) {
        super(model, modelPlatform);
    }

    /**
     * 启动一个 TTS 任务。父类的最终模板方法：
     * <ol>
     *   <li>登记 latch；</li>
     *   <li>对子类传入的 onComplete / onError 包裹一层，确保业务回调执行完后释放 latch；</li>
     *   <li>调用 {@link #doStart} 让子类发起真正的会话/连接。</li>
     * </ol>
     * 子类感知不到 latch，只需把收到的（已包装的）回调挂到 SDK / HTTP 框架上即可。
     * <p>
     * Start a TTS job. Final template method: registers the latch, wraps the supplied
     * onComplete/onError so the latch is released after the business callback finishes, then
     * delegates to {@link #doStart}. Subclasses just hand the (wrapped) callbacks to their
     * SDK/HTTP framework — they never see the latch.
     *
     * @param jobId      任务 id / Job id
     * @param voice      音色 / Voice
     * @param onProcess  音频帧回调 / Audio frame callback
     * @param onComplete 完成回调 / Completion callback (receives saved file path)
     * @param onError    错误回调 / Error callback
     */
    public final void start(String jobId, String voice,
                            Consumer<ByteBuffer> onProcess,
                            Consumer<String> onComplete,
                            Consumer<String> onError) {
        registerJob(jobId);
        Consumer<String> wrappedComplete = filePath -> {
            try {
                onComplete.accept(filePath);
            } finally {
                notifyJobFinished(jobId);
            }
        };
        Consumer<String> wrappedError = errMsg -> {
            try {
                onError.accept(errMsg);
            } finally {
                notifyJobFinished(jobId);
            }
        };
        try {
            doStart(jobId, voice, onProcess, wrappedComplete, wrappedError);
        } catch (RuntimeException e) {
            // doStart 自身抛异常时，已包装的回调可能从未触发，这里兜底释放 latch。
            // If doStart throws, the wrapped callbacks may never fire; release the latch here.
            notifyJobFinished(jobId);
            throw e;
        }
    }

    /**
     * 子类实现真正的"启动"动作（建立 WebSocket、初始化 SDK 状态等）。
     * 收到的 onComplete / onError 已经被父类包装过，子类只需在 SDK / HTTP 框架的
     * 回调路径上调用它们，无需关心 latch 释放。
     * <p>
     * Subclasses implement the actual "start" action (open WebSocket, init SDK state, etc).
     * The onComplete/onError received here are already wrapped by the parent — subclasses just
     * invoke them from their SDK/HTTP callback paths and don't need to think about the latch.
     */
    protected abstract void doStart(String jobId, String voice,
                                    Consumer<ByteBuffer> onProcess,
                                    Consumer<String> onComplete,
                                    Consumer<String> onError);

    /**
     * 处理语音合成任务（部分文本）。
     * <p>
     * Process a partial text segment for the speech synthesis job.
     *
     * @param jobId    Job id
     * @param partText Partial text
     */
    public abstract void processByStream(String jobId, String partText);

    /**
     * 子类实现真正的"完成"动作（如发送 finish 帧、发起一次性 HTTP 请求）。
     * 调用时机由父类 {@link #complete(String)} 控制。
     * <p>
     * Subclasses implement the real "complete" action (e.g. sending a finish frame, firing a
     * one-shot HTTP request). Invocation is orchestrated by {@link #complete(String)}.
     *
     * @param jobId Job id
     */
    protected abstract void doComplete(String jobId);

    /**
     * 模板方法：触发完成动作并阻塞等待异步回调结束。无论 doComplete 是否抛异常，
     * latch 最终都会被释放，避免下一次相同 jobId 复用时遗留状态。
     * <p>
     * Template method: triggers the completion action and blocks until the async callback
     * finishes. The latch is always released eventually, even if doComplete throws, to prevent
     * stale state from leaking into the next reuse of the same jobId.
     *
     * @param jobId Job id
     */
    public final void complete(String jobId) {
        CountDownLatch latch = jobToCompleteLatch.get(jobId);
        try {
            doComplete(jobId);
            if (latch != null && !latch.await(COMPLETE_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
                log.warn("TTS complete timeout after {}s, jobId:{}", COMPLETE_TIMEOUT_SECONDS, jobId);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("TTS complete interrupted, jobId:{}", jobId);
        } finally {
            // 兜底：无论同步/异步路径、无论是否超时，complete 返回前都确保 latch 释放并清理。
            // 如果 callback 路径已经 remove，这里就是 no-op；countDown 在计数 0 时也是 no-op（JDK 文档）。
            // <p>
            // Final safeguard: regardless of sync/async path or timeout, ensure the latch is
            // released and cleaned up before returning. If the callback path already removed it,
            // this is a no-op; countDown is also a no-op once count reaches 0 (per JDK contract).
            notifyJobFinished(jobId);
        }
    }

    private void registerJob(String jobId) {
        jobToCompleteLatch.put(jobId, new CountDownLatch(1));
    }

    private void notifyJobFinished(String jobId) {
        CountDownLatch latch = jobToCompleteLatch.remove(jobId);
        if (latch != null) {
            latch.countDown();
        }
    }

    public AbstractTtsModelService setProxyAddress(InetSocketAddress proxyAddress) {
        this.proxyAddress = proxyAddress;
        return this;
    }
}
