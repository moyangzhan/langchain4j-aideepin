export default class AudioQueue {
  private audioContext: AudioContext
  private queue: ArrayBuffer[] = []
  private isPlaying = false
  private currentSource: AudioBufferSourceNode | null = null
  private startTime = 0
  private buffer: AudioBuffer | null = null

  constructor(audioContext: AudioContext) {
    this.audioContext = audioContext
  }

  // 添加音频数据到队列
  addChunk(chunk: ArrayBuffer) {
    this.queue.push(chunk)
    if (!this.isPlaying && this.queue.length > 10)
      this.playNext()
  }

  async chunkToAudioBuffer(chunk: ArrayBuffer) {
    let frameBuffer = null
    try {
      frameBuffer = await this.audioContext.decodeAudioData(chunk)
    } catch (error) {
      console.error('Error decoding audio data:', error)
    }
    if (!frameBuffer)
      return

    const fadeDuration = 0.1
    frameBuffer = this.applyFadeIn(frameBuffer, fadeDuration)
    if (this.currentSource) {
      // 如果前一个音频还在播放，对其应用淡出效果
      const prevBuffer = this.currentSource.buffer
      if (prevBuffer)
        this.applyFadeOut(prevBuffer, fadeDuration)
    }
    return frameBuffer
  }

  // 播放下一段音频
  private async playNext() {
    console.log('play next,queue length:', this.queue.length)
    if (this.queue.length === 0) {
      this.isPlaying = false
      return
    }

    this.isPlaying = true
    try {
      // let i = 0
      // while (this.queue.length > 0) {
      //   console.log('decode audio data,i:', i++)
      //   const chunk = this.queue.shift()!
      //   if (!chunk) {
      //     console.warn('chunk is null')
      //     continue;
      //   }
      //   await this.chunkToAudioBuffer(chunk)
      // }
      const chunk = this.queue.shift()!
      const bf = await this.chunkToAudioBuffer(chunk)
      if (!bf) {
        this.playNext()
        return
      }
      this.buffer = bf
      this.currentSource = this.audioContext.createBufferSource()
      this.currentSource.connect(this.audioContext.destination)
      this.currentSource.buffer = this.buffer
      // 计算准确的播放时间点，确保音频连续
      const currentTime = this.audioContext.currentTime
      if (this.startTime === 0)
        this.startTime = currentTime

      const scheduledTime = Math.max(this.startTime, currentTime)
      this.currentSource.start(scheduledTime)
      this.startTime = scheduledTime + this.buffer.duration

      this.currentSource.onended = () => {
        this.currentSource = null
        console.log('audio stream ended, play next, queue length:', this.queue.length)
        this.playNext()
      }
    } catch (error) {
      console.error('Error decoding audio data:', error)
      this.isPlaying = false
    }
  }

  // 播放剩余的内容然后清空队列
  async complete() {
    console.log('audio stream complete')
    if (this.isPlaying) {
      setTimeout(() => {
        this.complete()
      }, 1000)
      return
    } else if (!this.isPlaying && this.queue.length > 0) {
      await this.playNext()
      setTimeout(() => {
        this.complete()
      }, 1000)
      return
    }
    await this.playNext()
    this.queue = []
    if (this.currentSource) {
      this.currentSource.stop()
      this.currentSource = null
    }
    this.isPlaying = false
    this.startTime = 0
  }

  async mergeAudioBuffers(buffer1: AudioBuffer, buffer2: AudioBuffer): Promise<AudioBuffer> {
    // 创建新的AudioBuffer，长度为两个buffer之和
    const mergedBuffer = this.audioContext.createBuffer(
      buffer1.numberOfChannels,
      buffer1.length + buffer2.length,
      buffer1.sampleRate,
    )

    // 对每个声道进行合并
    for (let channel = 0; channel < buffer1.numberOfChannels; channel++) {
      const channelData = mergedBuffer.getChannelData(channel)
      const buffer1Data = buffer1.getChannelData(channel)
      const buffer2Data = buffer2.getChannelData(channel)

      // 复制第一个buffer的数据
      channelData.set(buffer1Data, 0)
      // 复制第二个buffer的数据
      channelData.set(buffer2Data, buffer1.length)
    }

    return mergedBuffer
  }

  // 优化淡入淡出函数
  private applyFadeIn(buffer: AudioBuffer, fadeDuration: number): AudioBuffer {
    const fadeSamples = Math.floor(fadeDuration * buffer.sampleRate)
    const fadeEnd = Math.min(fadeSamples, buffer.length)

    // 使用更平滑的曲线（二次函数）
    for (let channel = 0; channel < buffer.numberOfChannels; channel++) {
      const channelData = buffer.getChannelData(channel)

      for (let i = 0; i < fadeEnd; i++) {
        const fadeProgress = i / fadeSamples
        // 使用二次函数使淡入更平滑
        channelData[i] *= fadeProgress * fadeProgress
      }
    }
    return buffer
  }

  private applyFadeOut(buffer: AudioBuffer, fadeDuration: number): AudioBuffer {
    const fadeSamples = Math.floor(fadeDuration * buffer.sampleRate)
    const length = buffer.length
    const fadeStart = Math.max(0, length - fadeSamples)

    // 使用更平滑的曲线（二次函数）
    for (let channel = 0; channel < buffer.numberOfChannels; channel++) {
      const channelData = buffer.getChannelData(channel)

      for (let i = fadeStart; i < length; i++) {
        const fadeProgress = (i - fadeStart) / fadeSamples
        // 使用二次函数使淡出更平滑
        channelData[i] *= (1 - fadeProgress) * (1 - fadeProgress)
      }
    }
    return buffer
  }
}
