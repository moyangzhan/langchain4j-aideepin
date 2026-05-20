export default class AudioQueue {
  private audioContext: AudioContext
  private queue: ArrayBuffer[] = []
  private isPlaying = false
  private currentSource: AudioBufferSourceNode | null = null
  private startTime = 0
  private completing = false
  private waitingForData = false

  constructor(audioContext: AudioContext) {
    this.audioContext = audioContext
  }

  addChunk(chunk: ArrayBuffer) {
    this.queue.push(chunk)
    if (this.waitingForData) {
      this.waitingForData = false
      this.playNext()
    } else if (!this.isPlaying && this.queue.length >= AudioQueue.BATCH_SIZE) {
      this.playNext()
    }
  }

  private static readonly BATCH_SIZE = 5

  private async playNext() {
    if (this.queue.length === 0) {
      this.isPlaying = false
      if (this.completing)
        this.cleanup()
      else
        this.waitingForData = true
      return
    }

    this.waitingForData = false
    this.isPlaying = true
    try {
      const batchCount = Math.min(this.queue.length, AudioQueue.BATCH_SIZE)
      const chunks = this.queue.splice(0, batchCount)
      const merged = this.mergeChunks(chunks)
      const buffer = await this.decodeChunk(merged)
      if (!buffer) {
        this.playNext()
        return
      }

      const source = this.audioContext.createBufferSource()
      const gainNode = this.audioContext.createGain()
      source.connect(gainNode)
      gainNode.connect(this.audioContext.destination)
      source.buffer = buffer

      const now = this.audioContext.currentTime
      if (this.startTime === 0)
        this.startTime = now

      const scheduledTime = Math.max(this.startTime, now)
      gainNode.gain.setValueAtTime(0, scheduledTime)
      gainNode.gain.linearRampToValueAtTime(1, scheduledTime + 0.005)

      source.start(scheduledTime)
      this.startTime = scheduledTime + buffer.duration

      this.currentSource = source

      source.onended = () => {
        if (this.currentSource === source)
          this.currentSource = null
        this.playNext()
      }
    } catch (error) {
      console.error('Error in playNext:', error)
      this.isPlaying = false
    }
  }

  complete() {
    if (this.completing)
      return
    this.completing = true
    this.waitingForData = false
    if (!this.isPlaying) {
      if (this.queue.length > 0)
        this.playNext()
      else
        this.cleanup()
    }
  }

  private cleanup() {
    this.queue = []
    if (this.currentSource) {
      this.currentSource.stop()
      this.currentSource = null
    }
    this.isPlaying = false
    this.startTime = 0
    this.completing = false
    this.waitingForData = false
  }

  private mergeChunks(chunks: ArrayBuffer[]): ArrayBuffer {
    const totalLength = chunks.reduce((sum, c) => sum + c.byteLength, 0)
    const merged = new Uint8Array(totalLength)
    let offset = 0
    for (const chunk of chunks) {
      merged.set(new Uint8Array(chunk), offset)
      offset += chunk.byteLength
    }
    return merged.buffer
  }

  private async decodeChunk(chunk: ArrayBuffer): Promise<AudioBuffer | null> {
    try {
      return await this.audioContext.decodeAudioData(chunk)
    } catch (error) {
      console.error('Error decoding audio data:', error)
      return null
    }
  }

  async mergeAudioBuffers(buffer1: AudioBuffer, buffer2: AudioBuffer): Promise<AudioBuffer> {
    const mergedBuffer = this.audioContext.createBuffer(
      buffer1.numberOfChannels,
      buffer1.length + buffer2.length,
      buffer1.sampleRate,
    )
    for (let channel = 0; channel < buffer1.numberOfChannels; channel++) {
      const channelData = mergedBuffer.getChannelData(channel)
      const buffer1Data = buffer1.getChannelData(channel)
      const buffer2Data = buffer2.getChannelData(channel)
      channelData.set(buffer1Data, 0)
      channelData.set(buffer2Data, buffer1.length)
    }
    return mergedBuffer
  }
}
