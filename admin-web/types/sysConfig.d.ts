export interface QuotaConfig {
  daily: number
  monthly: number
}
export interface RateLimitConfig {
  times: number
  minutes: number
}
export interface AliyunOssConfig {
  access_key_id: string
  access_key_secret: string
  bucket_name: string
  endpoint: string
}
export interface AsrConfig {
  model_name: string
  platform: string
  max_record_duration: number
  max_file_size: number
}
export interface TtsConfig {
  synthesizer_side: string
  platform: string
  model_name: string
}