-- ============================================================
-- OpenAI TTS & ASR model support
-- ============================================================

-- OpenAI 语音识别 (Whisper)
-- https://platform.openai.com/docs/guides/speech-to-text
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('whisper-1', 'OpenAI-语音识别', 'asr', 'openai', 'audio', false);

-- OpenAI 语音合成 (TTS)
-- https://platform.openai.com/docs/guides/text-to-speech
-- Voices: https://platform.openai.com/docs/guides/text-to-speech#voice-options
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('tts-1', 'OpenAI-语音合成', 'tts', 'openai', 'text', '{
  "voices": [
    {
      "name": "Alloy",
      "remark": "均衡中性",
      "param_name": "alloy",
      "lang": "多语言"
    },
    {
      "name": "Ash",
      "remark": "轻松沉稳男",
      "param_name": "ash",
      "lang": "多语言"
    },
    {
      "name": "Ballad",
      "remark": "柔和抒情男",
      "param_name": "ballad",
      "lang": "多语言"
    },
    {
      "name": "Coral",
      "remark": "温暖活力女",
      "param_name": "coral",
      "lang": "多语言"
    },
    {
      "name": "Echo",
      "remark": "沉稳叙事男",
      "param_name": "echo",
      "lang": "多语言"
    },
    {
      "name": "Fable",
      "remark": "英式讲故事",
      "param_name": "fable",
      "lang": "多语言"
    },
    {
      "name": "Nova",
      "remark": "友好专业女",
      "param_name": "nova",
      "lang": "多语言"
    },
    {
      "name": "Onyx",
      "remark": "权威深沉男",
      "param_name": "onyx",
      "lang": "多语言"
    },
    {
      "name": "Sage",
      "remark": "智慧温和女",
      "param_name": "sage",
      "lang": "多语言"
    },
    {
      "name": "Shimmer",
      "remark": "清脆明亮女",
      "param_name": "shimmer",
      "lang": "多语言"
    }
  ]
}', false);

-- 高清 TTS 模型（可选启用）
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('tts-1-hd', 'OpenAI-语音合成HD', 'tts', 'openai', 'text', '{
  "voices": [
    {
      "name": "Alloy",
      "remark": "均衡中性",
      "param_name": "alloy",
      "lang": "多语言"
    },
    {
      "name": "Ash",
      "remark": "轻松沉稳男",
      "param_name": "ash",
      "lang": "多语言"
    },
    {
      "name": "Ballad",
      "remark": "柔和抒情男",
      "param_name": "ballad",
      "lang": "多语言"
    },
    {
      "name": "Coral",
      "remark": "温暖活力女",
      "param_name": "coral",
      "lang": "多语言"
    },
    {
      "name": "Echo",
      "remark": "沉稳叙事男",
      "param_name": "echo",
      "lang": "多语言"
    },
    {
      "name": "Fable",
      "remark": "英式讲故事",
      "param_name": "fable",
      "lang": "多语言"
    },
    {
      "name": "Nova",
      "remark": "友好专业女",
      "param_name": "nova",
      "lang": "多语言"
    },
    {
      "name": "Onyx",
      "remark": "权威深沉男",
      "param_name": "onyx",
      "lang": "多语言"
    },
    {
      "name": "Sage",
      "remark": "智慧温和女",
      "param_name": "sage",
      "lang": "多语言"
    },
    {
      "name": "Shimmer",
      "remark": "清脆明亮女",
      "param_name": "shimmer",
      "lang": "多语言"
    }
  ]
}', false);
