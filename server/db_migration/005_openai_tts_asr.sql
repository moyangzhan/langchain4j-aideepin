-- ============================================================
-- OpenAI TTS & ASR model support
-- ============================================================

-- OpenAI 语音识别 (ASR)
-- https://platform.openai.com/docs/guides/speech-to-text
INSERT INTO adi_ai_model (name, title, type, platform, input_types, is_enable)
VALUES ('gpt-4o-mini-transcribe', 'OpenAI-ASR', 'asr', 'openai', 'audio', false);

-- OpenAI 语音合成 (TTS)
-- https://platform.openai.com/docs/guides/text-to-speech
-- Voices: https://platform.openai.com/docs/guides/text-to-speech#voice-options
-- gpt-4o-mini-tts supports all 13 voices
INSERT INTO adi_ai_model (name, title, type, platform, input_types, properties, is_enable)
VALUES ('gpt-4o-mini-tts', 'OpenAI-TTS', 'tts', 'openai', 'text', '{
  "voices": [
    {
      "name": "Alloy",
      "remark": "Alloy",
      "param_name": "alloy",
      "lang": "multilingual"
    },
    {
      "name": "Ash",
      "remark": "Ash",
      "param_name": "ash",
      "lang": "multilingual"
    },
    {
      "name": "Ballad",
      "remark": "Ballad",
      "param_name": "ballad",
      "lang": "multilingual"
    },
    {
      "name": "Coral",
      "remark": "Coral",
      "param_name": "coral",
      "lang": "multilingual"
    },
    {
      "name": "Echo",
      "remark": "Echo",
      "param_name": "echo",
      "lang": "multilingual"
    },
    {
      "name": "Fable",
      "remark": "Fable",
      "param_name": "fable",
      "lang": "multilingual"
    },
    {
      "name": "Nova",
      "remark": "Nova",
      "param_name": "nova",
      "lang": "multilingual"
    },
    {
      "name": "Onyx",
      "remark": "Onyx",
      "param_name": "onyx",
      "lang": "multilingual"
    },
    {
      "name": "Sage",
      "remark": "Sage",
      "param_name": "sage",
      "lang": "multilingual"
    },
    {
      "name": "Shimmer",
      "remark": "Shimmer",
      "param_name": "shimmer",
      "lang": "multilingual"
    },
    {
      "name": "Verse",
      "remark": "Verse",
      "param_name": "verse",
      "lang": "multilingual"
    },
    {
      "name": "Marin",
      "remark": "Marin",
      "param_name": "marin",
      "lang": "multilingual"
    },
    {
      "name": "Cedar",
      "remark": "Cedar",
      "param_name": "cedar",
      "lang": "multilingual"
    }
  ]
}', false);

-- ============================================================
-- Open API: Add api_key column to conversation, knowledge base, workflow
-- ============================================================
ALTER TABLE adi_conversation ADD COLUMN api_key varchar(200) DEFAULT '' NOT NULL;
ALTER TABLE adi_knowledge_base ADD COLUMN api_key varchar(200) DEFAULT '' NOT NULL;
ALTER TABLE adi_workflow ADD COLUMN api_key varchar(200) DEFAULT '' NOT NULL;
