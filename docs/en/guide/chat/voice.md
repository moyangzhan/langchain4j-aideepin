# Voice Input & Playback

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/chat/voice.md)

Chat supports a full voice loop: **voice questions** (record → auto-transcribe → send) and **voice answers** (TTS playback).

## Voice Input (Record a Question)

1. Click the mic icon in the input bar to **start**; the UI shows "Talking (N s)";
2. Click the icon again to finish — the recording is **sent** automatically;
3. The audio is transcribed by the system's ASR service and sent as the question.

Notes:

- The ASR service is configured globally by the admin (only one active at a time); users cannot switch it;
- Max duration and file size follow the system configuration; overlong recordings are cut off.

## Voice Playback (AI Replies)

Controlled in the character's [settings](character-config.md):

| Setting | Description |
|---|---|
| AI reply format | Auto / Text / Voice — "Auto" follows your input format |
| Auto-play voice replies | Play automatically when the answer arrives |
| Voice | Selectable with server-side TTS; not needed with browser-side synthesis |

For voice answers, click **Show text / Show audio** to switch between the two presentations.

> TTS is likewise configured by the admin. Whether synthesis happens server-side or in the browser depends on deployment; browser-side synthesis needs no voice selection.

## For Developers

Integrating new ASR/TTS platforms is development work — see [ASR Integration](../../dev/capability-asr.md) and [TTS Integration](../../dev/capability-tts.md).

---

Previous: [Character Settings](character-config.md) ｜ Next: [Drawing](../draw/draw.md)
