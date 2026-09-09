# Voice Input & Playback

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/chat/voice.md)

Chat supports voice on both sides: **voice questions** (record → auto-transcribe → send) and **voice answers** (TTS playback). Each side works independently — for example "voice in / text out" or "text in / voice out".

## Voice Input (Record a Question)

1. Click the mic icon in the input bar ("Click to start talking");
2. Click again to start recording; the icon area shows "Talking (N s)";
3. Click the icon once more to finish — the recording is **sent automatically**, no extra send click;
4. The audio is transcribed by the system's ASR service and sent as the question.

<figure>
  <img src="../../../image/cn/guide/chat/voice-01.png" alt="Voice input">
  <figcaption>Voice input</figcaption>
</figure>

Notes:

- The ASR service is configured globally by the admin (only one active at a time); users cannot switch it;
- Max duration and file size follow the system configuration (e.g. 60 seconds); overlong recordings are cut off;
- The transcription becomes the question text and cannot be edited before sending — ask again by text if the recognition is wrong.

## Voice Playback (AI Replies)

Controlled in the character's [settings](character-config.md#ai-reply-format):

| Setting | Options | Description |
|---|---|---|
| AI reply format | Auto / Text / Voice | "Auto" follows your input format |
| Auto-play voice replies | On / Off | Play automatically when the answer arrives |
| Voice | Dropdown | Server-side TTS: pick one; browser-side synthesis: not needed |

For voice answers, click **Show text / Show audio** to switch between them — text to check the content, audio to listen.

> [!NOTE]
> TTS is also configured by the admin. Whether synthesis happens server-side or in the browser depends on deployment: browser-side needs no voice selection, and available voices may differ.

## For Developers

Integrating new ASR/TTS platforms is development work — see [ASR Integration](../../dev/capability-asr.md) and [TTS Integration](../../dev/capability-tts.md).

---

Previous: [Character Settings](character-config.md) · Next: [Drawing](../draw/draw.md)
