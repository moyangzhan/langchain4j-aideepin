# Drawing

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/draw/draw.md)

On the **Draw** page: pick an image model at the top, fill in the prompt editor (which changes shape per platform), and review **my drawing history** below.

## Generation Flow

1. Pick a model in the image model selector (each platform has its own editor);
2. Fill in the prompt and parameters, submit the task;
3. The task runs asynchronously; images appear in the history when done.

## Per-Platform Editor Parameters

### OpenAI (gpt-image)

| Parameter | Options |
|---|---|
| Size | Square 1024x1024 / Portrait 1024x1536 / Landscape 1536x1024 |
| Quality | auto / low / medium / high |

### Tongyi Wanxiang (DashScope)

**Text-to-image**:

| Parameter | Description |
|---|---|
| Size | Fixed options |
| Number | Images per run |
| Seed | Fixed seed reproduces results; random differs each time |

**Background generation** — generates a new background for a subject image:

| Parameter | Description |
|---|---|
| Subject (required) | PNG, long edge ≤ 2048; **RGBA transparent areas** are where the background is generated |
| Guidance image (optional) | jpg / png / webp, constrains the background style |
| Prompt | Background description |

> Background generation relies on publicly accessible Alibaba Cloud OSS URLs; unreadable images fail the task — read the in-page instructions.

### SiliconFlow

| Parameter | Description |
|---|---|
| Size | Read dynamically from model properties |
| Seed | -1 means random |

## My Drawing History

- Reverse chronological, **infinite scroll**, "No more" at the end;
- Click a record for the **detail view**: prompt and reference images (original / guidance), comments, **I'll draw one too** (reuse the prompt), and **previous / next** record navigation.

### Management

| Action | Description |
|---|---|
| Delete task | Choose: delete the prompt only, or the prompt plus all images it generated |
| Delete one image | Removes just that image, keeping the prompt and others |
| Public / private toggle | Public images enter the gallery for everyone; off restores private |

## API

**More → API** in the top bar generates a draw API key and shows the endpoint docs for integrating text-to-image into your own program — see [API Reference · Draw Tasks](../../api/draw.md).

---

Previous: [Voice Input & Playback](../chat/voice.md) ｜ Next: [Gallery](gallery.md)
