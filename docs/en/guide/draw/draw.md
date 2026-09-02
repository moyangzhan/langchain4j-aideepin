# Drawing

> [← User Guide](../index.md) · [简体中文](../../../cn/guide/draw/draw.md)

On the **Draw** page: pick an image model at the top, fill in the prompt editor (which changes shape per platform), and review **my drawing history** below.

> 📷 Screenshot TODO: the draw page overview (model selector / editor / history). Replace with `![Draw page](../../../image/en/guide/draw/draw-01.png)` once added.

## Generation Flow

1. Pick a model in the image model selector — **each platform has its own editor**; switching models switches the editor;
2. Describe the image in the prompt box (Chinese or English);
3. Adjust editor parameters as needed (size, count, seed — see below);
4. Click generate; the task is submitted asynchronously;
5. Images appear in **my drawing history** when done — you can leave the page while it runs.

## Per-Platform Editor Parameters

### OpenAI (gpt-image)

| Parameter | Options |
|---|---|
| Size | auto / Square 1024x1024 / Portrait 1024x1536 / Landscape 1536x1024 |
| Quality | auto / low / medium / high (higher = finer, slower and costlier) |

### Tongyi Wanxiang (DashScope)

**Text-to-image**:

| Parameter | Description |
|---|---|
| Size | Determined by the selected model's configuration |
| Number | Images per run (slider 1–4) |
| Seed | Fixed seed reproduces results; "Random generate" differs each time |

**Background generation** — generates a new background for a subject image. The in-app usage notes read (translated):

> Subject image: the image whose background will be generated; required
> Guidance image: the image for AI to reference
> Prompt: description of the background
> Provide at least one of guidance image and prompt

| Parameter | Required | In-app hint (translated) |
|---|---|---|
| Subject | yes | Tooltip: "An image with a transparent background (RGBA, 4 channels)"; upload area: "PNG image, long edge no more than 2048 pixels" |
| Guidance image | no | Tooltip: "Common formats: jpg, png, webp. The guidance image can be RGB or RGBA with transparency; for RGBA, areas with Alpha = 0 do not participate in generation — suitable for guidance images with a subject." |
| Prompt | no | Background description (at least one of guidance image / prompt, otherwise "Please upload a guidance image or fill in the prompt") |

> [!WARNING]
> Background generation depends on **public accessibility** of the images: the model fetches the subject and guidance images over the public internet — with Alibaba Cloud OSS storage, images must be public-read; with local storage, the uploaded images must be publicly reachable (unavailable in local development). OSS is enabled in the admin console → System settings → Storage location.

### SiliconFlow

| Parameter | Description |
|---|---|
| Size | Read dynamically from model properties |
| Seed | -1 means random |

## My Drawing History

- Reverse chronological, **infinite scroll**, "No more" at the end;
- Click a record for the **detail view**: prompt and reference images (original / guidance), comments, **I'll draw one too** (reuse the prompt), **previous / next** record navigation ("No previous/next one" at the ends).

> 📷 Screenshot TODO: the drawing detail view (prompt, images, comments). Replace with `![Draw detail](../../../image/en/guide/draw/draw-02.png)` once added.

### Management

| Action | Where | Description |
|---|---|---|
| Delete task | Record's more menu | Deletes the prompt and **all images it generated** (the confirmation lists what goes) |
| Delete one image | On the image | Removes just that image, keeping the prompt and others |
| Public / private toggle | On the record | Public images enter the gallery ("Public access enabled"); off restores private ("External access disabled") |

> [!TIP]
> Want more exposure? Set works public and find them in the [Gallery](gallery.md) under "Public images", receiving likes and comments.

## API

**More → API** in the top bar generates a draw API key and endpoint docs for integrating text-to-image into your own program (create + poll), see [API Reference · Draw Tasks](../../api/draw.md).

---

Previous: [Voice Input & Playback](../chat/voice.md) · Next: [Gallery](gallery.md)
