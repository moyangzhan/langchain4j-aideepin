import { h } from 'vue'
import { NImage, NTag } from 'naive-ui'
import { BasicColumn } from '@/components/Table'
import { useI18n } from '@/locales'
import { useUser } from '@/store/modules/user'

export interface FileCleanupData {
  uuid: string
  name: string
  ext: string
  size: number | null
  url: string
  userId: number
  userName: string
  createTime: string
}

const IMAGE_EXTS = ['jpg', 'jpeg', 'png', 'gif', 'bmp', 'webp']
const TEXT_EXTS = ['txt', 'csv']

// 本地存储返回 /file/{uuid}.{ext} 相对路径，拼 /api 前缀带 token 预览；OSS 返回完整外链直用
// Local storage yields a relative /file/{uuid}.{ext} path (prefixed with /api plus a token for
// preview); OSS yields a full public link used as-is
export function previewUrl(row: FileCleanupData): string {
  if (row.url && row.url.startsWith('http')) {
    return row.url
  }
  return `/api${row.url}?token=${useUser().token}`
}

export function isImage(row: FileCleanupData): boolean {
  return IMAGE_EXTS.includes((row.ext || '').toLowerCase())
}

export function isTextPreviewable(row: FileCleanupData): boolean {
  return TEXT_EXTS.includes((row.ext || '').toLowerCase())
}

function formatSize(size: number | null): string {
  if (null == size) return '—'
  if (size < 1024) return `${size} B`
  if (size < 1024 * 1024) return `${(size / 1024).toFixed(1)} KB`
  if (size < 1024 * 1024 * 1024) return `${(size / 1024 / 1024).toFixed(1)} MB`
  return `${(size / 1024 / 1024 / 1024).toFixed(1)} GB`
}

export function getColumns(): BasicColumn<FileCleanupData>[] {
  const { t } = useI18n()
  return [
    {
      type: 'selection',
    },
    {
      title: t('columns.fileName'),
      key: 'name',
      width: 220,
      ellipsis: true,
    },
    {
      title: t('columns.fileType'),
      key: 'ext',
      width: 90,
      render(row) {
        return h(NTag, { size: 'small', bordered: false }, { default: () => row.ext || '—' })
      },
    },
    {
      title: t('columns.fileSize'),
      key: 'size',
      width: 100,
      render(row) {
        return formatSize(row.size)
      },
    },
    {
      title: t('columns.ownerName'),
      key: 'userName',
      width: 130,
    },
    {
      title: t('columns.createTime'),
      key: 'createTime',
      width: 170,
    },
    {
      title: t('columns.preview'),
      key: 'preview',
      width: 90,
      render(row) {
        if (!isImage(row) || !row.url) {
          return '—'
        }
        return h(NImage, {
          src: previewUrl(row),
          width: 56,
          objectFit: 'contain',
        })
      },
    },
  ]
}
