import { defineConfig, type DefaultTheme } from 'vitepress'
import fs from 'node:fs'
import path from 'node:path'

// ---------------------------------------------------------------------------
// Sidebar 自动生成：解析各 section 的 index.md
//   - H2 标题 → sidebar 分组
//   - 有序列表项 `1. [标题](相对路径) 说明` → 分组内页面
// 各语言（cn / en）× 各 section（guide / api / dev）各调用一次；
// index.md 是导航顺序的唯一事实来源，日常加页面只需更新 index.md。
// ---------------------------------------------------------------------------

const docRoot = path.resolve(process.cwd(), 'docs')

type SidebarItem = { text: string; link: string }
type SidebarGroup = { text: string; items: SidebarItem[] }

function parseSectionIndex(section: string): SidebarGroup[] {
  const indexPath = path.join(docRoot, ...section.split('/'), 'index.md')
  const content = fs.readFileSync(indexPath, 'utf-8')
  const h1 = content.match(/^#\s+(.+)$/m)?.[1]?.trim() ?? section
  const groups: SidebarGroup[] = []
  let current: SidebarGroup | null = null
  for (const line of content.split(/\r?\n/)) {
    const h2 = line.match(/^##\s+(.+)/)
    if (h2) {
      current = { text: h2[1].trim(), items: [] }
      groups.push(current)
      continue
    }
    const li = line.match(/^\s*\d+\.\s+\[([^\]]+)\]\(([^)]+)\)/)
    if (li) {
      if (!current) {
        current = { text: h1, items: [] }
        groups.push(current)
      }
      current.items.push({
        text: li[1].trim(),
        link: `/${section}/${li[2].replace(/\.md$/, '')}`,
      })
    }
  }
  return groups.filter((g) => g.items.length > 0)
}

// ---------------------------------------------------------------------------
// 翻页校验：页面底部「上一篇 / 下一篇」必须与 index.md 的顺序一致。
//   - 第 1 页的上一篇指向 section 的 index.md
//   - 中间页严格指向相邻页
//   - 最后一页的下一篇允许指向其他 section（「继续阅读」）
//   - 没有翻页行的页面（如 dev/ 迁移页）跳过
// 不一致时直接抛错，让 docs:build 失败，防止两处顺序悄然分叉。
// ---------------------------------------------------------------------------

function relLink(fromFile: string, toFile: string): string {
  return path.relative(path.dirname(fromFile), toFile).split(path.sep).join('/')
}

function absOf(section: string, link: string): string {
  return path.join(docRoot, ...link.replace(/^\//, '').split('/')) + '.md'
}

function validateFooterNav(section: string, groups: SidebarGroup[]): void {
  const pages = groups.flatMap((g) => g.items)
  pages.forEach((page, i) => {
    const file = absOf(section, page.link)
    if (!fs.existsSync(file)) {
      throw new Error(`[docs] sidebar 条目指向的文件不存在: ${file}（检查 ${section}/index.md）`)
    }
    const content = fs.readFileSync(file, 'utf-8')
    const prevM = content.match(/(?:上一篇|Previous)：\[[^\]]+\]\(([^)]+)\)/)
    const nextM = content.match(/(?:下一篇|Next)：\[[^\]]+\]\(([^)]+)\)/)
    if (!prevM && !nextM) return

    const prev = pages[i - 1]
    const next = pages[i + 1]
    if (prevM) {
      const expect = prev
        ? relLink(file, absOf(section, prev.link))
        : relLink(file, path.join(docRoot, ...section.split('/'), 'index.md'))
      if (prevM[1] !== expect) {
        throw new Error(
          `[docs] ${file} 的上一篇链接是 "${prevM[1]}"，但按 ${section}/index.md 顺序应为 "${expect}"`,
        )
      }
    }
    if (nextM && next) {
      const expect = relLink(file, absOf(section, next.link))
      if (nextM[1] !== expect) {
        throw new Error(
          `[docs] ${file} 的下一篇链接是 "${nextM[1]}"，但按 ${section}/index.md 顺序应为 "${expect}"`,
        )
      }
    }
  })
}

function buildSidebar(lang: 'cn' | 'en') {
  const guide = parseSectionIndex(`${lang}/guide`)
  const api = parseSectionIndex(`${lang}/api`)
  const dev = parseSectionIndex(`${lang}/dev`)
  validateFooterNav(`${lang}/guide`, guide)
  validateFooterNav(`${lang}/api`, api)
  validateFooterNav(`${lang}/dev`, dev)
  return {
    [`/${lang}/guide/`]: guide,
    [`/${lang}/api/`]: api,
    [`/${lang}/dev/`]: dev,
    [`/${lang}/`]: [...guide, ...api, ...dev],
  }
}

function buildNav(lang: 'cn' | 'en'): DefaultTheme.NavItem[] {
  const t =
    lang === 'cn'
      ? { guide: '使用指南', api: 'API 参考', dev: '开发文档' }
      : { guide: 'User Guide', api: 'API Reference', dev: 'Developer Docs' }
  return [
    { text: t.guide, link: `/${lang}/guide/`, activeMatch: `/${lang}/guide/` },
    { text: t.api, link: `/${lang}/api/`, activeMatch: `/${lang}/api/` },
    { text: t.dev, link: `/${lang}/dev/`, activeMatch: `/${lang}/dev/` },
  ]
}

// ---------------------------------------------------------------------------
// 站点配置：双前缀 locales（/cn/、/en/），源文件相对链接与 GitHub 行为一致。
// ---------------------------------------------------------------------------

export default defineConfig({
  base: '/langchain4j-aideepin/',
  title: 'AIDeepIn',
  description: 'AIDeepIn 使用指南、API 参考与开发文档',

  head: [['link', { rel: 'icon', href: '/langchain4j-aideepin/image/adi-gallery.png' }]],

  themeConfig: {
    search: {
      provider: 'local',
      options: {
        locales: {
          'zh-CN': {
            translations: {
              button: { buttonText: '搜索文档', buttonAriaLabel: '搜索文档' },
              modal: {
                noResultsText: '无法找到相关结果',
                resetButtonTitle: '清除查询条件',
                displayDetails: '显示详细列表',
                footer: { selectText: '选择', navigateText: '切换', closeText: '关闭' },
              },
            },
          },
        },
      },
    },
  },

  locales: {
    root: {
      label: '简体中文',
      lang: 'zh-CN',
      themeConfig: { nav: buildNav('cn') },
    },
    cn: {
      label: '简体中文',
      lang: 'zh-CN',
      themeConfig: {
        nav: buildNav('cn'),
        sidebar: buildSidebar('cn'),
        outline: { label: '本页目录', level: [2, 4] },
        docFooter: { prev: '上一篇', next: '下一篇' },
        returnToTopLabel: '回到顶部',
        darkModeSwitchLabel: '主题',
        lightModeSwitchTitle: '切换到浅色模式',
        darkModeSwitchTitle: '切换到深色模式',
      },
    },
    en: {
      label: 'English',
      lang: 'en-US',
      themeConfig: {
        nav: buildNav('en'),
        sidebar: buildSidebar('en'),
        outline: { label: 'On this page', level: [2, 4] },
        returnToTopLabel: 'Back to top',
      },
    },
  },
})
