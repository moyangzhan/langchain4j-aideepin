import { defineConfig, type DefaultTheme } from 'vitepress'
import fs from 'node:fs'
import path from 'node:path'

// ---------------------------------------------------------------------------
// Sidebar 自动生成：解析各 section 的 index.md
//   - H2 标题 → sidebar 分组
//   - 有序列表项 `1. [标题](相对路径) 说明` → 分组内页面
// 各语言（cn / en）× 各 section（guide / api / dev / deployment）各调用一次；
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
  const deployment = parseSectionIndex(`${lang}/deployment`)
  validateFooterNav(`${lang}/guide`, guide)
  validateFooterNav(`${lang}/api`, api)
  validateFooterNav(`${lang}/dev`, dev)
  validateFooterNav(`${lang}/deployment`, deployment)
  return {
    [`/${lang}/guide/`]: guide,
    [`/${lang}/api/`]: api,
    [`/${lang}/dev/`]: dev,
    [`/${lang}/deployment/`]: deployment,
    [`/${lang}/`]: [...guide, ...api, ...dev, ...deployment],
  }
}

function buildNav(lang: 'cn' | 'en'): DefaultTheme.NavItem[] {
  const t =
    lang === 'cn'
      ? { guide: '使用指南', api: 'API 参考', dev: '开发文档', deployment: '部署' }
      : { guide: 'User Guide', api: 'API Reference', dev: 'Developer Docs', deployment: 'Deployment' }
  return [
    { text: t.guide, link: `/${lang}/guide/`, activeMatch: `/${lang}/guide/` },
    { text: t.api, link: `/${lang}/api/`, activeMatch: `/${lang}/api/` },
    { text: t.dev, link: `/${lang}/dev/`, activeMatch: `/${lang}/dev/` },
    { text: t.deployment, link: `/${lang}/deployment/`, activeMatch: `/${lang}/deployment/` },
  ]
}

// ---------------------------------------------------------------------------
// 锚点 slug 与 GitHub 对齐：默认 slugify 会给数字开头的标题加下划线前缀
// （"## 1. 初始化数据库" → "_1-初始化数据库"），与 GitHub 生成的
// "1-初始化数据库" 不一致，导致源文件里 GitHub 风格的锚点链接在站点上失效。
// ---------------------------------------------------------------------------
const githubSlugify = (s: string): string =>
  s
    .trim()
    .toLowerCase()
    .replace(/[^\p{L}\p{N} \-_]/gu, '')
    .replace(/ /g, '-')

// ---------------------------------------------------------------------------
// 锚点校验：所有 #anchor 链接必须命中目标标题（slug 规则与 githubSlugify
// 一致，即站点实际生成的 id）。VitePress 死链检测只查页面不查锚点，锚点
// 写错不会构建失败、链接静默落到页顶；这里在构建期比对全部锚点链接，
// 不一致直接抛错，防止死链进站。
// ---------------------------------------------------------------------------
function validateAnchorLinks(): void {
  const mdFiles: string[] = []
  ;(function walk(dir: string) {
    for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
      if (e.name.startsWith('.') || e.name === 'node_modules') continue
      const p = path.join(dir, e.name)
      if (e.isDirectory()) walk(p)
      else if (e.name.endsWith('.md')) mdFiles.push(p)
    }
  })(docRoot)

  const headingsCache = new Map<string, Set<string>>()
  const headingsOf = (file: string): Set<string> => {
    let set = headingsCache.get(file)
    if (!set) {
      set = new Set()
      for (const m of fs.readFileSync(file, 'utf8').matchAll(/^#{1,6}\s+(.+)$/gm)) {
        set.add(githubSlugify(m[1].replace(/[`*]/g, '').trim()))
      }
      headingsCache.set(file, set)
    }
    return set
  }

  const bad: string[] = []
  for (const file of mdFiles) {
    const content = fs.readFileSync(file, 'utf8')
    for (const m of content.matchAll(/\]\(([^)#\s]+)?#([^)\s]+)\)/g)) {
      const target = m[1] ?? ''
      if (/^https?:/.test(target)) continue
      const targetFile = target ? path.resolve(path.dirname(file), target) : file
      const rel = path.relative(docRoot, file)
      if (!fs.existsSync(targetFile)) {
        bad.push(`${rel}: 链接目标文件不存在 ${target}`)
        continue
      }
      if (!headingsOf(targetFile).has(m[2])) {
        bad.push(`${rel}: #${m[2]} -> ${target || '(本页)'} 无对应标题`)
      }
    }
  }
  if (bad.length > 0) {
    throw new Error(`[docs] 锚点链接校验失败:\n${bad.join('\n')}`)
  }
}
validateAnchorLinks()

// ---------------------------------------------------------------------------
// 站点配置：双前缀 locales（/cn/、/en/），源文件相对链接与 GitHub 行为一致。
// ---------------------------------------------------------------------------

export default defineConfig({
  base: '/langchain4j-aideepin/',
  title: 'AIDeepIn',
  description: 'AIDeepIn 使用指南、API 参考、开发文档与部署指南',

  head: [['link', { rel: 'icon', href: '/langchain4j-aideepin/image/adi-gallery.png' }]],

  markdown: {
    anchor: { slugify: githubSlugify },
    toc: { slugify: githubSlugify },
  },

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
