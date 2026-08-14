import type { Ref } from 'vue'
import { nextTick, ref } from 'vue'

type ScrollElement = HTMLDivElement | null

interface ScrollReturn {
  scrollRef: Ref<ScrollElement>
  isAtBottom: Ref<boolean>
  scrollTo: (top: number) => Promise<void>
  scrollToBottom: (smooth?: boolean) => Promise<void>
  scrollToTop: () => Promise<void>
  scrollToBottomIfAtBottom: () => Promise<void>
  checkAtBottom: () => void
}

export function useScroll(): ScrollReturn {
  const scrollRef = ref<ScrollElement>(null)
  const isAtBottom = ref(true)
  const threshold = 100 // 阈值，表示滚动条到底部的距离阈值 | Distance from the bottom below which the view is considered "at bottom"

  // 根据当前滚动位置刷新 isAtBottom，供组件在 @scroll 处理函数中调用 | Refresh isAtBottom from the current scroll position; call it from the component's @scroll handler
  const checkAtBottom = () => {
    const el = scrollRef.value
    if (!el)
      return
    isAtBottom.value = el.scrollHeight - el.scrollTop - el.clientHeight <= threshold
  }

  const scrollTo = async (top: number) => {
    await nextTick()
    if (scrollRef.value)
      scrollRef.value.scrollTop = top
  }

  const scrollToBottom = async (smooth = false) => {
    await nextTick()
    if (scrollRef.value) {
      if (smooth)
        scrollRef.value.scrollTo({ top: scrollRef.value.scrollHeight, behavior: 'smooth' })
      else
        scrollRef.value.scrollTop = scrollRef.value.scrollHeight
      isAtBottom.value = true
    }
  }

  const scrollToTop = async () => {
    await nextTick()
    if (scrollRef.value)
      scrollRef.value.scrollTop = 0
  }

  const scrollToBottomIfAtBottom = async () => {
    await nextTick()
    if (scrollRef.value) {
      const distanceToBottom = scrollRef.value.scrollHeight - scrollRef.value.scrollTop - scrollRef.value.clientHeight
      if (distanceToBottom <= threshold) {
        scrollRef.value.scrollTop = scrollRef.value.scrollHeight
        isAtBottom.value = true
      }
    }
  }

  return {
    scrollRef,
    isAtBottom,
    scrollTo,
    scrollToBottom,
    scrollToTop,
    scrollToBottomIfAtBottom,
    checkAtBottom,
  }
}
