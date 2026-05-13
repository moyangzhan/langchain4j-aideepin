<script setup lang='ts'>
import { onMounted, ref } from 'vue'
import { NResult, NSpace } from 'naive-ui'
import { useRoute, useRouter } from 'vue-router'

const router = useRouter()
const route = useRoute()
const activeUserSuccess = ref<boolean>(true)
const activeUserMsg = ref<string>('')
const countdown = ref<number>(5)

const active = route.query.active as string
if (active === 'success') {
  activeUserSuccess.value = true
  activeUserMsg.value = route.query.msg as string
} else if (active === 'fail') {
  activeUserSuccess.value = false
  const failMsg = route.query.msg as string
  if (failMsg)
    activeUserMsg.value = failMsg
  else
    activeUserMsg.value = '失败，将重新操作'
}
const intervalId = setInterval(() => {
  if (countdown.value < 1) {
    clearInterval(intervalId)
    router.push('/')
  } else {
    countdown.value--
  }
}, 1000)
onMounted(() => {
  console.log('user Active onMounted')
})
</script>

<template>
  <NSpace justify="center" class="h-full items-center">
    <NResult
      class="m-0 p-0" :status="activeUserSuccess ? 'success' : 'error'"
      :title="activeUserSuccess ? '成功' : '失败'" :description="`${activeUserMsg}，倒计时：${countdown}秒`"
    />
  </NSpace>
</template>
