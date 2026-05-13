<script lang="ts" setup>
import { computed } from 'vue'
import { NAvatar } from 'naive-ui'
import { useUserStore } from '@/store'
import { isString } from '@/utils/is'
import defaultAvatar from '@/assets/avatar.jpg'
import ollamaAvatar from '@/assets/ollama.png'
import baiduchatgpt from '@/assets/baiduchatgpt.png'
import tongyiqianwen from '@/assets/tongyiqianwen.png'
import deepseekAvatar from '@/assets/deepseek.png'
import openai from '@/assets/openai.png'
import siliconflow from '@/assets/siliconflow.svg'

interface Props {
  name?: string
  imageSize?: number
}
defineProps<Props>()

const userStore = useUserStore()

const avatar = computed(() => userStore.userInfo.avatar)
</script>

<template>
  <!-- User's avatar -->
  <template v-if="name === 'user'">
    <NAvatar
      v-if="isString(avatar) && avatar.length > 0"
      :size="imageSize ? imageSize : 32"
      :src="avatar"
      :fallback-src="defaultAvatar"
      :style="{ backgroundColor: 'transparent' }"
    />
    <NAvatar
      v-else
      round
      :size="imageSize ? imageSize : 32"
      :src="defaultAvatar"
      style="background-color: transparent"
    />
  </template>
  <!-- Model platform's avatar -->
  <NAvatar
    v-else-if="name === 'deepseek'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="deepseekAvatar"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name === 'dashscope'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="tongyiqianwen"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name === 'qianfan'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="baiduchatgpt"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name === 'ollama'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="ollamaAvatar"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name === 'openai'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="openai"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name === 'siliconflow'"
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="siliconflow"
    :style="{ backgroundColor: 'transparent' }"
  />
  <NAvatar
    v-else-if="name"
    round
    bordered
    :size="imageSize ? imageSize : 32"
    :style="{
      color: 'gray',
      border: 'solid 1px gray',
      backgroundColor: 'transparent',
    }"
  >
    {{ name.substring(0, 2) }}
  </NAvatar>
  <NAvatar
    v-else
    object-fit="contain"
    round
    :size="imageSize ? imageSize : 32"
    :src="openai"
    :style="{ backgroundColor: 'transparent' }"
  />
</template>
