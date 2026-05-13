<script setup lang='ts'>
import { computed, onMounted, ref, watch } from 'vue'
import { NModal, NTabPane, NTabs } from 'naive-ui'
import General from './General.vue'
import Quota from './Quota.vue'
import ModifyPassword from './ModifyPassword.vue'
import api from '@/api'
import { SvgIcon } from '@/components/common'
import { emptyQuota } from '@/utils/functions'

interface Props {
  visible: boolean
}

interface Emit {
  (e: 'update:visible', visible: boolean): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emit>()
const active = ref('General')
const loading = ref(false)
const userConfig = ref<User.Config>(emptyQuota())

const show = computed({
  get() {
    return props.visible
  },
  set(visible: boolean) {
    emit('update:visible', visible)
  },
})

watch(
  active,
  (val) => {
    if (val === 'Quota')
      fetchConfig()
  },
)

async function fetchConfig() {
  try {
    loading.value = true
    const { data } = await api.fetchUserConfig<User.Config>()
    userConfig.value = data
  } finally {
    loading.value = false
  }
}

onMounted(() => {
  fetchConfig()
})
</script>

<template>
  <NModal v-model:show="show" :auto-focus="false" preset="card" style="width: 95%; max-width: 640px">
    <div>
      <NTabs v-model:value="active" type="line" animated>
        <NTabPane name="General" tab="General">
          <template #tab>
            <SvgIcon class="text-lg" icon="ri:file-user-line" />
            <span class="ml-2">{{ $t('setting.general') }}</span>
          </template>
          <div class="min-h-[100px]">
            <General />
          </div>
        </NTabPane>
        <NTabPane name="Quota" tab="Quota">
          <template #tab>
            <SvgIcon class="text-lg" icon="eos-icons:quota-outlined" />
            <span class="ml-2">{{ $t('setting.quota') }}</span>
          </template>
          <Quota :user-config="userConfig" @reloadConfig="fetchConfig" />
        </NTabPane>
        <NTabPane name="ModifyPassword" tab="ModifyPassword">
          <template #tab>
            <SvgIcon class="text-lg" icon="carbon:password" />
            <span class="ml-2">{{ $t('setting.modifyPassword') }}</span>
          </template>
          <ModifyPassword />
        </NTabPane>
      </NTabs>
    </div>
  </NModal>
</template>
