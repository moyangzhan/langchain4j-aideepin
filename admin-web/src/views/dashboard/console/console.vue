<template>
  <div class="console">
    <!--数据卡片-->
    <n-grid cols="1 s:2 m:3 xl:4 2xl:5" responsive="screen" :x-gap="12" :y-gap="8">
      <n-grid-item>
        <NCard
          :title="t('dashboard.newUsers')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="userStatistic.todayCreated > 0"
              :startVal="0"
              :endVal="userStatistic.todayCreated"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ userStatistic.todayCreated }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" text :repeat="2" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.totalUsers') }}</div>
                <div class="text-sn">
                  <CountTo :startVal="0" :endVal="userStatistic.totalNormal" />
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
      <n-grid-item>
        <NCard
          :title="t('dashboard.newKnowledgeBases')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="kbStatistic.kbTodayCreated > 0"
              :startVal="0"
              :endVal="kbStatistic.kbTodayCreated"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ kbStatistic.kbTodayCreated }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" :width="100" size="medium" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.totalCount') }}</div>
                <div class="text-sn">
                  <CountTo
                    v-if="kbStatistic.kbTotal > 0"
                    :startVal="0"
                    :endVal="kbStatistic.kbTotal"
                  />
                  <div v-else class="text-sn">
                    {{ kbStatistic.kbTotal }}
                  </div>
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
      <n-grid-item>
        <NCard
          :title="t('dashboard.newKnowledgePoints')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="kbStatistic.itemTodayCreated > 0"
              :startVal="0"
              :endVal="kbStatistic.itemTodayCreated"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ kbStatistic.itemTodayCreated }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" :width="100" size="medium" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.totalCount') }}</div>
                <div class="text-sn">
                  <CountTo
                    v-if="kbStatistic.itemTotal > 0"
                    :startVal="0"
                    :endVal="kbStatistic.itemTotal"
                  />
                  <div v-else class="text-sn">
                    {{ kbStatistic.itemTotal }}
                  </div>
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
      <n-grid-item>
        <NCard
          :title="t('dashboard.conversationCount')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="convStatistic.todayCreated > 0"
              :startVal="0"
              :endVal="convStatistic.todayCreated"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ convStatistic.todayCreated }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" :width="100" size="medium" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.totalCount') }}</div>
                <div class="text-sn">
                  <CountTo
                    v-if="convStatistic.total > 0"
                    :startVal="0"
                    :endVal="Number(convStatistic.total)"
                  />
                  <div v-else class="text-sn">
                    {{ convStatistic.total }}
                  </div>
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
      <n-grid-item>
        <NCard
          :title="t('dashboard.tokenConsumption')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="tokenCostStatistic.todayTokenCost > 0"
              :startVal="0"
              :endVal="tokenCostStatistic.todayTokenCost"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ tokenCostStatistic.todayTokenCost }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" :width="100" size="medium" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.monthlyConsumption') }}</div>
                <div class="text-sn">
                  <CountTo
                    v-if="tokenCostStatistic.monthTokenCost > 0"
                    :startVal="0"
                    :endVal="Number(tokenCostStatistic.monthTokenCost)"
                  />
                  <div v-else class="text-sn">
                    {{ tokenCostStatistic.monthTokenCost }}
                  </div>
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
      <n-grid-item>
        <NCard
          :title="t('dashboard.imageConsumption')"
          :segmented="{ content: true, footer: true }"
          size="small"
          :bordered="false"
        >
          <template #header-extra>
            <n-tag type="success">{{ t('dashboard.day') }}</n-tag>
          </template>
          <div class="flex justify-between px-1 py-1">
            <n-skeleton v-if="loading" :width="100" size="medium" />
            <CountTo
              v-else-if="imageCostStatistic.todayCost > 0"
              :startVal="0"
              :endVal="imageCostStatistic.todayCost"
              class="text-3xl"
            />
            <div v-else class="text-3xl">
              {{ imageCostStatistic.todayCost }}
            </div>
          </div>
          <template #footer>
            <div class="flex justify-between">
              <n-skeleton v-if="loading" :width="100" size="medium" />
              <template v-else>
                <div class="text-sn">{{ t('dashboard.monthlyConsumption') }}</div>
                <div class="text-sn">
                  <CountTo
                    v-if="imageCostStatistic.monthCost > 0"
                    :startVal="0"
                    :endVal="Number(imageCostStatistic.monthCost)"
                  />
                  <div v-else class="text-sn">
                    {{ imageCostStatistic.monthCost }}
                  </div>
                </div>
              </template>
            </div>
          </template>
        </NCard>
      </n-grid-item>
    </n-grid>
  </div>
</template>
<script lang="ts" setup>
  import { ref, onMounted } from 'vue'
  import { getStatistic } from '@/api/dashboard/console'
  import { CountTo } from '@/components/CountTo/index'
  import { t } from '@/locales'

  interface UserStatistic {
    todayCreated: number
    todayActivated: number
    totalNormal: number
  }

  interface KbStatistic {
    kbTodayCreated: number
    itemTodayCreated: number
    kbTotal: number
    itemTotal: number
  }

  interface TokenCostStatistic {
    todayTokenCost: number
    monthTokenCost: number
  }

  interface ImageCostStatistic {
    todayCost: number
    monthCost: number
  }

  interface ConvStatistic {
    todayCreated: number
    total: number
  }

  const loading = ref(true)
  const userStatistic = ref({} as UserStatistic)
  const kbStatistic = ref({} as KbStatistic)
  const tokenCostStatistic = ref({} as TokenCostStatistic)
  const imageCostStatistic = ref({} as ImageCostStatistic)
  const convStatistic = ref({} as ConvStatistic)

  onMounted(async () => {
    const { data: resp } = await getStatistic()
    userStatistic.value = resp.userStatistic
    kbStatistic.value = resp.kbStatistic
    tokenCostStatistic.value = resp.tokenCostStatistic
    imageCostStatistic.value = resp.imageCostStatistic
    convStatistic.value = resp.convStatistic
    loading.value = false
  })
</script>

<style lang="less" scoped></style>
