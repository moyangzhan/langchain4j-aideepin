<script setup lang='ts'>
import { NCollapse, NCollapseItem, NTable } from 'naive-ui'
import { t } from '@/locales'
</script>

<template>
  <div class="border dark:border-gray-700 rounded-lg p-4">
    <h4 class="font-medium mb-3">
      {{ t('extApi.docCommonParams') }}
    </h4>
    <NCollapse>
      <NCollapseItem :title="t('extApi.docRequest')" name="request">
        <NTable :bordered="true" :single-line="false" size="small">
          <tbody>
            <tr>
              <td style="width: 150px; font-weight: 600">
                Authorization
              </td>
              <td>Bearer YOUR_API_KEY</td>
            </tr>
            <tr>
              <td style="font-weight: 600">
                Content-Type
              </td>
              <td>application/json</td>
            </tr>
          </tbody>
        </NTable>
      </NCollapseItem>
      <NCollapseItem :title="t('extApi.docResponseStructure')" name="response-structure">
        <p class="text-sm text-gray-500 mb-2">
          {{ t('extApi.docResponseStructureDesc') }}
        </p>
        <NTable :bordered="true" :single-line="false" size="small" style="margin-bottom: 8px">
          <thead>
            <tr>
              <th style="min-width: 120px">
                {{ t('extApi.docParamName') }}
              </th>
              <th style="min-width: 60px">
                Type
              </th>
              <th>{{ t('extApi.docParamDesc') }}</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td>success</td>
              <td>boolean</td>
              <td>{{ t('extApi.docResponseSuccess') }}</td>
            </tr>
            <tr>
              <td>code</td>
              <td>string</td>
              <td>{{ t('extApi.docResponseCode') }}</td>
            </tr>
            <tr>
              <td>message</td>
              <td>string</td>
              <td>{{ t('extApi.docResponseMessage') }}</td>
            </tr>
            <tr>
              <td>data</td>
              <td>object</td>
              <td>{{ t('extApi.docResponseData') }}</td>
            </tr>
          </tbody>
        </NTable>
        <pre class="text-xs whitespace-pre-wrap break-all bg-gray-100 dark:bg-gray-800 rounded p-3">{
  "success": true,
  "code": "success",
  "message": "",
  "data": { ... }
}</pre>
      </NCollapseItem>
    </NCollapse>
  </div>
</template>
