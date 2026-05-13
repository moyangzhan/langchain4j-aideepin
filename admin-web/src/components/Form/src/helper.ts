import { ComponentType } from './types/index'
import { t } from '@/locales'

/**
 * @description: 生成placeholder
 */
export function createPlaceholderMessage(component: ComponentType) {
  if (component === 'NInput') return t('form.pleaseInput')
  if (
    ['NPicker', 'NSelect', 'NCheckbox', 'NRadio', 'NSwitch', 'NDatePicker', 'NTimePicker'].includes(
      component
    )
  )
    return t('form.pleaseSelect')
  return ''
}

const DATE_TYPE = ['DatePicker', 'MonthPicker', 'WeekPicker', 'TimePicker']

function genType() {
  return [...DATE_TYPE, 'RangePicker']
}

/**
 * 时间字段
 */
export const dateItemType = genType()

export function defaultType(component) {
  if (component === 'NInput') return ''
  if (component === 'NInputNumber') return null
  return [
    'NPicker',
    'NSelect',
    'NCheckbox',
    'NRadio',
    'NSwitch',
    'NDatePicker',
    'NTimePicker',
  ].includes(component)
    ? ''
    : undefined
}
