import { ComponentType } from '../../types/componentType'
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
