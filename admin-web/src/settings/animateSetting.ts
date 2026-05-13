import { t } from '@/locales'

export function getAnimates() {
  return [
    { value: 'zoom-fade', label: t('setting.animateZoomFade') },
    { value: 'zoom-out', label: t('setting.animateZoomOut') },
    { value: 'fade-slide', label: t('setting.animateFadeSlide') },
    { value: 'fade', label: t('setting.animateFade') },
    { value: 'fade-bottom', label: t('setting.animateFadeBottom') },
    { value: 'fade-scale', label: t('setting.animateFadeScale') },
  ]
}
