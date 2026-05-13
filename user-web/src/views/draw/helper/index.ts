import api from '@/api'
import { useDrawStore } from '@/store'

const drawStore = useDrawStore()
const initLoadingDate = new Date(1970, 1, 1)
let beginLoading: Date = new Date(1970, 1, 1)

export async function checkProcess(uuid: string) {
  if (beginLoading.getTime() !== initLoadingDate.getTime() && new Date().getTime() - beginLoading.getTime() * 1000 > 25 * 1000) {
    drawStore.setLoading(false)
    drawStore.setLoadingUuid('')
    beginLoading = initLoadingDate
    return
  }
  beginLoading = new Date()
  const imageResp = await api.fetchDraw<Chat.Draw>(uuid)
  if (imageResp.success) {
    drawStore.updateDraw(imageResp.data)
    if (imageResp.data.processStatus === 1) {
      setTimeout(() => {
        checkProcess(uuid)
      }, 3000)
    } else {
      drawStore.setLoading(false)
      drawStore.setLoadingUuid('')
      if (imageResp.data.processStatus === 2)
        window.$message && window.$message.error(imageResp.data.processStatusRemark)
    }
  }
}
