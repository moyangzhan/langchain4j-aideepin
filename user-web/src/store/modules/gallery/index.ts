import { defineStore } from 'pinia'
import { changeFileUrlToUuid } from '@/utils/functions'

export const useGalleryStore = defineStore('gallery-store', {
  state: (): Chat.GalleryState => {
    return {
      loadingUuid: '',
      loading: false,
      publicDraws: [],
      myStarDraws: [],
    }
  },

  getters: {

  },

  actions: {

    appendPublicDraws(draws: Chat.Draw[]) {
      if (draws.length === 0)
        return
      draws.forEach((item) => {
        if (this.publicDraws.findIndex(pd => pd.uuid === item.uuid) === -1) {
          // item.imageUrls = item.imageUuids.map(uuid => `/draw/public/thumbnail/${item.uuid}/${uuid}`)
          for (let i = 0; i < item.imageUrls.length; i++) {
            const url = item.imageUrls[i]
            if (!url.includes('http'))
              item.imageUrls[i] = `/api/draw/public/thumbnail/${item.uuid}/${changeFileUrlToUuid(url)}`
          }
          this.publicDraws.push(item)
        }
      })
    },

    setPublic(draw: Chat.Draw) {
      const hit = this.publicDraws.find(item => item.uuid === draw.uuid)
      if (hit)
        hit.isPublic = draw.isPublic
      else
        this.publicDraws.unshift(draw)
    },

    appendStarDraws(draws: Chat.Draw[]) {
      if (draws.length === 0)
        return

      const needAdd: Chat.Draw[] = []
      draws.forEach((item) => {
        // item.imageUrls = item.imageUuids.map(uuid => `/draw/public/thumbnail/${item.uuid}/${uuid}`)
        for (let i = 0; i < item.imageUrls.length; i++) {
          const url = item.imageUrls[i]
          if (!url.includes('http'))
            item.imageUrls[i] = `/api/draw/public/thumbnail/${item.uuid}/${changeFileUrlToUuid(url)}`
        }
        item.isStar = true

        const hit = this.publicDraws.find(pd => pd.uuid === item.uuid)
        if (hit)
          hit.isStar = true

        const myStarExist = this.myStarDraws.find(pd => pd.uuid === item.uuid)
        if (!myStarExist)
          needAdd.push(item)
      })
      this.myStarDraws.push(...needAdd)
    },

    setLoading(loading: boolean) {
      this.loading = loading
    },

    setLoadingUuid(uuid: string) {
      this.loadingUuid = uuid
    },

    unStarDraw(draw: Chat.Draw) {
      const index = this.myStarDraws.findIndex(item => item.uuid === draw.uuid)
      if (index > -1)
        this.myStarDraws.splice(index, 1)

      const hit = this.publicDraws.find(pd => pd.uuid === draw.uuid)
      if (hit)
        Object.assign(hit, draw)
    },

    starDraw(draw: Chat.Draw) {
      // draw.imageUrls = draw.imageUuids.map(uuid => `/draw/public/thumbnail/${draw.uuid}/${uuid}`)
      for (let i = 0; i < draw.imageUrls.length; i++) {
        const url = draw.imageUrls[i]
        if (!url.includes('http'))
          draw.imageUrls[i] = `/api/draw/public/thumbnail/${draw.uuid}/${changeFileUrlToUuid(url)}`
      }
      const hit = this.myStarDraws.find(pd => pd.uuid === draw.uuid)
      if (hit)
        Object.assign(hit, draw)
      else
        this.myStarDraws.unshift(draw)

      const hit2 = this.publicDraws.find(pd => pd.uuid === draw.uuid)
      if (hit2)
        Object.assign(hit2, draw)
    },

  },
})
