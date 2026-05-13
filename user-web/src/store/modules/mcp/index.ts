import { defineStore } from 'pinia'

export const useMcpStore = defineStore('mcp-store', {
  state: (): Mcp.McpState => {
    return {
      loading: false,
      userMcpLoading: false,
      myUserMcpList: [],
    }
  },

  getters: {
  },

  actions: {

    appendMyUserMcpList(userMcpList: Mcp.UserMcp[]) {
      if (userMcpList.length === 0)
        return
      userMcpList.forEach((item) => {
        if (this.myUserMcpList.findIndex(userMcp => userMcp.uuid === item.uuid) === -1) {
          item.mcpInfo.customizedParamDefinitions.forEach((paramDefine) => {
            // 将已设置好的参数赋值给mcp的customizedParamDefinition.value
            item.mcpCustomizedParams.forEach((customizedParam) => {
              paramDefine.value = customizedParam.value
              paramDefine.enctrypted = customizedParam.enctrypted
            })
          })
          this.myUserMcpList.push(item)
        }
      })
    },

    setLoading(loading: boolean) {
      this.loading = loading
    },

    setUserMcpLoading(loading: boolean) {
      this.userMcpLoading = loading
    },
  },
})
