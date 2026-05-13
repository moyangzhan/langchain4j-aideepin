declare namespace User {
    interface Profile {
        avatar?: string
        name?: string
        description?: string
        uuid: string
      }

    interface UserQuota {
        tokenByDay: number
        tokenByMonth: number
        requestTimesByDay: number
        requestTimesByMonth: number
        drawByDay: number
        drawByMonth: number
    }

    interface QuotaCost {
        freeTokenCost: {
            todayTokenCost: number
            monthTokenCost: number
        },
        freeRequestTimes: {
            todayRequestTimes: number
            monthRequestTimes: number
        },
        freeDrawTimes: {
            todayDrawTimes: number
            monthDrawTimes: number
        },
        paidTokenCost: {
            todayTokenCost: number
            monthTokenCost: number
        },
        paidRequestTimes: {
            todayRequestTimes: number
            monthRequestTimes: number
        },
        paidDrawTimes: {
            todayDrawTimes: number
            monthDrawTimes: number
        }
    }

    interface Config{
        secretKey?: string
        contextEnable?: boolean
        contextMsgPairNum?: number
        locale?: string
        userQuota: UserQuota
        quotaCost: QuotaCost
    }
}
