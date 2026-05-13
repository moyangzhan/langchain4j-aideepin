import { ss } from '@/utils/storage'

const LOCAL_NAME = 'userStorage'

export interface UserState {
  userInfo: User.Profile
}

export function defaultSetting(): UserState {
  return {
    userInfo: {
      avatar: '/api/user/avatar/1',
      name: 'aideepin',
      description: '',
      uuid: '',
    },
  }
}

export function getLocalState(): UserState {
  const localSetting: UserState | undefined = ss.get(LOCAL_NAME)
  return { ...defaultSetting(), ...localSetting }
}

export function setLocalState(setting: UserState): void {
  ss.set(LOCAL_NAME, setting)
}
