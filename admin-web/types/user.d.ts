export type UserInfoType = {
  uuid: string
  name: string
  email: string
}
export interface IUserState {
  token: string
  name: string
  welcome: string
  avatar: string
  permissions: any[]
  info: UserInfoType
}
export type UserListParams = {
  uuid: string
  name: string
  email: string
}