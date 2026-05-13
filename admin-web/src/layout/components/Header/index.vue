<template>
  <div class="layout-header">
    <!--顶部菜单-->
    <div
      class="layout-header-left"
      v-if="navMode === 'horizontal' || (navMode === 'horizontal-mix' && mixMenu)"
    >
      <div class="logo" v-if="navMode === 'horizontal'">
        <img :src="websiteConfig.logo" alt="" />
        <h2 v-show="!collapsed" class="title">{{ websiteConfig.title }}</h2>
      </div>
      <AsideMenu
        :collapsed="collapsed"
        v-model:location="getMenuLocation"
        :inverted="getInverted"
        mode="horizontal"
      />
    </div>
    <!--左侧菜单-->
    <div class="layout-header-left" v-else>
      <!-- 菜单收起 -->
      <div
        class="ml-1 layout-header-trigger layout-header-trigger-min"
        @click="handleMenuCollapsed"
      >
        <n-icon size="18" v-if="collapsed">
          <MenuUnfoldOutlined />
        </n-icon>
        <n-icon size="18" v-else>
          <MenuFoldOutlined />
        </n-icon>
      </div>
      <!-- 刷新 -->
      <div
        class="mr-1 layout-header-trigger layout-header-trigger-min"
        v-if="headerSetting.isReload"
        @click="reloadPage"
      >
        <n-icon size="18">
          <ReloadOutlined />
        </n-icon>
      </div>
      <!-- 面包屑 -->
      <n-breadcrumb v-if="crumbsSetting.show">
        <template
          v-for="routeItem in breadcrumbList"
          :key="routeItem.name === 'Redirect' ? void 0 : routeItem.name"
        >
          <n-breadcrumb-item v-if="routeItem.meta.title">
            <n-dropdown
              v-if="routeItem.children.length"
              :options="routeItem.children"
              @select="dropdownSelect"
            >
              <span class="link-text">
                <component
                  v-if="crumbsSetting.showIcon && routeItem.meta.icon"
                  :is="routeItem.meta.icon"
                />
                {{ t(routeItem.meta.title) }}
              </span>
            </n-dropdown>
            <span class="link-text" v-else>
              <component
                v-if="crumbsSetting.showIcon && routeItem.meta.icon"
                :is="routeItem.meta.icon"
              />
              {{ t(routeItem.meta.title) }}
            </span>
          </n-breadcrumb-item>
        </template>
      </n-breadcrumb>
    </div>
    <div class="layout-header-right">
      <!-- <div
        class="layout-header-trigger layout-header-trigger-min"
        v-for="item in iconList"
        :key="item.icon"
      >
        <n-tooltip placement="bottom">
          <template #trigger>
            <n-icon size="18">
              <component :is="item.icon" v-on="item.eventObject || {}" />
            </n-icon>
          </template>
          <span>{{ item.tips }}</span>
        </n-tooltip>
      </div> -->
      <!--切换语言-->
      <div class="layout-header-trigger layout-header-trigger-min layout-header-text-btn mr-3">
        <n-dropdown trigger="hover" :options="langOptions" @select="handleLangSelect">
          <span class="cursor-pointer">{{ locale === 'zh-CN' ? '简体中文' : 'ENGLISH' }}</span>
        </n-dropdown>
      </div>
      <!--切换全屏-->
      <!-- <div class="layout-header-trigger layout-header-trigger-min">
        <n-tooltip placement="bottom">
          <template #trigger>
            <n-icon size="18">
              <component :is="fullscreenIcon" @click="toggleFullScreen" />
            </n-icon>
          </template>
          <span>{{ t('setting.fullscreen') }}</span>
        </n-tooltip>
      </div> -->
      <!-- 退出登录 -->
      <div
        class="layout-header-trigger layout-header-trigger-min layout-header-text-btn"
        @click="doLogout"
      >
        <span style="display: inline-flex; align-items: center; gap: 4px">
          {{ username }}
          <n-icon size="16"><LogoutOutlined /></n-icon>
        </span>
      </div>
      <!--设置-->
      <!-- <div class="layout-header-trigger layout-header-trigger-min" @click="openSetting">
        <n-tooltip placement="bottom-end">
          <template #trigger>
            <n-icon size="18" style="font-weight: bold">
              <SettingOutlined />
            </n-icon>
          </template>
          <span>{{ t('setting.projectConfig') }}</span>
        </n-tooltip>
      </div> -->
    </div>
  </div>
  <!--项目配置-->
  <ProjectSetting ref="drawerSetting" />
</template>

<script lang="ts">
  import { defineComponent, reactive, toRefs, ref, computed, unref } from 'vue'
  import { useRouter, useRoute } from 'vue-router'
  import components from './components'
  import { NDialogProvider, useDialog, useMessage } from 'naive-ui'
  import { TABS_ROUTES } from '@/store/mutation-types'
  import { useUserStore } from '@/store/modules/user'
  import ProjectSetting from './ProjectSetting.vue'
  import { AsideMenu } from '@/layout/components/Menu'
  import { useProjectSetting } from '@/hooks/setting/useProjectSetting'
  import { websiteConfig } from '@/config/website.config'
  import { t, type Locale } from '@/locales'
  import { useLocale } from '@/hooks/useLocale'
  import { GlobalOutlined } from '@vicons/antd'

  export default defineComponent({
    name: 'PageHeader',
    components: { ...components, NDialogProvider, ProjectSetting, AsideMenu, GlobalOutlined },
    props: {
      collapsed: {
        type: Boolean,
      },
      inverted: {
        type: Boolean,
      },
    },
    emits: ['update:collapsed'],
    setup(props, { emit }) {
      const userStore = useUserStore()
      const message = useMessage()
      const dialog = useDialog()
      const { navMode, navTheme, headerSetting, menuSetting, crumbsSetting } = useProjectSetting()
      const { locale, changeLocale } = useLocale()

      const drawerSetting = ref()

      const state = reactive({
        username: userStore?.info?.name ?? '',
        fullscreenIcon: 'FullscreenOutlined',
        navMode,
        navTheme,
        headerSetting,
        crumbsSetting,
      })

      const getInverted = computed(() => {
        return ['light', 'header-dark'].includes(unref(navTheme)) ? props.inverted : !props.inverted
      })

      const mixMenu = computed(() => {
        return unref(menuSetting).mixMenu
      })

      const getChangeStyle = computed(() => {
        const { collapsed } = props
        const { minMenuWidth, menuWidth } = unref(menuSetting)
        return {
          left: collapsed ? `${minMenuWidth}px` : `${menuWidth}px`,
          width: `calc(100% - ${collapsed ? `${minMenuWidth}px` : `${menuWidth}px`})`,
        }
      })

      const getMenuLocation = computed(() => {
        return 'header'
      })

      const router = useRouter()
      const route = useRoute()

      const generator: any = (routerMap) => {
        return routerMap.map((item) => {
          const currentMenu = {
            ...item,
            label: t(item.meta.title),
            key: item.name,
            disabled: item.path === '/',
          }
          // 是否有子菜单，并递归处理
          if (item.children && item.children.length > 0) {
            // Recursion
            currentMenu.children = generator(item.children, currentMenu)
          }
          return currentMenu
        })
      }

      const breadcrumbList = computed(() => {
        return generator(route.matched)
      })

      const dropdownSelect = (key) => {
        router.push({ name: key })
      }

      // 刷新页面
      const reloadPage = () => {
        router.push({
          path: '/redirect' + unref(route).fullPath,
        })
      }

      // 退出登录
      const doLogout = () => {
        dialog.info({
          title: t('common.tip'),
          content: t('login.logoutConfirmContent'),
          positiveText: t('common.positiveText'),
          negativeText: t('common.negativeText'),
          onPositiveClick: () => {
            userStore.logout().then(() => {
              message.success(t('login.logoutSuccess'))
              // 移除标签页
              localStorage.removeItem(TABS_ROUTES)
              router
                .replace({
                  name: 'Login',
                  query: {
                    redirect: route.fullPath,
                  },
                })
                .finally(() => location.reload())
            })
          },
          onNegativeClick: () => {},
        })
      }

      // 切换全屏图标
      const toggleFullscreenIcon = () =>
        (state.fullscreenIcon =
          document.fullscreenElement !== null ? 'FullscreenExitOutlined' : 'FullscreenOutlined')

      // 监听全屏切换事件
      document.addEventListener('fullscreenchange', toggleFullscreenIcon)

      // 全屏切换
      const toggleFullScreen = () => {
        if (!document.fullscreenElement) {
          document.documentElement.requestFullscreen()
        } else {
          if (document.exitFullscreen) {
            document.exitFullscreen()
          }
        }
      }

      // 图标列表
      // const iconList = [
      //   {
      //     icon: 'SearchOutlined',
      //     tips: '搜索',
      //   },
      // ];
      const avatarOptions = [
        {
          label: t('login.logout'),
          key: 2,
        },
      ]

      const langOptions = [
        { label: '简体中文', key: 'zh-CN' },
        { label: 'English', key: 'en-US' },
      ]

      const handleLangSelect = (key: string) => {
        changeLocale(key as Locale)
      }

      //头像下拉菜单
      const avatarSelect = (key) => {
        switch (key) {
          case 1:
            router.push({ name: 'Setting' })
            break
          case 2:
            doLogout()
            break
        }
      }

      function openSetting() {
        const { openDrawer } = drawerSetting.value
        openDrawer()
      }

      function handleMenuCollapsed() {
        emit('update:collapsed', !props.collapsed)
      }

      return {
        ...toRefs(state),
        // iconList,
        toggleFullScreen,
        doLogout,
        route,
        dropdownSelect,
        avatarOptions,
        getChangeStyle,
        avatarSelect,
        breadcrumbList,
        reloadPage,
        drawerSetting,
        openSetting,
        getInverted,
        getMenuLocation,
        mixMenu,
        websiteConfig,
        handleMenuCollapsed,
        langOptions,
        handleLangSelect,
        t,
        locale,
      }
    },
  })
</script>

<style lang="less" scoped>
  .layout-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0;
    height: 64px;
    box-shadow: 0 1px 4px rgb(0 21 41 / 8%);
    transition: all 0.2s ease-in-out;
    width: 100%;
    z-index: 11;

    &-left {
      display: flex;
      align-items: center;

      .logo {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 64px;
        line-height: 64px;
        overflow: hidden;
        white-space: nowrap;
        padding-left: 10px;

        img {
          width: auto;
          height: 32px;
          margin-right: 10px;
        }

        .title {
          margin-bottom: 0;
        }
      }

      ::v-deep(.ant-breadcrumb span:last-child .link-text) {
        color: #515a6e;
      }

      .n-breadcrumb {
        display: inline-block;
      }

      &-menu {
        color: var(--text-color);
      }
    }

    &-right {
      display: flex;
      align-items: center;
      margin-right: 20px;

      .avatar {
        display: flex;
        align-items: center;
        height: 64px;
      }

      > * {
        cursor: pointer;
      }
    }

    &-trigger {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      width: 64px;
      height: 64px;
      cursor: pointer;
      transition: all 0.2s ease-in-out;

      .n-icon {
        display: flex;
        align-items: center;
        height: 64px;
      }

      &:hover {
        background: hsla(0, 0%, 100%, 0.08);
      }

      .anticon {
        font-size: 16px;
        color: #515a6e;
      }
    }

    &-trigger-min {
      width: auto;
      padding: 0 12px;
    }

    &-text-btn {
      height: 36px;
      border: 1px solid #e0e0e0;
      padding: 0 16px;
      font-size: 14px;
      color: #515a6e;
      background: #fff;
      white-space: nowrap;

      &:hover {
        border-color: #36ad6a;
        color: #36ad6a;
      }
    }
  }

  .layout-header-light {
    background: #fff;
    color: #515a6e;

    .n-icon {
      color: #515a6e;
    }

    .layout-header-left {
      ::v-deep(.n-breadcrumb .n-breadcrumb-item:last-child .n-breadcrumb-item__link) {
        color: #515a6e;
      }
    }

    .layout-header-trigger {
      &:hover {
        background: #f8f8f9;
      }
    }
  }

  .layout-header-fix {
    position: fixed;
    top: 0;
    right: 0;
    left: 200px;
    z-index: 11;
  }

  //::v-deep(.menu-router-link) {
  //  color: #515a6e;
  //
  //  &:hover {
  //    color: #1890ff;
  //  }
  //}
</style>
