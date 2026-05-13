<template>
  <div class="page-footer">
    <div class="page-footer-link">
      <a href="https://github.com/jekip/naive-ui-admin" target="_blank">
        {{ t('footer.officialSite') }}
      </a>
      <a href="https://github.com/jekip/naive-ui-admin" target="_blank">
        {{ t('footer.community') }}
      </a>
      <a href="https://github.com/jekip/naive-ui-admin/issues" target="_blank">
        {{ t('footer.communication') }}
      </a>
    </div>
    <div class="copyright">naive-ui-admin 1.4 · Made by Ah jung</div>
  </div>
</template>

<script>
  import { t } from '@/locales'
  export default {
    name: 'PageFooter',
    components: {},
    props: {
      collapsed: {
        type: Boolean,
      },
    },
  }
</script>

<style lang="less" scoped>
  .page-footer {
    //margin: 28px 0 24px 0;
    padding: 0 16px;
    text-align: center;

    a {
      font-size: 14px;
      color: #808695;
      -webkit-transition: all 0.2s ease-in-out;
      transition: all 0.2s ease-in-out;

      &:hover {
        color: #515a6e;
      }
    }

    &-link {
      display: flex;
      justify-content: center;
      margin-bottom: 8px;

      a:not(:last-child) {
        margin-right: 40px;
      }
    }

    .copyright {
      color: #808695;
      font-size: 14px;
    }
  }
</style>
