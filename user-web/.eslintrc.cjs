module.exports = {
  root: true,
  rules: {
    'no-console': process.env.NODE_ENV === 'production' ? 'error' : 'off',
    'no-debugger': process.env.NODE_ENV === 'production' ? 'error' : 'off',
    '@typescript-eslint/brace-style': ['error', '1tbs', { allowSingleLine: true }],
  },
  extends: ['@antfu'],
}
