export const TOKEN_ESTIMATOR = [
  { label: 'Openai', value: 'openai' },
  { label: '通义千问', value: 'qwen' },
  { label: 'Huggingface', value: 'huggingface' },
]
export const OPENAI_IMAGE_SIZE_OPTIONS = [
  { label: '方(1024x1024)', value: '1024x1024' },
  { label: '竖屏(1024x1536)', value: '1024x1536' },
  { label: '横屏(1536x1024)', value: '1536x1024' },
]
export const OPENAI_IMAGE_QUALITY_OPTIONS = [
  { label: '低', value: 'low' },
  { label: '中', value: 'medium' },
  { label: '高', value: 'high' },
]
export const TONGYI_WANX_SIZE_OPTIONS = [
  { label: '方(1024x1024)', value: '1024*1024' },
  { label: '竖屏(720x1280)', value: '720*1280' },
  { label: '横屏(1280X720)', value: '1280*720' },
]
export const RESPONSE_SHOW_TYPE_OPTIONS = [
  { label: '自动', value: 1 },
  { label: '文本', value: 2 },
  { label: '音频(自动播放)', value: 3 },
  { label: '音频(不自动播放)', value: 4 },
]
export const CHAT_MESSAGE_CONTENT_TYPE = {
  auto: 1, // 自动
  text: 2, // 文本
  audio: 3, // 音频
}
/**
 * 音频合成位置：客户端(web)或服务端
 */
export const AUDIO_SYNTHESIZER_SIDE = {
  client: 'client',
  server: 'server',
}
export const GOOGLE_LANGUAGE_OPTIONS = [
  {
    language_code: 'af',
    language_name: 'Afrikaans',
  },
  {
    language_code: 'ak',
    language_name: 'Akan',
  },
  {
    language_code: 'sq',
    language_name: 'Albanian',
  },
  {
    language_code: 'ws',
    language_name: 'Samoa',
  },
  {
    language_code: 'am',
    language_name: 'Amharic',
  },
  {
    language_code: 'ar',
    language_name: 'Arabic',
  },
  {
    language_code: 'hy',
    language_name: 'Armenian',
  },
  {
    language_code: 'az',
    language_name: 'Azerbaijani',
  },
  {
    language_code: 'eu',
    language_name: 'Basque',
  },
  {
    language_code: 'be',
    language_name: 'Belarusian',
  },
  {
    language_code: 'bem',
    language_name: 'Bemba',
  },
  {
    language_code: 'bn',
    language_name: 'Bengali',
  },
  {
    language_code: 'bh',
    language_name: 'Bihari',
  },
  {
    language_code: 'xx-bork',
    language_name: 'Bork, bork, bork!',
  },
  {
    language_code: 'bs',
    language_name: 'Bosnian',
  },
  {
    language_code: 'br',
    language_name: 'Breton',
  },
  {
    language_code: 'bg',
    language_name: 'Bulgarian',
  },
  {
    language_code: 'bt',
    language_name: 'Bhutanese',
  },
  {
    language_code: 'km',
    language_name: 'Cambodian',
  },
  {
    language_code: 'ca',
    language_name: 'Catalan',
  },
  {
    language_code: 'chr',
    language_name: 'Cherokee',
  },
  {
    language_code: 'ny',
    language_name: 'Chichewa',
  },
  {
    language_code: 'zh-cn',
    language_name: 'Chinese (Simplified)',
  },
  {
    language_code: 'zh-tw',
    language_name: 'Chinese (Traditional)',
  },
  {
    language_code: 'co',
    language_name: 'Corsican',
  },
  {
    language_code: 'hr',
    language_name: 'Croatian',
  },
  {
    language_code: 'cs',
    language_name: 'Czech',
  },
  {
    language_code: 'da',
    language_name: 'Danish',
  },
  {
    language_code: 'nl',
    language_name: 'Dutch',
  },
  {
    language_code: 'xx-elmer',
    language_name: 'Elmer Fudd',
  },
  {
    language_code: 'en',
    language_name: 'English',
  },
  {
    language_code: 'eo',
    language_name: 'Esperanto',
  },
  {
    language_code: 'et',
    language_name: 'Estonian',
  },
  {
    language_code: 'ee',
    language_name: 'Ewe',
  },
  {
    language_code: 'fo',
    language_name: 'Faroese',
  },
  {
    language_code: 'tl',
    language_name: 'Filipino',
  },
  {
    language_code: 'fi',
    language_name: 'Finnish',
  },
  {
    language_code: 'fr',
    language_name: 'French',
  },
  {
    language_code: 'fy',
    language_name: 'Frisian',
  },
  {
    language_code: 'gaa',
    language_name: 'Ga',
  },
  {
    language_code: 'gl',
    language_name: 'Galician',
  },
  {
    language_code: 'ka',
    language_name: 'Georgian',
  },
  {
    language_code: 'de',
    language_name: 'German',
  },
  {
    language_code: 'el',
    language_name: 'Greek',
  },
  {
    language_code: 'kl',
    language_name: 'Greenlandic',
  },
  {
    language_code: 'gn',
    language_name: 'Guarani',
  },
  {
    language_code: 'gu',
    language_name: 'Gujarati',
  },
  {
    language_code: 'xx-hacker',
    language_name: 'Hacker',
  },
  {
    language_code: 'ht',
    language_name: 'Haitian Creole',
  },
  {
    language_code: 'ha',
    language_name: 'Hausa',
  },
  {
    language_code: 'haw',
    language_name: 'Hawaiian',
  },
  {
    language_code: 'iw',
    language_name: 'Hebrew',
  },
  {
    language_code: 'hi',
    language_name: 'Hindi',
  },
  {
    language_code: 'hu',
    language_name: 'Hungarian',
  },
  {
    language_code: 'is',
    language_name: 'Icelandic',
  },
  {
    language_code: 'ig',
    language_name: 'Igbo',
  },
  {
    language_code: 'id',
    language_name: 'Indonesian',
  },
  {
    language_code: 'ia',
    language_name: 'Interlingua',
  },
  {
    language_code: 'ga',
    language_name: 'Irish',
  },
  {
    language_code: 'it',
    language_name: 'Italian',
  },
  {
    language_code: 'ja',
    language_name: 'Japanese',
  },
  {
    language_code: 'jw',
    language_name: 'Javanese',
  },
  {
    language_code: 'kn',
    language_name: 'Kannada',
  },
  {
    language_code: 'kk',
    language_name: 'Kazakh',
  },
  {
    language_code: 'rw',
    language_name: 'Kinyarwanda',
  },
  {
    language_code: 'rn',
    language_name: 'Kirundi',
  },
  {
    language_code: 'xx-klingon',
    language_name: 'Klingon',
  },
  {
    language_code: 'kg',
    language_name: 'Kongo',
  },
  {
    language_code: 'ko',
    language_name: 'Korean',
  },
  {
    language_code: 'kri',
    language_name: 'Krio (Sierra Leone)',
  },
  {
    language_code: 'ku',
    language_name: 'Kurdish',
  },
  {
    language_code: 'ckb',
    language_name: 'Kurdish (Soranî)',
  },
  {
    language_code: 'ky',
    language_name: 'Kyrgyz',
  },
  {
    language_code: 'lo',
    language_name: 'Laothian',
  },
  {
    language_code: 'la',
    language_name: 'Latin',
  },
  {
    language_code: 'lv',
    language_name: 'Latvian',
  },
  {
    language_code: 'ln',
    language_name: 'Lingala',
  },
  {
    language_code: 'lt',
    language_name: 'Lithuanian',
  },
  {
    language_code: 'loz',
    language_name: 'Lozi',
  },
  {
    language_code: 'lg',
    language_name: 'Luganda',
  },
  {
    language_code: 'ach',
    language_name: 'Luo',
  },
  {
    language_code: 'mk',
    language_name: 'Macedonian',
  },
  {
    language_code: 'mg',
    language_name: 'Malagasy',
  },
  {
    language_code: 'ms',
    language_name: 'Malay',
  },
  {
    language_code: 'ml',
    language_name: 'Malayalam',
  },
  {
    language_code: 'mt',
    language_name: 'Maltese',
  },
  {
    language_code: 'mv',
    language_name: 'Maldives',
  },
  {
    language_code: 'mi',
    language_name: 'Maori',
  },
  {
    language_code: 'mr',
    language_name: 'Marathi',
  },
  {
    language_code: 'mfe',
    language_name: 'Mauritian Creole',
  },
  {
    language_code: 'mo',
    language_name: 'Moldavian',
  },
  {
    language_code: 'mn',
    language_name: 'Mongolian',
  },
  {
    language_code: 'sr-me',
    language_name: 'Montenegrin',
  },
  {
    language_code: 'my',
    language_name: 'Myanmar',
  },
  {
    language_code: 'ne',
    language_name: 'Nepali',
  },
  {
    language_code: 'pcm',
    language_name: 'Nigerian Pidgin',
  },
  {
    language_code: 'nso',
    language_name: 'Northern Sotho',
  },
  {
    language_code: 'no',
    language_name: 'Norwegian',
  },
  {
    language_code: 'nn',
    language_name: 'Norwegian (Nynorsk)',
  },
  {
    language_code: 'oc',
    language_name: 'Occitan',
  },
  {
    language_code: 'or',
    language_name: 'Oriya',
  },
  {
    language_code: 'om',
    language_name: 'Oromo',
  },
  {
    language_code: 'ps',
    language_name: 'Pashto',
  },
  {
    language_code: 'fa',
    language_name: 'Persian',
  },
  {
    language_code: 'xx-pirate',
    language_name: 'Pirate',
  },
  {
    language_code: 'pl',
    language_name: 'Polish',
  },
  {
    language_code: 'pt',
    language_name: 'Portuguese',
  },
  {
    language_code: 'pt-br',
    language_name: 'Portuguese (Brazil)',
  },
  {
    language_code: 'pt-pt',
    language_name: 'Portuguese (Portugal)',
  },
  {
    language_code: 'pa',
    language_name: 'Punjabi',
  },
  {
    language_code: 'qu',
    language_name: 'Quechua',
  },
  {
    language_code: 'ro',
    language_name: 'Romanian',
  },
  {
    language_code: 'rm',
    language_name: 'Romansh',
  },
  {
    language_code: 'nyn',
    language_name: 'Runyakitara',
  },
  {
    language_code: 'ru',
    language_name: 'Russian',
  },
  {
    language_code: 'gd',
    language_name: 'Scots Gaelic',
  },
  {
    language_code: 'sr',
    language_name: 'Serbian',
  },
  {
    language_code: 'sh',
    language_name: 'Serbo-Croatian',
  },
  {
    language_code: 'st',
    language_name: 'Sesotho',
  },
  {
    language_code: 'tn',
    language_name: 'Setswana',
  },
  {
    language_code: 'crs',
    language_name: 'Seychellois Creole',
  },
  {
    language_code: 'sn',
    language_name: 'Shona',
  },
  {
    language_code: 'sd',
    language_name: 'Sindhi',
  },
  {
    language_code: 'si',
    language_name: 'Sinhalese',
  },
  {
    language_code: 'sk',
    language_name: 'Slovak',
  },
  {
    language_code: 'sl',
    language_name: 'Slovenian',
  },
  {
    language_code: 'so',
    language_name: 'Somali',
  },
  {
    language_code: 'es',
    language_name: 'Spanish',
  },
  {
    language_code: 'es-419',
    language_name: 'Spanish (Latin American)',
  },
  {
    language_code: 'su',
    language_name: 'Sundanese',
  },
  {
    language_code: 'sw',
    language_name: 'Swahili',
  },
  {
    language_code: 'sv',
    language_name: 'Swedish',
  },
  {
    language_code: 'tg',
    language_name: 'Tajik',
  },
  {
    language_code: 'ta',
    language_name: 'Tamil',
  },
  {
    language_code: 'tt',
    language_name: 'Tatar',
  },
  {
    language_code: 'te',
    language_name: 'Telugu',
  },
  {
    language_code: 'th',
    language_name: 'Thai',
  },
  {
    language_code: 'ti',
    language_name: 'Tigrinya',
  },
  {
    language_code: 'to',
    language_name: 'Tonga',
  },
  {
    language_code: 'lua',
    language_name: 'Tshiluba',
  },
  {
    language_code: 'tum',
    language_name: 'Tumbuka',
  },
  {
    language_code: 'tr',
    language_name: 'Turkish',
  },
  {
    language_code: 'tk',
    language_name: 'Turkmen',
  },
  {
    language_code: 'tw',
    language_name: 'Twi',
  },
  {
    language_code: 'ug',
    language_name: 'Uighur',
  },
  {
    language_code: 'uk',
    language_name: 'Ukrainian',
  },
  {
    language_code: 'ur',
    language_name: 'Urdu',
  },
  {
    language_code: 'uz',
    language_name: 'Uzbek',
  },
  {
    language_code: 'vu',
    language_name: 'Vanuatu',
  },
  {
    language_code: 'vi',
    language_name: 'Vietnamese',
  },
  {
    language_code: 'cy',
    language_name: 'Welsh',
  },
  {
    language_code: 'wo',
    language_name: 'Wolof',
  },
  {
    language_code: 'xh',
    language_name: 'Xhosa',
  },
  {
    language_code: 'yi',
    language_name: 'Yiddish',
  },
  {
    language_code: 'yo',
    language_name: 'Yoruba',
  },
  {
    language_code: 'zu',
    language_name: 'Zulu',
  },
].map(x => ({ label: x.language_name, value: x.language_code }))

export const GOOGLE_COUNTRY_OPTIONS = [
  {
    country_code: 'af',
    country_name: 'Afghanistan',
  },
  {
    country_code: 'al',
    country_name: 'Albania',
  },
  {
    country_code: 'dz',
    country_name: 'Algeria',
  },
  {
    country_code: 'as',
    country_name: 'American Samoa',
  },
  {
    country_code: 'ad',
    country_name: 'Andorra',
  },
  {
    country_code: 'ao',
    country_name: 'Angola',
  },
  {
    country_code: 'ai',
    country_name: 'Anguilla',
  },
  {
    country_code: 'aq',
    country_name: 'Antarctica',
  },
  {
    country_code: 'ag',
    country_name: 'Antigua and Barbuda',
  },
  {
    country_code: 'ar',
    country_name: 'Argentina',
  },
  {
    country_code: 'am',
    country_name: 'Armenia',
  },
  {
    country_code: 'aw',
    country_name: 'Aruba',
  },
  {
    country_code: 'au',
    country_name: 'Australia',
  },
  {
    country_code: 'at',
    country_name: 'Austria',
  },
  {
    country_code: 'az',
    country_name: 'Azerbaijan',
  },
  {
    country_code: 'bs',
    country_name: 'Bahamas',
  },
  {
    country_code: 'bh',
    country_name: 'Bahrain',
  },
  {
    country_code: 'bd',
    country_name: 'Bangladesh',
  },
  {
    country_code: 'bb',
    country_name: 'Barbados',
  },
  {
    country_code: 'by',
    country_name: 'Belarus',
  },
  {
    country_code: 'be',
    country_name: 'Belgium',
  },
  {
    country_code: 'bz',
    country_name: 'Belize',
  },
  {
    country_code: 'bj',
    country_name: 'Benin',
  },
  {
    country_code: 'bm',
    country_name: 'Bermuda',
  },
  {
    country_code: 'bt',
    country_name: 'Bhutan',
  },
  {
    country_code: 'bo',
    country_name: 'Bolivia',
  },
  {
    country_code: 'ba',
    country_name: 'Bosnia and Herzegovina',
  },
  {
    country_code: 'bw',
    country_name: 'Botswana',
  },
  {
    country_code: 'bv',
    country_name: 'Bouvet Island',
  },
  {
    country_code: 'br',
    country_name: 'Brazil',
  },
  {
    country_code: 'io',
    country_name: 'British Indian Ocean Territory',
  },
  {
    country_code: 'bn',
    country_name: 'Brunei Darussalam',
  },
  {
    country_code: 'bg',
    country_name: 'Bulgaria',
  },
  {
    country_code: 'bf',
    country_name: 'Burkina Faso',
  },
  {
    country_code: 'bi',
    country_name: 'Burundi',
  },
  {
    country_code: 'kh',
    country_name: 'Cambodia',
  },
  {
    country_code: 'cm',
    country_name: 'Cameroon',
  },
  {
    country_code: 'ca',
    country_name: 'Canada',
  },
  {
    country_code: 'cv',
    country_name: 'Cape Verde',
  },
  {
    country_code: 'ky',
    country_name: 'Cayman Islands',
  },
  {
    country_code: 'cf',
    country_name: 'Central African Republic',
  },
  {
    country_code: 'td',
    country_name: 'Chad',
  },
  {
    country_code: 'cl',
    country_name: 'Chile',
  },
  {
    country_code: 'cn',
    country_name: 'China',
  },
  {
    country_code: 'cx',
    country_name: 'Christmas Island',
  },
  {
    country_code: 'cc',
    country_name: 'Cocos (Keeling) Islands',
  },
  {
    country_code: 'co',
    country_name: 'Colombia',
  },
  {
    country_code: 'km',
    country_name: 'Comoros',
  },
  {
    country_code: 'cg',
    country_name: 'Congo',
  },
  {
    country_code: 'cd',
    country_name: 'Congo, the Democratic Republic of the',
  },
  {
    country_code: 'ck',
    country_name: 'Cook Islands',
  },
  {
    country_code: 'cr',
    country_name: 'Costa Rica',
  },
  {
    country_code: 'ci',
    country_name: 'Cote D\'ivoire',
  },
  {
    country_code: 'hr',
    country_name: 'Croatia',
  },
  {
    country_code: 'cu',
    country_name: 'Cuba',
  },
  {
    country_code: 'cy',
    country_name: 'Cyprus',
  },
  {
    country_code: 'cz',
    country_name: 'Czech Republic',
  },
  {
    country_code: 'dk',
    country_name: 'Denmark',
  },
  {
    country_code: 'dj',
    country_name: 'Djibouti',
  },
  {
    country_code: 'dm',
    country_name: 'Dominica',
  },
  {
    country_code: 'do',
    country_name: 'Dominican Republic',
  },
  {
    country_code: 'ec',
    country_name: 'Ecuador',
  },
  {
    country_code: 'eg',
    country_name: 'Egypt',
  },
  {
    country_code: 'sv',
    country_name: 'El Salvador',
  },
  {
    country_code: 'gq',
    country_name: 'Equatorial Guinea',
  },
  {
    country_code: 'er',
    country_name: 'Eritrea',
  },
  {
    country_code: 'ee',
    country_name: 'Estonia',
  },
  {
    country_code: 'et',
    country_name: 'Ethiopia',
  },
  {
    country_code: 'fk',
    country_name: 'Falkland Islands (Malvinas)',
  },
  {
    country_code: 'fo',
    country_name: 'Faroe Islands',
  },
  {
    country_code: 'fj',
    country_name: 'Fiji',
  },
  {
    country_code: 'fi',
    country_name: 'Finland',
  },
  {
    country_code: 'fr',
    country_name: 'France',
  },
  {
    country_code: 'gf',
    country_name: 'French Guiana',
  },
  {
    country_code: 'pf',
    country_name: 'French Polynesia',
  },
  {
    country_code: 'tf',
    country_name: 'French Southern Territories',
  },
  {
    country_code: 'ga',
    country_name: 'Gabon',
  },
  {
    country_code: 'gm',
    country_name: 'Gambia',
  },
  {
    country_code: 'ge',
    country_name: 'Georgia',
  },
  {
    country_code: 'de',
    country_name: 'Germany',
  },
  {
    country_code: 'gh',
    country_name: 'Ghana',
  },
  {
    country_code: 'gi',
    country_name: 'Gibraltar',
  },
  {
    country_code: 'gr',
    country_name: 'Greece',
  },
  {
    country_code: 'gl',
    country_name: 'Greenland',
  },
  {
    country_code: 'gd',
    country_name: 'Grenada',
  },
  {
    country_code: 'gp',
    country_name: 'Guadeloupe',
  },
  {
    country_code: 'gu',
    country_name: 'Guam',
  },
  {
    country_code: 'gt',
    country_name: 'Guatemala',
  },
  {
    country_code: 'gn',
    country_name: 'Guinea',
  },
  {
    country_code: 'gw',
    country_name: 'Guinea-Bissau',
  },
  {
    country_code: 'gy',
    country_name: 'Guyana',
  },
  {
    country_code: 'ht',
    country_name: 'Haiti',
  },
  {
    country_code: 'hm',
    country_name: 'Heard Island and Mcdonald Islands',
  },
  {
    country_code: 'va',
    country_name: 'Holy See (Vatican City State)',
  },
  {
    country_code: 'hn',
    country_name: 'Honduras',
  },
  {
    country_code: 'hk',
    country_name: 'Hong Kong',
  },
  {
    country_code: 'hu',
    country_name: 'Hungary',
  },
  {
    country_code: 'is',
    country_name: 'Iceland',
  },
  {
    country_code: 'in',
    country_name: 'India',
  },
  {
    country_code: 'id',
    country_name: 'Indonesia',
  },
  {
    country_code: 'ir',
    country_name: 'Iran, Islamic Republic of',
  },
  {
    country_code: 'iq',
    country_name: 'Iraq',
  },
  {
    country_code: 'ie',
    country_name: 'Ireland',
  },
  {
    country_code: 'il',
    country_name: 'Israel',
  },
  {
    country_code: 'it',
    country_name: 'Italy',
  },
  {
    country_code: 'jm',
    country_name: 'Jamaica',
  },
  {
    country_code: 'jp',
    country_name: 'Japan',
  },
  {
    country_code: 'jo',
    country_name: 'Jordan',
  },
  {
    country_code: 'kz',
    country_name: 'Kazakhstan',
  },
  {
    country_code: 'ke',
    country_name: 'Kenya',
  },
  {
    country_code: 'ki',
    country_name: 'Kiribati',
  },
  {
    country_code: 'kp',
    country_name: 'Korea, Democratic People\'s Republic of',
  },
  {
    country_code: 'kr',
    country_name: 'Korea, Republic of',
  },
  {
    country_code: 'kw',
    country_name: 'Kuwait',
  },
  {
    country_code: 'kg',
    country_name: 'Kyrgyzstan',
  },
  {
    country_code: 'la',
    country_name: 'Lao People\'s Democratic Republic',
  },
  {
    country_code: 'lv',
    country_name: 'Latvia',
  },
  {
    country_code: 'lb',
    country_name: 'Lebanon',
  },
  {
    country_code: 'ls',
    country_name: 'Lesotho',
  },
  {
    country_code: 'lr',
    country_name: 'Liberia',
  },
  {
    country_code: 'ly',
    country_name: 'Libyan Arab Jamahiriya',
  },
  {
    country_code: 'li',
    country_name: 'Liechtenstein',
  },
  {
    country_code: 'lt',
    country_name: 'Lithuania',
  },
  {
    country_code: 'lu',
    country_name: 'Luxembourg',
  },
  {
    country_code: 'mo',
    country_name: 'Macao',
  },
  {
    country_code: 'mk',
    country_name: 'Macedonia, the Former Yugosalv Republic of',
  },
  {
    country_code: 'mg',
    country_name: 'Madagascar',
  },
  {
    country_code: 'mw',
    country_name: 'Malawi',
  },
  {
    country_code: 'my',
    country_name: 'Malaysia',
  },
  {
    country_code: 'mv',
    country_name: 'Maldives',
  },
  {
    country_code: 'ml',
    country_name: 'Mali',
  },
  {
    country_code: 'mt',
    country_name: 'Malta',
  },
  {
    country_code: 'mh',
    country_name: 'Marshall Islands',
  },
  {
    country_code: 'mq',
    country_name: 'Martinique',
  },
  {
    country_code: 'mr',
    country_name: 'Mauritania',
  },
  {
    country_code: 'mu',
    country_name: 'Mauritius',
  },
  {
    country_code: 'yt',
    country_name: 'Mayotte',
  },
  {
    country_code: 'mx',
    country_name: 'Mexico',
  },
  {
    country_code: 'fm',
    country_name: 'Micronesia, Federated States of',
  },
  {
    country_code: 'md',
    country_name: 'Moldova, Republic of',
  },
  {
    country_code: 'mc',
    country_name: 'Monaco',
  },
  {
    country_code: 'mn',
    country_name: 'Mongolia',
  },
  {
    country_code: 'ms',
    country_name: 'Montserrat',
  },
  {
    country_code: 'ma',
    country_name: 'Morocco',
  },
  {
    country_code: 'mz',
    country_name: 'Mozambique',
  },
  {
    country_code: 'mm',
    country_name: 'Myanmar',
  },
  {
    country_code: 'na',
    country_name: 'Namibia',
  },
  {
    country_code: 'nr',
    country_name: 'Nauru',
  },
  {
    country_code: 'np',
    country_name: 'Nepal',
  },
  {
    country_code: 'nl',
    country_name: 'Netherlands',
  },
  {
    country_code: 'an',
    country_name: 'Netherlands Antilles',
  },
  {
    country_code: 'nc',
    country_name: 'New Caledonia',
  },
  {
    country_code: 'nz',
    country_name: 'New Zealand',
  },
  {
    country_code: 'ni',
    country_name: 'Nicaragua',
  },
  {
    country_code: 'ne',
    country_name: 'Niger',
  },
  {
    country_code: 'ng',
    country_name: 'Nigeria',
  },
  {
    country_code: 'nu',
    country_name: 'Niue',
  },
  {
    country_code: 'nf',
    country_name: 'Norfolk Island',
  },
  {
    country_code: 'mp',
    country_name: 'Northern Mariana Islands',
  },
  {
    country_code: 'no',
    country_name: 'Norway',
  },
  {
    country_code: 'om',
    country_name: 'Oman',
  },
  {
    country_code: 'pk',
    country_name: 'Pakistan',
  },
  {
    country_code: 'pw',
    country_name: 'Palau',
  },
  {
    country_code: 'ps',
    country_name: 'Palestinian Territory, Occupied',
  },
  {
    country_code: 'pa',
    country_name: 'Panama',
  },
  {
    country_code: 'pg',
    country_name: 'Papua New Guinea',
  },
  {
    country_code: 'py',
    country_name: 'Paraguay',
  },
  {
    country_code: 'pe',
    country_name: 'Peru',
  },
  {
    country_code: 'ph',
    country_name: 'Philippines',
  },
  {
    country_code: 'pn',
    country_name: 'Pitcairn',
  },
  {
    country_code: 'pl',
    country_name: 'Poland',
  },
  {
    country_code: 'pt',
    country_name: 'Portugal',
  },
  {
    country_code: 'pr',
    country_name: 'Puerto Rico',
  },
  {
    country_code: 'qa',
    country_name: 'Qatar',
  },
  {
    country_code: 're',
    country_name: 'Reunion',
  },
  {
    country_code: 'ro',
    country_name: 'Romania',
  },
  {
    country_code: 'ru',
    country_name: 'Russian Federation',
  },
  {
    country_code: 'rw',
    country_name: 'Rwanda',
  },
  {
    country_code: 'sh',
    country_name: 'Saint Helena',
  },
  {
    country_code: 'kn',
    country_name: 'Saint Kitts and Nevis',
  },
  {
    country_code: 'lc',
    country_name: 'Saint Lucia',
  },
  {
    country_code: 'pm',
    country_name: 'Saint Pierre and Miquelon',
  },
  {
    country_code: 'vc',
    country_name: 'Saint Vincent and the Grenadines',
  },
  {
    country_code: 'ws',
    country_name: 'Samoa',
  },
  {
    country_code: 'sm',
    country_name: 'San Marino',
  },
  {
    country_code: 'st',
    country_name: 'Sao Tome and Principe',
  },
  {
    country_code: 'sa',
    country_name: 'Saudi Arabia',
  },
  {
    country_code: 'sn',
    country_name: 'Senegal',
  },
  {
    country_code: 'rs',
    country_name: 'Serbia and Montenegro',
  },
  {
    country_code: 'sc',
    country_name: 'Seychelles',
  },
  {
    country_code: 'sl',
    country_name: 'Sierra Leone',
  },
  {
    country_code: 'sg',
    country_name: 'Singapore',
  },
  {
    country_code: 'sk',
    country_name: 'Slovakia',
  },
  {
    country_code: 'si',
    country_name: 'Slovenia',
  },
  {
    country_code: 'sb',
    country_name: 'Solomon Islands',
  },
  {
    country_code: 'so',
    country_name: 'Somalia',
  },
  {
    country_code: 'za',
    country_name: 'South Africa',
  },
  {
    country_code: 'gs',
    country_name: 'South Georgia and the South Sandwich Islands',
  },
  {
    country_code: 'es',
    country_name: 'Spain',
  },
  {
    country_code: 'lk',
    country_name: 'Sri Lanka',
  },
  {
    country_code: 'sd',
    country_name: 'Sudan',
  },
  {
    country_code: 'sr',
    country_name: 'Suriname',
  },
  {
    country_code: 'sj',
    country_name: 'Svalbard and Jan Mayen',
  },
  {
    country_code: 'sz',
    country_name: 'Swaziland',
  },
  {
    country_code: 'se',
    country_name: 'Sweden',
  },
  {
    country_code: 'ch',
    country_name: 'Switzerland',
  },
  {
    country_code: 'sy',
    country_name: 'Syrian Arab Republic',
  },
  {
    country_code: 'tw',
    country_name: 'Taiwan, Province of China',
  },
  {
    country_code: 'tj',
    country_name: 'Tajikistan',
  },
  {
    country_code: 'tz',
    country_name: 'Tanzania, United Republic of',
  },
  {
    country_code: 'th',
    country_name: 'Thailand',
  },
  {
    country_code: 'tl',
    country_name: 'Timor-Leste',
  },
  {
    country_code: 'tg',
    country_name: 'Togo',
  },
  {
    country_code: 'tk',
    country_name: 'Tokelau',
  },
  {
    country_code: 'to',
    country_name: 'Tonga',
  },
  {
    country_code: 'tt',
    country_name: 'Trinidad and Tobago',
  },
  {
    country_code: 'tn',
    country_name: 'Tunisia',
  },
  {
    country_code: 'tr',
    country_name: 'Turkiye',
  },
  {
    country_code: 'tm',
    country_name: 'Turkmenistan',
  },
  {
    country_code: 'tc',
    country_name: 'Turks and Caicos Islands',
  },
  {
    country_code: 'tv',
    country_name: 'Tuvalu',
  },
  {
    country_code: 'ug',
    country_name: 'Uganda',
  },
  {
    country_code: 'ua',
    country_name: 'Ukraine',
  },
  {
    country_code: 'ae',
    country_name: 'United Arab Emirates',
  },
  {
    country_code: 'uk',
    country_name: 'United Kingdom',
  },
  {
    country_code: 'gb',
    country_name: 'United Kingdom',
  },
  {
    country_code: 'us',
    country_name: 'United States',
  },
  {
    country_code: 'um',
    country_name: 'United States Minor Outlying Islands',
  },
  {
    country_code: 'uy',
    country_name: 'Uruguay',
  },
  {
    country_code: 'uz',
    country_name: 'Uzbekistan',
  },
  {
    country_code: 'vu',
    country_name: 'Vanuatu',
  },
  {
    country_code: 've',
    country_name: 'Venezuela',
  },
  {
    country_code: 'vn',
    country_name: 'Viet Nam',
  },
  {
    country_code: 'vg',
    country_name: 'Virgin Islands, British',
  },
  {
    country_code: 'vi',
    country_name: 'Virgin Islands, U.S.',
  },
  {
    country_code: 'wf',
    country_name: 'Wallis and Futuna',
  },
  {
    country_code: 'eh',
    country_name: 'Western Sahara',
  },
  {
    country_code: 'ye',
    country_name: 'Yemen',
  },
  {
    country_code: 'zm',
    country_name: 'Zambia',
  },
  {
    country_code: 'zw',
    country_name: 'Zimbabwe',
  },
].map(x => ({ label: x.country_name, value: x.country_code }))
