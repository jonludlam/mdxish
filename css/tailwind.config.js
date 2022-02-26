module.exports = {
  content : [
    "../src/jsapp/*.ml",
    "../lib/*.ml",
    "../index.html"
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      typography: {
        DEFAULT: {
          css: {
            'code::before': {
              content: '""'
            },
            'code::after': {
              content: '""'
            }
          }
        }
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [ require('@tailwindcss/typography') ]
}
