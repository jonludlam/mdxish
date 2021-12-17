module.exports = {
  content : [
    "../src/*.ml",
    "../lib/*.ml",
    "../index.html"
  ],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [    require('@tailwindcss/typography')]
}
