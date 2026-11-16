// Apply the saved choice before the stylesheet paints the page.
(() => {
  let theme = 'light';
  try {
    const saved = localStorage.getItem('podcast-web-theme');
    if (saved === 'light' || saved === 'dark') theme = saved;
  } catch {}
  document.documentElement.dataset.theme = theme;
})();
