'use strict';
(() => {
  const button = document.getElementById('install-app');
  const help = document.getElementById('install-help');
  const standalone = window.matchMedia('(display-mode: standalone)');
  let installPrompt = null;
  const isInstalled = () => standalone.matches || navigator.standalone === true;
  function updateButton() {
    button.hidden = !window.isSecureContext || isInstalled();
    if (isInstalled()) help.hidden = true;
  }
  window.addEventListener('beforeinstallprompt', event => {
    event.preventDefault();
    installPrompt = event;
    updateButton();
  });
  window.addEventListener('appinstalled', () => {
    installPrompt = null;
    button.hidden = true;
    help.hidden = true;
  });
  standalone.addEventListener('change', updateButton);
  button.addEventListener('click', async () => {
    if (!installPrompt) {
      help.hidden = false;
      help.open = true;
      return;
    }
    const prompt = installPrompt;
    installPrompt = null;
    button.disabled = true;
    try {
      await prompt.prompt();
      const choice = await prompt.userChoice;
      if (choice.outcome !== 'accepted') {
        help.hidden = false;
        help.open = true;
      }
    } catch {
      help.hidden = false;
      help.open = true;
    } finally {
      button.disabled = false;
    }
  });
  updateButton();

  const syncThemeColor = () => {
    document.querySelector('meta[name="theme-color"]').content =
      document.documentElement.dataset.theme === 'dark' ? '#111a1a' : '#f7f8f3';
  };
  syncThemeColor();
  new MutationObserver(syncThemeColor).observe(document.documentElement,
    {attributes: true, attributeFilter: ['data-theme']});

  // Only navigation has an offline fallback. API requests, audio and mutations
  // use the network normally, keeping the existing login boundary intact.
  if ('serviceWorker' in navigator && window.isSecureContext) {
    navigator.serviceWorker.register('./sw.js', {scope: './', updateViaCache: 'none'})
      .catch(error => console.warn('Podcast offline screen could not be registered.', error));
  }
})();
