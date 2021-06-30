import * as xhr from 'common/xhr';

export const assetUrl = (path: string, opts: AssetUrlOpts = {}) => {
  opts = opts || {};
  const baseUrl = opts.sameDomain ? '' : document.body.getAttribute('data-asset-url'),
    version = opts.version || document.body.getAttribute('data-asset-version');
  return baseUrl + '/assets' + (opts.noVersion ? '' : '/_' + version) + '/' + path;
};

const loadedCss = new Map<string, true>();
export const loadCss = (url: string) => {
  if (!loadedCss.has(url)) {
    loadedCss.set(url, true);
    const el = document.createElement('link');
    el.rel = 'stylesheet';
    el.href = assetUrl(url);
    document.head.append(el);
  }
};

export const loadCssPath = (key: string) => loadCss(`css/${key}.${$('body').data('dev') ? 'dev' : 'min'}.css`);

export const jsModule = (name: string) => `compiled/${name}${$('body').data('dev') ? '' : '.min'}.js`;

const loadedScript = new Map<string, Promise<void>>();
export const loadScript = (url: string, opts: AssetUrlOpts = {}): Promise<void> => {
  if (!loadedScript.has(url)) loadedScript.set(url, xhr.script(assetUrl(url, opts)));
  return loadedScript.get(url)!;
};

export const loadModule = (name: string): Promise<void> => loadScript(jsModule(name));

export const userComplete = (): Promise<UserComplete> => {
  loadCssPath('complete');
  return loadModule('userComplete').then(_ => window.UserComplete);
};

export const hopscotch = () => {
  loadCss('vendor/hopscotch/dist/css/hopscotch.min.css');
  return loadScript('vendor/hopscotch/dist/js/hopscotch.min.js', {
    noVersion: true,
  });
};
