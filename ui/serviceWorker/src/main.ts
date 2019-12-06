const searchParams = new URL(self.location.href).searchParams;
const isDev = searchParams.has('dev');
const assetBase = new URL(searchParams.get('asset-url')!, self.location.href).href;
const assetVersion = self.location.pathname.split('/')[2];

function assetUrl(path: string): string {
  return `${assetBase}assets/${path}`;
}

function versionedAssetUrl(path: string): string {
  return assetUrl(`${assetVersion}/${path}`);
}

function compiledScript(name: string): string {
  return `compiled/lichess.${name}${isDev ? '' : '.min'}.js`;
}

function themedStylesheets(name: string): string[] {
  return ['light', 'dark', 'transp'].map(theme => {
    return `css/${name}.${theme}.${isDev ? 'dev' : 'min'}.css`;
  });
}

async function handleInstall() {
  const offlineCache = await caches.open('offline' + assetVersion);

  await offlineCache.add(new Request('/dasher', {
    headers: {
      'Accept': 'application/vnd.lichess.v2+json'
    }
  }));

  await offlineCache.addAll([
    '/444',
    '/editor',
  ]);

  await caches.open(assetVersion).then(cache => cache.addAll([
    '/favicon.ico',
    '/manifest.json',
    ...[
      'trans/refs.json',
      ...themedStylesheets('site'),
      ...themedStylesheets('editor'),
      ...themedStylesheets('challenge'),
      ...themedStylesheets('notify'),
      ...themedStylesheets('dasher'),
      ...themedStylesheets('autocomplete'),
      compiledScript('site'),
      compiledScript('deps'),
      compiledScript('editor'),
      compiledScript('cli'),
      compiledScript('challenge'),
      compiledScript('notify'),
      compiledScript('dasher'),
      'javascripts/vendor/typeahead.jquery.min.js',
      'javascripts/vendor/chessground.min.js',
      'images/icons/trash.svg',
      'images/icons/pointer.svg',
      'font/lichess.woff2',
      'font/lichess.chess.woff2',
      'font/noto-sans-latin.woff2',
      'font/noto-sans-bold-latin.woff2',
      'font/roboto-light-latin.woff2',
      'piece-css/cburnett.css',
      'images/board/svg/brown.svg',
    ].map(versionedAssetUrl),
    ...[
      'logo/lichess-favicon-32.png',
      'logo/lichess-favicon-32-invert.png',
      'logo/lichess-favicon-64.png',
      'logo/lichess-favicon-128.png',
      'logo/lichess-favicon-256.png',
      'logo/lichess-favicon-192.png',
      'logo/lichess-favicon-512.png',
    ].map(assetUrl),
  ]));
}

self.addEventListener('install', e => e.waitUntil(handleInstall()));

async function handleActivate() {
  const cacheNames = await caches.keys();
  await Promise.all(
    cacheNames
      .filter(cacheName => !cacheName.endsWith(assetVersion))
      .map(cacheName => caches.delete(cacheName)));
}

self.addEventListener('activate', e => e.waitUntil(handleActivate()));

async function handleResponse(_response: Response) {
  //console.log(response.url, response.body);
}

async function handleFetch(event: FetchEvent) {
  const url = new URL(event.request.url, self.location.href);
  if (url.pathname == '/dasher') {
    const offlineDasher = await caches.match('/dasher', {ignoreSearch: true, ignoreVary: true});
    console.log('offline dasher', offlineDasher);
    if (offlineDasher) return offlineDasher;
  } else if (url.pathname == '/editor' || url.pathname.startsWith('/editor/')) {
    const editorResponse = await caches.match('/editor');
    if (editorResponse) return editorResponse;
  } else {
    const assetCache = await self.caches.open(assetVersion);
    const assetReponse = await assetCache.match(event.request);
    if (assetReponse) return assetReponse;
  }

  try {
    const response = await fetch(event.request);
    console.log(event.request, response);
    event.waitUntil(handleResponse(response.clone()));
    return response;
  } catch(err) {
    if (event.request.mode == 'navigate') {
      const offlinePage = await caches.match('/444');
      if (offlinePage) return offlinePage;
    }
    throw err;
  }
}

self.addEventListener('fetch', event => {
  //if (event.request.method != 'GET') return;
  return event.respondWith(handleFetch(event));
});

self.addEventListener('push', event => {
  const data = event.data!.json();
  return self.registration.showNotification(data.title, {
    badge: assetUrl('images/logo.256.png'),
    icon: assetUrl('images/logo.256.png'),
    body: data.body,
    tag: data.tag,
    data: data.payload,
    requireInteraction: true,
  });
});

async function handleNotificationClick(event: NotificationEvent) {
  const notifications = await self.registration.getNotifications();
  notifications.forEach(notification => notification.close());

  const windowClients = await self.clients.matchAll({
    type: 'window',
    includeUncontrolled: true,
  }) as ReadonlyArray<WindowClient>;

  // determine url
  const data = event.notification.data.userData;
  let url = '/';
  if (data.fullId) url = '/' + data.fullId;
  else if (data.threadId) url = '/inbox/' + data.threadId + '#bottom';
  else if (data.challengeId) url = '/' + data.challengeId;

  // focus open window with same url
  for (const client of windowClients) {
    const clientUrl = new URL(client.url, self.location.href);
    if (clientUrl.pathname === url && 'focus' in client) return await client.focus();
  }

  // navigate from open homepage to url
  for (const client of windowClients) {
    const clientUrl = new URL(client.url, self.location.href);
    if (clientUrl.pathname === '/') return await client.navigate(url);
  }

  // open new window
  return await self.clients.openWindow(url);
}

self.addEventListener('notificationclick', e => e.waitUntil(handleNotificationClick(e)));
