import { h, VNode } from 'snabbdom';
import { Redraw, Close, bind, header } from './util';
import debounce from 'common/debounce';
import * as xhr from 'common/xhr';
import throttle from 'common/throttle';

export interface BackgroundCtrl {
  list: Background[];
  set(k: string): void;
  get(): string;
  getImage(): string;
  setImage(i: string): void;
  trans: Trans;
  close: Close;
}

export interface BackgroundData {
  current: string;
  image: string;
}

interface Background {
  key: string;
  name: string;
  title?: string;
}

export function ctrl(data: BackgroundData, trans: Trans, redraw: Redraw, close: Close): BackgroundCtrl {
  const list: Background[] = [
    { key: 'system', name: trans.noarg('systemDefault') },
    { key: 'light', name: trans.noarg('light') },
    { key: 'dark', name: trans.noarg('dark') },
    { key: 'darkBoard', name: 'Dark Board', title: 'Like Dark, but chess boards are also darker' },
    { key: 'transp', name: trans.noarg('transparent') },
  ];

  const announceFail = () => lichess.announce({ msg: 'Failed to save background preference' });

  const reloadAllTheThings = () => {
    if (window.Highcharts) lichess.reload();
  };

  return {
    list,
    trans,
    get: () => data.current,
    set: throttle(700, (c: string) => {
      data.current = c;
      xhr
        .text('/pref/bg', {
          body: xhr.form({ bg: c }),
          method: 'post',
        })
        .then(reloadAllTheThings, announceFail);
      applyBackground(data, list);
      redraw();
    }),
    getImage: () => data.image,
    setImage(i: string) {
      data.image = i;
      xhr
        .text('/pref/bgImg', {
          body: xhr.form({ bgImg: i }),
          method: 'post',
        })
        .then(reloadAllTheThings, announceFail);
      applyBackground(data, list);
      redraw();
    },
    close,
  };
}

export function view(ctrl: BackgroundCtrl): VNode {
  const cur = ctrl.get();

  return h('div.sub.background', [
    header(ctrl.trans.noarg('background'), ctrl.close),
    h(
      'div.selector.large',
      ctrl.list.map(bg => {
        return h(
          'a.text',
          {
            class: { active: cur === bg.key },
            attrs: { 'data-icon': '', title: bg.title || '' },
            hook: bind('click', () => ctrl.set(bg.key)),
          },
          bg.name
        );
      })
    ),
    cur === 'transp' ? imageInput(ctrl) : null,
  ]);
}

function imageInput(ctrl: BackgroundCtrl) {
  return h('div.image', [
    h('p', ctrl.trans.noarg('backgroundImageUrl')),
    h('input', {
      attrs: {
        type: 'text',
        placeholder: 'https://',
        value: ctrl.getImage(),
      },
      hook: {
        insert: vnode => {
          $(vnode.elm as HTMLElement).on(
            'change keyup paste',
            debounce(function (this: HTMLInputElement) {
              const url = (this.value as string).trim();
              // modules/pref/src/main/PrefForm.scala
              if ((url.startsWith('https://') || url.startsWith('//')) && url.length >= 10 && url.length <= 400)
                ctrl.setImage(url);
            }, 300)
          );
        },
      },
    }),
  ]);
}

function applyBackground(data: BackgroundData, list: Background[]) {
  const key = data.current;
  const cls = key == 'transp' ? 'dark transp' : key == 'darkBoard' ? 'dark dark-board' : key;

  $('body')
    .removeClass([...list.map(b => b.key), 'dark-board'].join(' '))
    .addClass(cls);

  if (key === 'transp') {
    const bgData = document.getElementById('bg-data');
    bgData
      ? (bgData.innerHTML = 'body.transp::before{background-image:url(' + data.image + ');}')
      : $('head').append('<style id="bg-data">body.transp::before{background-image:url(' + data.image + ');}</style>');
  }
}
