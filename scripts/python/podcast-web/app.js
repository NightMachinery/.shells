'use strict';
const $ = id => document.getElementById(id);
$('theme').value = document.documentElement.dataset.theme || 'light';
$('theme').onchange = () => {
  const theme = $('theme').value;
  document.documentElement.dataset.theme = theme;
  try { localStorage.setItem('podcast-web-theme', theme); } catch {}
};
const state = {data:null, page:0, size:40, selected:new Set(), visible:[], pending:0, loading:false};
const preferences = ['hide-downloaded','hide-archived','playlist','sort','search'];
const normalize = s => String(s||'').normalize('NFKC').replace(/ي/g,'ی').replace(/ك/g,'ک').toLocaleLowerCase().replace(/[\u200c\u200d]/g,'');
const node = (tag, text, cls) => {const e=document.createElement(tag); if(text!==undefined)e.textContent=text;if(cls)e.className=cls;return e;};
function link(text,url){const a=node('a',text);a.href=url;a.target='_blank';a.rel='noopener';return a;}
function button(text,fn,cls){const b=node('button',text,cls);b.onclick=fn;return b;}
function notice(message,error=false){$('notice').textContent=message;$('notice').className=error?'error':'';}
async function request(path,data){const response=await fetch('api/'+path,{cache:'no-store',...(data?{method:'POST',headers:{'Content-Type':'application/json','X-Podcast-Action':'1'},body:JSON.stringify(data)}:{})});if(!response.ok){let message=`Request failed (${response.status}). Your login may have expired; reload the page.`;try{message=(await response.json()).error||message;}catch{}throw Error(message);}if(!response.headers.get('content-type')?.includes('application/json'))throw Error('Your login may have expired. Reload the page to sign in.');return response.json();}
async function action(path,data){if(path==='download')$('download-queue').open=true;state.pending++;try{await request(path,data);await load();}catch(e){notice(e.message,true);}finally{state.pending--;}}
function save(){try{const p={};preferences.forEach(id=>p[id]=$(id).type==='checkbox'?$(id).checked:$(id).value);localStorage.setItem('podcast-web-preferences',JSON.stringify(p));}catch{}}
function selectedCount(){const count=state.selected.size;$('selection').textContent=count?`${count} selected`:'';['bulk-download','bulk-archive','bulk-restore'].forEach(id=>$(id).disabled=!count);$('select-page').checked=state.visible.length>0&&state.visible.every(e=>state.selected.has(e.id));$('select-page').indeterminate=!$('select-page').checked&&state.visible.some(e=>state.selected.has(e.id));}
function duration(n){if(!n)return '';const m=Math.floor(n/60);return m>=60?`${Math.floor(m/60)}h ${m%60}m`:`${m} min`;}
function render(){if(!state.data)return;const d=state.data,source=$('playlist').value,query=normalize($('search').value).trim();const order=d.memberships[source]||[],positions=new Map(order.map((id,i)=>[id,i]));const sourceEpisodes = d.episodes.filter(e => source === 'downloaded' ? !!e.audio_url : positions.has(e.id));
const hideDownloaded = $('hide-downloaded').checked, hideArchived = $('hide-archived').checked;
const matchesSearch = e => !query || normalize(e.title).includes(query) || e.id.includes(query);
const afterDownloaded = sourceEpisodes.filter(e => !hideDownloaded || !e.audio_url);
const afterArchived = afterDownloaded.filter(e => !hideArchived || !e.archived);
let episodes = afterArchived.filter(matchesSearch);
const hiddenDownloaded = sourceEpisodes.length - afterDownloaded.length;
const hiddenArchived = afterDownloaded.length - afterArchived.length;
const hiddenSearch = afterArchived.length - episodes.length;
$('hide-downloaded-label').textContent = `Hide downloaded (${sourceEpisodes.filter(e => e.audio_url).length.toLocaleString()})`;
$('hide-archived-label').textContent = `Hide archived (${sourceEpisodes.filter(e => e.archived).length.toLocaleString()})`;
const hiddenReasons = [[hiddenDownloaded, 'downloaded'], [hiddenArchived, 'archived'], [hiddenSearch, 'not matching your search']].filter(([n]) => n).map(([n, reason]) => `${n.toLocaleString()} ${reason}`);
$('filter-summary').hidden = !hiddenReasons.length;
$('filter-description').textContent = `Showing ${episodes.length.toLocaleString()} of ${sourceEpisodes.length.toLocaleString()} episodes. Hidden: ${hiddenReasons.join(', ')}.`;
$('show-all').textContent = `Show all ${sourceEpisodes.length.toLocaleString()}`;
const sort=$('sort').value;if(sort==='title')episodes.sort((a,b)=>a.title.localeCompare(b.title,'fa'));else if(sort==='playlist')episodes.sort((a,b)=>(positions.get(a.id)||0)-(positions.get(b.id)||0));else{const uploads=new Map((d.memberships.uploads||[]).map((id,i)=>[id,i]));episodes.sort((a,b)=>{const ai=uploads.get(a.id),bi=uploads.get(b.id);if(ai!==undefined&&bi!==undefined)return sort==='old'?bi-ai:ai-bi;const delta=String(b.published||'').localeCompare(String(a.published||''));return sort==='old'?-delta:delta;});}const pages=Math.max(1,Math.ceil(episodes.length/state.size));state.page=Math.min(state.page,pages-1);state.visible=episodes.slice(state.page*state.size,(state.page+1)*state.size);$('count').textContent=`(${episodes.length.toLocaleString()} of ${sourceEpisodes.length.toLocaleString()})`;$('page').textContent=`Page ${state.page+1} of ${pages}`;$('previous').disabled=state.page===0;$('next').disabled=state.page>=pages-1;const container=$('episodes');container.replaceChildren();if(!episodes.length)container.append(node('p',query?'No matching episodes. Try another search or turn off a hide filter.':source==='downloaded'&&$('hide-downloaded').checked?'Turn off “Hide downloaded” to see your downloaded library.':d.refreshes[source]?.status==='loading'?'Loading this catalog from YouTube. It will appear here automatically…':'No episodes to show. Try turning off the hide filters.','empty'));for(const e of state.visible){const row=node('article',undefined,'episode'),check=node('input');check.type='checkbox';check.checked=state.selected.has(e.id);check.setAttribute('aria-label',`Select ${e.title}`);check.onchange=()=>{check.checked?state.selected.add(e.id):state.selected.delete(e.id);selectedCount();};row.append(check);const body=node('div'),title=node('h3',e.title);title.dir='auto';body.append(title);const meta=node('div',undefined,'meta');if(e.published)meta.append(node('span',`${e.published.slice(0,4)}-${e.published.slice(4,6)}-${e.published.slice(6,8)}`));if(e.duration)meta.append(node('span',duration(e.duration)));if(e.audio_url)meta.append(node('span','Downloaded','badge'));if(e.archived)meta.append(node('span','Archived','badge archived'));if(e.job&&['queued','downloading'].includes(e.job.status))meta.append(node('span',e.job.status==='queued'?'Queued':'Downloading…','badge'));body.append(meta);if(e.job?.status==='error')body.append(node('div',e.job.error,'job-error'));row.append(body);const controls=node('div',undefined,'actions');if(e.audio_url){controls.append(link('Audio ↗',e.audio_url));controls.append(button('Listen',()=>{const audio=$('audio-player');$('player').hidden=false;$('playing-title').textContent=e.title;if(audio.getAttribute('src')!==e.audio_url)audio.src=e.audio_url;audio.play().catch(()=>{});}));}else{const b=button(e.job?.status==='error'?'Retry download':'Download',()=>action('download',{id:e.id}),'primary');b.disabled=['queued','downloading'].includes(e.job?.status);controls.append(b);}if(!e.local)controls.append(link('YouTube ↗',`https://www.youtube.com/watch?v=${e.id}`));controls.append(button(e.archived?'Restore':'Archive',()=>action('archive',{ids:[e.id],archived:!e.archived})));row.append(controls);container.append(row);}const plink=$('playlist-link');plink.hidden=['uploads','downloaded'].includes(source);plink.href=`https://www.youtube.com/playlist?list=${encodeURIComponent(source)}`;selectedCount();}
function renderPlaylists(){const select=$('playlist'),previous=select.value;select.replaceChildren();for(const [value,text] of [['uploads','All episodes'],['downloaded','Downloaded library'],...state.data.playlists.map(p=>[p.id,p.title])]){const option=node('option',text);option.value=value;select.append(option);}select.value=[...select.options].some(o=>o.value===previous)?previous:'uploads';$('playlist-count').textContent=`(${state.data.playlists.length})`;const cards=$('playlists');cards.replaceChildren();if(!state.data.playlists.length)cards.append(node('p','No public playlists found. Add a playlist using its YouTube link.','playlist-help'));for(const p of state.data.playlists){const card=node('article',undefined,'playlist-card'),heading=node('h3',p.title);heading.dir='auto';card.append(heading,button('Browse episodes',()=>{select.value=p.id;changed({target:select});$('search').scrollIntoView({block:'start',behavior:'smooth'});}),link('YouTube ↗',`https://www.youtube.com/playlist?list=${p.id}`));cards.append(card);}}
async function load(){if(state.loading)return;state.loading=true;try{const data=await request('catalog');const changedPlaylists=JSON.stringify(state.data?.playlists)!==JSON.stringify(data.playlists);state.data=data;renderQueue();$('title').textContent=data.title;document.title=data.title+' · Podcast library';$('feed').href=data.feed_url;if(changedPlaylists)renderPlaylists();render();const loading=Object.entries(data.refreshes).filter(([,v])=>v.status==='loading');const errors=Object.entries(data.refreshes).filter(([,v])=>v.status==='error');const jobs=data.episodes.filter(e=>['queued','downloading'].includes(e.job?.status));const loaded=(data.memberships.uploads||[]).length,downloaded=data.episodes.filter(e=>e.audio_url).length;notice([`${loaded.toLocaleString()} episodes · ${downloaded.toLocaleString()} downloaded`,loading.length?'Updating catalog from YouTube…':'',jobs.length?`${jobs.length} download${jobs.length===1?'':'s'} in the queue`:'',...errors.map(([s,v])=>`${s==='uploads'?'Episode catalog':s==='playlists'?'Playlists':'Playlist'}: ${v.error}`)].filter(Boolean).join('\n'),!!errors.length);}catch(e){notice(e.message,true);}finally{state.loading=false;}}
async function ensureSource(){const source=$('playlist').value;if(!['uploads','downloaded'].includes(source)&&!state.data?.sources[source]&&state.data?.refreshes[source]?.status!=='loading')await action('refresh',{source});}
function changed(event){if(event?.target?.id==='playlist'&&$('playlist').value==='downloaded')$('hide-downloaded').checked=false;state.page=0;save();render();ensureSource();}
for(const id of preferences)$(id).addEventListener(id==='search'?'input':'change',changed);
$('refresh').onclick=async()=>{await action('refresh',{source:$('playlist').value==='downloaded'?'uploads':$('playlist').value});await action('refresh',{source:'playlists'});};
$('previous').onclick=()=>{state.page--;render();};$('next').onclick=()=>{state.page++;render();};
$('select-page').onchange=()=>{for(const e of state.visible)$('select-page').checked?state.selected.add(e.id):state.selected.delete(e.id);render();};
async function archiveSelection(archived){const ids=[...state.selected];for(let i=0;i<ids.length;i+=500)await action('archive',{ids:ids.slice(i,i+500),archived});state.selected.clear();render();}
$('bulk-archive').onclick=()=>archiveSelection(true);$('bulk-restore').onclick=()=>archiveSelection(false);$('bulk-download').onclick=async()=>{const ids=[...state.selected];state.selected.clear();for(const id of ids)await action('download',{id});render();};
try{const p=JSON.parse(localStorage.getItem('podcast-web-preferences')||'{}');for(const id of preferences){if(p[id]!==undefined){if($(id).type==='checkbox')$(id).checked=p[id];else if(id!=='playlist')$(id).value=p[id];}}state.savedPlaylist=p.playlist;}catch{}
(async()=>{await load();if(state.savedPlaylist&&[...$('playlist').options].some(o=>o.value===state.savedPlaylist)){$('playlist').value=state.savedPlaylist;render();await ensureSource();}})();
setInterval(()=>{if(!document.hidden&&!state.pending){const active=state.data&&(Object.values(state.data.refreshes).some(v=>v.status==='loading')||state.data.episodes.some(e=>['queued','downloading'].includes(e.job?.status)));if(active||Date.now()-(state.lastSlowLoad||0)>60000){state.lastSlowLoad=Date.now();load();}}},4000);
document.addEventListener('visibilitychange',()=>{if(!document.hidden)load();});

$('add-playlist').onsubmit=async event=>{event.preventDefault();const b=event.currentTarget.querySelector('button');b.disabled=true;try{const result=await request('playlist',{url:$('playlist-url').value});await load();$('playlist').value=result.id;changed({target:$('playlist')});$('playlist-url').value='';}catch(e){notice(e.message,true);}finally{b.disabled=false;}};

$('show-all').onclick = () => { $('hide-downloaded').checked = false; $('hide-archived').checked = false; $('search').value = ''; changed(); };

function renderQueue() {
  const jobs = state.data.episodes.filter(e => e.job);
  const active = jobs.filter(e => ['queued', 'downloading'].includes(e.job.status)).sort((a,b) => (a.job.status === 'downloading' ? 0 : 1) - (b.job.status === 'downloading' ? 0 : 1) || a.job.updated - b.job.updated);
  const recent = jobs.filter(e => !['queued', 'downloading'].includes(e.job.status)).sort((a,b) => b.job.updated - a.job.updated);
  const running = active.filter(e => e.job.status === 'downloading').length;
  const failed = recent.filter(e => e.job.status === 'error').length;
  $('queue-count').textContent = `(${active.length} active)`;
  $('queue-status').textContent = active.length ? `${running} downloading · ${active.length-running} waiting` : 'No downloads waiting.';
  if (failed) $('queue-status').textContent += ` · ${failed} failed`;
  $('queue-active').replaceChildren();
  $('queue-recent').replaceChildren();
  const row = (episode, tag, position) => {
    const item = node(tag, undefined, 'queue-item');
    const content = node('div');
    const title = node('div', episode.title, 'queue-title'); title.dir = 'auto'; content.append(title);
    const labels = {downloading: 'Downloading…', queued: `Waiting · position ${position}`, done: 'Downloaded', error: 'Failed'};
    content.append(node('span', labels[episode.job.status] || episode.job.status, 'badge'));
    if (episode.job.error) content.append(node('div', episode.job.error, 'job-error'));
    const controls = node('div', undefined, 'queue-actions');
    if (episode.audio_url) controls.append(link('Audio ↗', episode.audio_url));
    if (episode.job.status === 'error') controls.append(button('Retry download', () => action('download', {id:episode.id})));
    item.append(content, controls); return item;
  };
  active.forEach((episode,i) => $('queue-active').append(row(episode, 'li', i+1)));
  recent.slice(0,20).forEach(episode => $('queue-recent').append(row(episode, 'article')));
  $('queue-history-count').textContent = `(${Math.min(20,recent.length)} recent)`;
  $('queue-history').hidden = !recent.length;
}
