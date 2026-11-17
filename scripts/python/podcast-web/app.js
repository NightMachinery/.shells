'use strict';
const $ = id => document.getElementById(id);
$('theme').value = document.documentElement.dataset.theme || 'light';
$('theme').onchange = () => {
  const theme = $('theme').value;
  document.documentElement.dataset.theme = theme;
  try { localStorage.setItem('podcast-web-theme', theme); } catch {}
};
const state = {data:null, page:0, size:40, selected:new Set(), visible:[], pending:0, loading:false, podcast:null, channelToken:0, feedSources:new Set()};
const preferences = ['hide-downloaded','hide-archived','hide-shorts','min-length','max-length','playlist','sort','search'];
const normalize = s => String(s||'').normalize('NFKC').replace(/ي/g,'ی').replace(/ك/g,'ک').toLocaleLowerCase().replace(/[\u200c\u200d]/g,'');
const node = (tag, text, cls) => {const e=document.createElement(tag); if(text!==undefined)e.textContent=text;if(cls)e.className=cls;return e;};
function link(text,url){const a=node('a',text);a.href=url;a.target='_blank';a.rel='noopener';return a;}
function button(text,fn,cls){const b=node('button',text,cls);b.onclick=fn;return b;}
function notice(message,error=false){$('notice').textContent=message;$('notice').className=error?'error':'';}
async function request(path,data){
  const podcast=state.podcast;
  let target='api/'+path;
  if(!data&&path==='catalog'&&podcast)target+='?podcast='+encodeURIComponent(podcast);
  const response=await fetch(target,{cache:'no-store',...(data?{method:'POST',headers:{'Content-Type':'application/json','X-Podcast-Action':'1'},body:JSON.stringify({...data,...(data.podcast?{}:podcast?{podcast}:{})})}:{})});
  if(!response.ok){let message=`Request failed (${response.status}). Your login may have expired; reload the page.`;try{message=(await response.json()).error||message;}catch{}throw Error(message);}
  if(!response.headers.get('content-type')?.includes('application/json'))throw Error('Your login may have expired. Reload the page to sign in.');
  return response.json();
}
async function action(path,data){if(path==='download')$('download-queue').open=true;state.pending++;try{await request(path,data);await load();}catch(e){notice(e.message,true);}finally{state.pending--;}}
function save(){try{const p={};preferences.forEach(id=>p[id]=$(id).type==='checkbox'?$(id).checked:$(id).value);localStorage.setItem('podcast-web-preferences',JSON.stringify(p));}catch{}}
function selectedCount(){const count=state.selected.size;$('selection').textContent=count?`${count} selected`:'';['bulk-download','bulk-archive','bulk-restore'].forEach(id=>$(id).disabled=!count);$('select-page').checked=state.visible.length>0&&state.visible.every(e=>state.selected.has(e.id));$('select-page').indeterminate=!$('select-page').checked&&state.visible.some(e=>state.selected.has(e.id));}
function duration(n){if(!n)return '';const m=Math.floor(n/60);return m>=60?`${Math.floor(m/60)}h ${m%60}m`:`${m} min`;}
function render(){if(!state.data)return;const d=state.data,source=$('playlist').value,query=normalize($('search').value).trim();const order=d.memberships[source]||[],positions=new Map(order.map((id,i)=>[id,i]));const sourceEpisodes = d.episodes.filter(e => source === 'downloaded' ? !!e.audio_url : positions.has(e.id));
const hideDownloaded = $('hide-downloaded').checked, hideArchived = $('hide-archived').checked;
const minutes = id => $(id).value === '' ? null : Number($(id).value);
const minimum = minutes('min-length'), maximum = minutes('max-length');
const invalidLength = [minimum, maximum].some(n => n !== null && (!Number.isFinite(n) || n < 0)) || (minimum !== null && maximum !== null && minimum > maximum);
$('length-error').textContent = invalidLength ? 'Enter nonnegative lengths, with minimum no greater than maximum.' : '';
const lengthActive = minimum !== null || maximum !== null;
const matchesLength = e => !invalidLength && (!lengthActive || (Number.isFinite(e.duration) && e.duration > 0 && (minimum === null || e.duration >= minimum * 60) && (maximum === null || e.duration <= maximum * 60)));
const matchesSearch = e => !query || normalize(e.title).includes(query) || e.id.includes(query);
const afterDownloaded = sourceEpisodes.filter(e => !hideDownloaded || !e.audio_url);
const afterArchived = afterDownloaded.filter(e => !hideArchived || !e.archived);
const afterShorts = afterArchived.filter(e => !$('hide-shorts').checked || !e.is_short);
const afterLength = afterShorts.filter(matchesLength);
let episodes = afterLength.filter(matchesSearch);
const hiddenDownloaded = sourceEpisodes.length - afterDownloaded.length;
const hiddenArchived = afterDownloaded.length - afterArchived.length;
const hiddenShorts = afterArchived.length - afterShorts.length;
const hiddenLength = afterShorts.length - afterLength.length;
const hiddenSearch = afterLength.length - episodes.length;
$('hide-shorts-label').textContent = `Hide YouTube Shorts (${sourceEpisodes.filter(e => e.is_short).length.toLocaleString()})`;
$('hide-downloaded-label').textContent = `Hide downloaded (${sourceEpisodes.filter(e => e.audio_url).length.toLocaleString()})`;
$('hide-archived-label').textContent = `Hide archived (${sourceEpisodes.filter(e => e.archived).length.toLocaleString()})`;
const hiddenReasons = [[hiddenDownloaded, 'downloaded'], [hiddenArchived, 'archived'], [hiddenShorts, 'YouTube Shorts'], [hiddenLength, 'outside your length limits or with unknown length'], [hiddenSearch, 'not matching your search']].filter(([n]) => n).map(([n, reason]) => `${n.toLocaleString()} ${reason}`);
$('filter-summary').hidden = !hiddenReasons.length;
$('filter-description').textContent = `Showing ${episodes.length.toLocaleString()} of ${sourceEpisodes.length.toLocaleString()} episodes. Hidden: ${hiddenReasons.join(', ')}.`;
$('show-all').textContent = `Show all ${sourceEpisodes.length.toLocaleString()}`;
const sort=$('sort').value;if(sort==='title')episodes.sort((a,b)=>a.title.localeCompare(b.title,'fa'));else if(sort==='playlist')episodes.sort((a,b)=>(positions.get(a.id)||0)-(positions.get(b.id)||0));else{const uploads=new Map((d.memberships.uploads||[]).map((id,i)=>[id,i]));episodes.sort((a,b)=>{const ai=uploads.get(a.id),bi=uploads.get(b.id);if(ai!==undefined&&bi!==undefined)return sort==='old'?bi-ai:ai-bi;const delta=String(b.published||'').localeCompare(String(a.published||''));return sort==='old'?-delta:delta;});}const pages=Math.max(1,Math.ceil(episodes.length/state.size));state.page=Math.min(state.page,pages-1);state.visible=episodes.slice(state.page*state.size,(state.page+1)*state.size);$('count').textContent=`(${episodes.length.toLocaleString()} of ${sourceEpisodes.length.toLocaleString()})`;$('page').textContent=`Page ${state.page+1} of ${pages}`;$('previous').disabled=state.page===0;$('next').disabled=state.page>=pages-1;const container=$('episodes');container.replaceChildren();if(!episodes.length)container.append(node('p',query?'No matching episodes. Try another search or turn off a hide filter.':source==='downloaded'&&$('hide-downloaded').checked?'Turn off “Hide downloaded” to see your downloaded library.':d.refreshes[source]?.status==='loading'?'Loading this catalog from YouTube. It will appear here automatically…':'No episodes to show. Try turning off the hide filters.','empty'));for(const e of state.visible){const row=node('article',undefined,'episode'),check=node('input');check.type='checkbox';check.checked=state.selected.has(e.id);check.setAttribute('aria-label',`Select ${e.title}`);check.onchange=()=>{check.checked?state.selected.add(e.id):state.selected.delete(e.id);selectedCount();};row.append(check);const body=node('div'),title=node('h3',e.title);title.dir='auto';body.append(title);const meta=node('div',undefined,'meta');if(e.published)meta.append(node('span',`${e.published.slice(0,4)}-${e.published.slice(4,6)}-${e.published.slice(6,8)}`));if(e.duration)meta.append(node('span',duration(e.duration)));if(e.is_short)meta.append(node('span','YouTube Short','badge'));if(e.audio_url)meta.append(node('span','Downloaded','badge'));if(e.archived)meta.append(node('span','Archived','badge archived'));if(e.job&&['queued','downloading'].includes(e.job.status))meta.append(node('span',e.job.status==='queued'?'Queued':'Downloading…','badge'));body.append(meta);if(e.job?.status==='error')body.append(node('div',e.job.error,'job-error'));row.append(body);const controls=node('div',undefined,'actions');if(e.audio_url){controls.append(link('Audio ↗',e.audio_url));controls.append(button('Listen',()=>{const audio=$('audio-player');$('player').hidden=false;$('playing-title').textContent=e.title;if(audio.getAttribute('src')!==e.audio_url)audio.src=e.audio_url;audio.play().catch(()=>{});}));}else{const b=button(e.job?.status==='error'?'Retry download':'Download',()=>action('download',{id:e.id}),'primary');b.disabled=['queued','downloading'].includes(e.job?.status);controls.append(b);}if(!e.local)controls.append(link('YouTube ↗',`https://www.youtube.com/watch?v=${e.id}`));controls.append(button(e.archived?'Restore':'Archive',()=>action('archive',{ids:[e.id],archived:!e.archived})));row.append(controls);container.append(row);}const plink=$('playlist-link');plink.hidden=['uploads','downloaded'].includes(source);plink.href=`https://www.youtube.com/playlist?list=${encodeURIComponent(source)}`;selectedCount();}
function renderPlaylists(){const select=$('playlist'),previous=select.value;select.replaceChildren();for(const [value,text] of [['uploads','All episodes'],['downloaded','Downloaded library'],...state.data.playlists.map(p=>[p.id,p.title])]){const option=node('option',text);option.value=value;select.append(option);}select.value=[...select.options].some(o=>o.value===previous)?previous:'uploads';$('playlist-count').textContent=`(${state.data.playlists.length})`;const cards=$('playlists');cards.replaceChildren();if(!state.data.playlists.length)cards.append(node('p','No public playlists found. Add a playlist using its YouTube link.','playlist-help'));for(const p of state.data.playlists){const card=node('article',undefined,'playlist-card'),heading=node('h3',p.title);heading.dir='auto';card.append(heading,button('Browse episodes',()=>{select.value=p.id;changed({target:select});$('search').scrollIntoView({block:'start',behavior:'smooth'});}),link('YouTube ↗',`https://www.youtube.com/playlist?list=${p.id}`));cards.append(card);}}
async function load(){if(state.loading)return;state.loading=true;const token=state.channelToken;try{const data=await request('catalog');if(token!==state.channelToken)return;const changedPlaylists=state.data?.current_podcast!==data.current_podcast||JSON.stringify(state.data?.playlists)!==JSON.stringify(data.playlists);state.data=data;renderLibrary();renderQueue();$('title').textContent=data.title;document.title=data.title+' · Podcast library';$('feed').href=data.feed_url;if(changedPlaylists)renderPlaylists();render();const loading=Object.entries(data.refreshes).filter(([,v])=>v.status==='loading');const errors=Object.entries(data.refreshes).filter(([,v])=>v.status==='error');const jobs=data.episodes.filter(e=>['queued','downloading'].includes(e.job?.status));const loaded=(data.memberships.uploads||[]).length,downloaded=data.episodes.filter(e=>e.audio_url).length;notice([`${loaded.toLocaleString()} episodes · ${downloaded.toLocaleString()} downloaded`,loading.length?'Updating catalog from YouTube…':'',jobs.length?`${jobs.length} download${jobs.length===1?'':'s'} in the queue`:'',...errors.map(([s,v])=>`${s==='uploads'?'Episode catalog':s==='playlists'?'Playlists':'Playlist'}: ${v.error}`)].filter(Boolean).join('\n'),!!errors.length);}catch(e){notice(e.message,true);}finally{state.loading=false;}}
async function ensureSource(){if(!state.data)return;const source=$('playlist').value;if(!['uploads','downloaded'].includes(source)&&!state.data?.sources[source]&&state.data?.refreshes[source]?.status!=='loading')await action('refresh',{source});}
function changed(event){if(event?.target?.id==='playlist'&&$('playlist').value==='downloaded')$('hide-downloaded').checked=false;state.page=0;save();render();ensureSource();}
for(const id of preferences)$(id).addEventListener(['search','min-length','max-length'].includes(id)?'input':'change',changed);
$('refresh').onclick=async()=>{await action('refresh',{source:$('playlist').value==='downloaded'?'uploads':$('playlist').value});await action('refresh',{source:'playlists'});await action('refresh',{source:'shorts'});};
$('previous').onclick=()=>{state.page--;render();};$('next').onclick=()=>{state.page++;render();};
$('select-page').onchange=()=>{for(const e of state.visible)$('select-page').checked?state.selected.add(e.id):state.selected.delete(e.id);render();};
async function archiveSelection(archived){const podcast=state.podcast,ids=[...state.selected];for(let i=0;i<ids.length;i+=500)await action('archive',{ids:ids.slice(i,i+500),archived,podcast});state.selected.clear();render();}
$('bulk-archive').onclick=()=>archiveSelection(true);$('bulk-restore').onclick=()=>archiveSelection(false);$('bulk-download').onclick=async()=>{const podcast=state.podcast,ids=[...state.selected];state.selected.clear();for(const id of ids)await action('download',{id,podcast});render();};
try{state.podcast=localStorage.getItem('podcast-web-source')||null;const p=JSON.parse(localStorage.getItem('podcast-web-preferences')||'{}');for(const id of preferences){if(p[id]!==undefined){if($(id).type==='checkbox')$(id).checked=p[id];else if(id!=='playlist')$(id).value=p[id];}}state.savedPlaylist=p.playlist;}catch{}
(async()=>{await load();if(state.savedPlaylist&&[...$('playlist').options].some(o=>o.value===state.savedPlaylist)){$('playlist').value=state.savedPlaylist;render();await ensureSource();}})();
setInterval(()=>{if(!document.hidden&&!state.pending){const active=state.data&&(Object.values(state.data.refreshes).some(v=>v.status==='loading')||state.data.episodes.some(e=>['queued','downloading'].includes(e.job?.status)));if(active||Date.now()-(state.lastSlowLoad||0)>60000){state.lastSlowLoad=Date.now();load();}}},4000);
document.addEventListener('visibilitychange',()=>{if(!document.hidden)load();});

$('add-playlist').onsubmit=async event=>{event.preventDefault();const b=event.currentTarget.querySelector('button');b.disabled=true;try{const result=await request('playlist',{url:$('playlist-url').value});await load();$('playlist').value=result.id;changed({target:$('playlist')});$('playlist-url').value='';}catch(e){notice(e.message,true);}finally{b.disabled=false;}};

$('show-all').onclick = () => { $('hide-downloaded').checked = false; $('hide-archived').checked = false; $('hide-shorts').checked = false; $('min-length').value = ''; $('max-length').value = ''; $('search').value = ''; changed(); };

function renderQueue() {
  const jobs = (state.data.queue || state.data.episodes).filter(e => e.job);
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
    const title = node('div', (episode.podcast_title ? episode.podcast_title + ': ' : '') + episode.title, 'queue-title'); title.dir = 'auto'; content.append(title);
    const labels = {downloading: 'Downloading…', queued: `Waiting · position ${position}`, done: 'Downloaded', error: 'Failed'};
    content.append(node('span', labels[episode.job.status] || episode.job.status, 'badge'));
    if (episode.job.error) content.append(node('div', episode.job.error, 'job-error'));
    const controls = node('div', undefined, 'queue-actions');
    if (episode.audio_url) controls.append(link('Audio ↗', episode.audio_url));
    if (episode.job.status === 'error') controls.append(button('Retry download', () => action('download', {id:episode.id,podcast:episode.podcast_id || state.podcast})));
    item.append(content, controls); return item;
  };
  active.forEach((episode,i) => $('queue-active').append(row(episode, 'li', i+1)));
  recent.slice(0,20).forEach(episode => $('queue-recent').append(row(episode, 'article')));
  $('queue-history-count').textContent = `(${Math.min(20,recent.length)} recent)`;
  $('queue-history').hidden = !recent.length;
}

function renderLibrary() {
  const channels=state.data.podcasts;
  if(!channels)return;
  $('podcast-library').hidden=false;
  const current=state.data.current_podcast;
  if(!state.podcast)state.podcast=current;
  const select=$('podcast');
  const options=JSON.stringify(channels.map(c=>[c.id,c.title]));
  if(select.dataset.options!==options){
    select.replaceChildren();
    for(const channel of channels){const option=node('option',channel.title);option.value=channel.id;select.append(option);}
    select.dataset.options=options;
  }
  select.value=current;
  const channel=channels.find(c=>c.id===current);
  $('auto-download').checked=!!channel?.auto_download;
  $('auto-status').textContent=channel?.auto_error?'Automatic check failed: '+channel.auto_error:'New channels start with automatic downloads off. Enabling it watches for new uploads; older episodes stay manual.';
  $('podcast-feed-url').value=state.data.feed_url;
  $('global-feed-url').value=state.data.global_feed_url;
  const excluded=channels.find(c=>c.excluded_from_global);
  document.querySelector('label[for="global-feed-url"]').textContent='Global feed: everything except '+(excluded?.title||'the original podcast');
  const sources=$('feed-sources');sources.replaceChildren();
  for(const source of channels){
    const label=node('label',undefined,'feed-source');
    const check=node('input');check.type='checkbox';check.value=source.id;check.checked=state.feedSources.has(source.id);
    check.onchange=()=>{check.checked?state.feedSources.add(source.id):state.feedSources.delete(source.id);$('create-feed').disabled=!state.feedSources.size;};
    const text=node('span',source.title+' · '+source.downloaded+' downloaded'+(source.loading?' · catalog loading…':''));
    label.append(check,text);sources.append(label);
  }
  $('create-feed').disabled=!state.feedSources.size;
}
async function switchPodcast(identifier) {
  state.podcast=identifier;state.channelToken++;state.page=0;state.selected.clear();state.data=null;
  $('search').value='';$('playlist').value='uploads';
  $('episodes').replaceChildren(node('p','Loading podcast…','empty'));
  try{localStorage.setItem('podcast-web-source',identifier);}catch{}
  while(state.loading)await new Promise(resolve=>setTimeout(resolve,30));
  await load();
}
$('podcast').onchange=()=>switchPodcast($('podcast').value);
$('auto-download').onchange=async()=>{
  const toggle=$('auto-download'),enabled=toggle.checked;toggle.disabled=true;
  try{await request('auto',{enabled});await load();$('library-status').textContent=enabled?'Automatic downloads enabled for new uploads.':'Automatic downloads disabled.';}
  catch(error){toggle.checked=!enabled;$('library-status').textContent=error.message;}
  finally{toggle.disabled=false;}
};
$('add-podcast').onsubmit=async event=>{
  event.preventDefault();const button=event.currentTarget.querySelector('button');button.disabled=true;button.textContent='Adding…';
  try{const result=await request('podcasts',{url:$('podcast-url').value});$('podcast-url').value='';await switchPodcast(result.id);$('library-status').textContent='Podcast added. Automatic downloads are off.';}
  catch(error){$('library-status').textContent=error.message;}
  finally{button.disabled=false;button.textContent='Add podcast';}
};
$('create-feed').onclick=async()=>{
  const button=$('create-feed');button.disabled=true;
  try{const result=await request('feeds',{podcasts:[...state.feedSources]});$('combined-feed-url').value=result.url;$('combined-feed').hidden=false;$('library-status').textContent='Combined feed created. It updates as episodes are downloaded.';}
  catch(error){$('library-status').textContent=error.message;}
  finally{button.disabled=!state.feedSources.size;}
};
async function copyFeed(id){
  const input=$(id);
  try{await navigator.clipboard.writeText(input.value);$('library-status').textContent='Feed URL copied.';}
  catch{input.focus();input.select();$('library-status').textContent='Select and copy the feed URL.';}
}
$('copy-podcast-feed').onclick=()=>copyFeed('podcast-feed-url');
$('copy-combined-feed').onclick=()=>copyFeed('combined-feed-url');
$('copy-global-feed').onclick=()=>copyFeed('global-feed-url');
