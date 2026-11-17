from datetime import datetime, timezone
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch
import xml.etree.ElementTree as ET

from library import Library
from server import Catalog
from telegram_source import TelegramCatalog, channel_handle, parse_preview

CHANNEL = "tg_sample_audio"
PRIMARY = "UC" + "a" * 22
HTML = '''<div class="tgme_channel_info_header_title">Sample audio</div>
<div class="tgme_widget_message" data-post="sample_audio/42">
<div class="tgme_widget_message_document_icon audio"></div>
<div class="tgme_widget_message_document_title">A &amp; B</div>
<div class="tgme_widget_message_text">First line<br>Second line</div>
<time datetime="2026-03-04T09:00:00+00:00"></time></div>
<div class="tgme_widget_message" data-post="sample_audio/41">Text only</div>'''


class TelegramTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.catalog = TelegramCatalog(self.root/'state', self.root/'audio', 'https://example.com/audio', CHANNEL, 'Audio')

    def tearDown(self):
        self.catalog.pool.shutdown(wait=True)
        self.tmp.cleanup()

    def test_public_preview_only_audio_and_original_date(self):
        title, entries, oldest = parse_preview(HTML, 'sample_audio')
        self.assertEqual((title, oldest), ('Sample audio', 41))
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['id'], 'tg_42')
        self.assertEqual(entries[0]['title'], 'A & B')
        self.assertEqual(entries[0]['description'], 'First line\nSecond line')
        self.assertEqual(entries[0]['upload_date'], '20260304')
        for value in ('https://t.me/+invite', 'https://t.me/sample_audio/42', 'https://evil.example/sample_audio', 'https://t.me/sample_audio?before=42'):
            with self.assertRaises(ValueError):channel_handle(value)
        self.assertEqual(channel_handle('https://t.me/s/Sample_audio'), 'sample_audio')

    def test_pagination_archive_and_failed_refresh_keep_cache(self):
        with patch.dict('os.environ', {}, clear=True), patch('telegram_source.public_page', side_effect=[parse_preview(HTML, 'sample_audio'), ('Audio', [], None)]) as fetch:
            self.catalog.refresh('uploads')
            self.assertEqual(fetch.call_args_list[1].args, ('sample_audio', 41))
        self.catalog.archive(['tg_42'], True)
        row = self.catalog.snapshot()['episodes'][0]
        self.assertTrue(row['archived'])
        self.assertEqual(row['source_url'], 'https://t.me/sample_audio/42')
        with patch.dict('os.environ', {}, clear=True), patch('telegram_source.public_page', side_effect=ValueError('offline')), self.assertLogs(level='ERROR'):
            self.catalog.refresh('uploads')
        self.assertEqual(self.catalog.snapshot()['memberships']['uploads'], ['tg_42'])

    def test_telegram_download_retains_format_and_updates_feed(self):
        file = SimpleNamespace(name='Original.mp3', duration=125, ext='.mp3')
        message = SimpleNamespace(id=42, file=file, audio=True, voice=False,
                                  document=SimpleNamespace(attributes=[]), message='Original description',
                                  date=datetime(2026, 3, 4, tzinfo=timezone.utc))
        class Client:
            async def connect(self):pass
            async def disconnect(self):pass
            async def is_user_authorized(self):return True
            async def get_me(self):return SimpleNamespace(bot=False)
            async def get_messages(self, handle, ids):
                assert (handle, ids)==('sample_audio',42)
                return message
            async def download_media(self, message, file):Path(file).write_bytes(b'fixture audio')
        with self.catalog.db() as db:self.catalog.upsert_episode(db, {'id':'tg_42','title':'Original.mp3'})
        with patch('telegram_source.user_client', return_value=Client()):self.catalog.download_locked('tg_42')
        row = self.catalog.snapshot()['episodes'][0]
        self.assertEqual(row['job']['status'], 'done')
        self.assertTrue(row['audio_url'].endswith('.mp3'))
        self.assertEqual(row['duration'], 125)
        item=ET.parse(self.catalog.audio/'feed.rss').find('./channel/item')
        self.assertEqual(item.find('enclosure').get('type'), 'audio/mpeg')
        self.assertIn('2026',item.findtext('description'))
        self.assertFalse(list(self.catalog.audio.glob('*.part')))

    def test_registry_exclusion_persists_and_custom_mix_can_include_it(self):
        primary=Catalog(self.root/'primary', self.root/'libraries'/'primary', 'https://example.com/yt/primary', PRIMARY, 'Primary')
        library=Library(primary,Catalog,policy_file=self.root/'policy.json')
        try:
            with patch.object(library,'resolve',return_value=(CHANNEL,'Audio','https://t.me/sample_audio')),patch.object(library,'schedule_catalog'):
                library.add('https://t.me/sample_audio', excluded=True)
            telegram=library.catalog(CHANNEL)
            path=telegram.audio/'Audio [tg_42].mp3';path.write_bytes(b'audio')
            telegram.add_to_feed({'id':'tg_42','title':'Audio'},path)
            library.rebuild_feeds()
            self.assertEqual(len(ET.parse(library.audio_root/'feeds/global.rss').findall('./channel/item')),0)
            mixed=library.create_mix([PRIMARY,CHANNEL])
            self.assertEqual(len(ET.parse(library.audio_root/'feeds'/mixed.rsplit('/',1)[-1]).findall('./channel/item')),1)
            row=next(r for r in library.snapshot(CHANNEL)['podcasts'] if r['id']==CHANNEL)
            self.assertTrue(row['excluded_from_global'])
            self.assertFalse(row['auto_download'])
            library.set_excluded(CHANNEL,False)
            self.assertEqual(len(ET.parse(library.audio_root/'feeds/global.rss').findall('./channel/item')),1)
            library.set_excluded(CHANNEL,True)
            with library.db() as db:self.assertEqual(db.execute('SELECT excluded_global FROM podcasts WHERE id=?',(CHANNEL,)).fetchone()[0],1)
            with self.assertRaises(ValueError):library.set_excluded(PRIMARY,False)
        finally:
            library.refresh_pool.shutdown(wait=True);library.playlist_pool.shutdown(wait=True);primary.pool.shutdown(wait=True)


if __name__=='__main__':unittest.main()
