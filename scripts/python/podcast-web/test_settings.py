from concurrent.futures import ThreadPoolExecutor
from copy import deepcopy
from pathlib import Path
import subprocess
import tempfile
import tomllib
import unittest
from unittest.mock import patch

from library import Library
from server import Catalog
from settings import SettingsFile, dumps

PRIMARY, SECOND = ['UC' + c*22 for c in 'ab']
ID = 'abcdefghijk'


class SettingsTests(unittest.TestCase):
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.root=Path(self.tmp.name)
        self.catalog=Catalog(self.root/'state',self.root/'audio'/'original','https://example.com/audio/original',PRIMARY,'گفت‌وگو "One"')
        self.library=Library(self.catalog,Catalog,policy_file=self.root/'policy.json',settings_path=self.root/'config'/'library.toml')

    def tearDown(self):
        self.library.stop.set();self.library.settings.stop.set()
        self.library.refresh_pool.shutdown(wait=True);self.library.playlist_pool.shutdown(wait=True);self.catalog.pool.shutdown(wait=True)
        self.tmp.cleanup()

    def test_runtime_choices_persist_but_catalogs_and_jobs_do_not(self):
        with self.catalog.db() as db:self.catalog.upsert_episode(db,{'id':ID,'title':'Episode'})
        self.catalog.archive([ID],True)
        self.library.set_auto(PRIMARY,False)
        with patch.object(self.catalog,'schedule_refresh'):self.catalog.add_playlist('PLmanualplaylist')
        with patch.object(self.library,'resolve',return_value=(SECOND,'Second','https://www.youtube.com/@second')),patch.object(self.library,'schedule_catalog'):
            self.library.add('@second',excluded=True)
        self.library.create_mix([PRIMARY,SECOND])
        doc=self.library.settings.read();rows={p['id']:p for p in doc['podcasts']}
        self.assertEqual(rows[PRIMARY]['archived'],[ID])
        self.assertEqual(rows[PRIMARY]['playlists'],['PLmanualplaylist'])
        self.assertFalse(rows[PRIMARY]['auto_download']);self.assertTrue(rows[SECOND]['excluded_from_global'])
        self.assertEqual(len(doc['feeds']),1)
        before=self.library.settings.path.read_bytes()
        self.catalog.extract=lambda url:{'entries':[{'id':ID,'title':'Refreshed','duration':45}]}
        self.catalog.refresh('uploads');self.catalog.job(ID,'done')
        self.assertEqual(self.library.settings.path.read_bytes(),before)
        self.assertEqual(tomllib.loads(dumps(doc)),doc)

    def test_toml_rebuilds_settings_and_archives_after_sqlite_loss(self):
        with self.catalog.db() as db:self.catalog.upsert_episode(db,{'id':ID,'title':'Episode'})
        self.catalog.archive([ID],True);self.library.set_auto(PRIMARY,False)
        with patch.object(self.catalog,'schedule_refresh'):self.catalog.add_playlist('PLmanualplaylist')
        original_feed=self.library.create_mix([PRIMARY])
        fresh=Catalog(self.root/'rebuilt',self.catalog.audio,self.catalog.base_url,PRIMARY,'Default title')
        rebuilt=Library(fresh,Catalog,policy_file=self.root/'new-policy.json',settings_path=self.library.settings.path)
        try:
            snap=rebuilt.snapshot()
            self.assertTrue(next(e for e in snap['episodes'] if e['id']==ID)['archived'])
            self.assertFalse(snap['podcasts'][0]['auto_download'])
            self.assertEqual(snap['title'],'گفت‌وگو "One"')
            self.assertEqual(snap['sources'],{})
            self.assertIn('PLmanualplaylist',[p['id'] for p in snap['playlists']])
            self.assertEqual(rebuilt.create_mix([PRIMARY]),original_feed)
            with patch.object(rebuilt,'schedule_catalog') as discovery,patch.object(fresh,'schedule_refresh'),patch.object(rebuilt,'prefetch_playlists'):
                rebuilt.maintain_catalogs()
                discovery.assert_called_once_with(fresh)
        finally:
            rebuilt.refresh_pool.shutdown(wait=True);rebuilt.playlist_pool.shutdown(wait=True);fresh.pool.shutdown(wait=True)

    def test_existing_sqlite_settings_export_without_losing_legacy_archives(self):
        with self.catalog.db() as db:
            db.execute('INSERT INTO episodes(id,title,archived) VALUES(?,?,1)',(ID,'Legacy'))
            db.execute("INSERT INTO playlists VALUES('PLlegacyplaylist','Legacy playlist')")
        migrated=Library(self.catalog,Catalog,policy_file=self.root/'migrated-policy.json',settings_path=self.root/'migrated.toml')
        try:
            row=migrated.settings.read()['podcasts'][0]
            self.assertEqual(row['archived'],[ID]);self.assertEqual(row['playlists'],['PLlegacyplaylist'])
        finally:migrated.refresh_pool.shutdown(wait=True);migrated.playlist_pool.shutdown(wait=True)

    def test_invalid_settings_never_overwrite_file_or_change_sqlite_flags(self):
        self.library.settings.path.write_text('version = 999\n')
        with self.assertRaises(ValueError):self.library.set_auto(PRIMARY,False)
        self.assertEqual(self.library.settings.path.read_text(),'version = 999\n')
        self.assertTrue(self.library.podcasts()[0]['auto_download'])

    def test_concurrent_updates_merge_and_repeated_updates_do_not_rewrite(self):
        store=self.library.settings
        with ThreadPoolExecutor(max_workers=2) as pool:
            list(pool.map(lambda pair:store.set_source(PRIMARY,*pair),[('auto_download',False),('archived',[ID])]))
        row=store.read()['podcasts'][0]
        self.assertFalse(row['auto_download']);self.assertEqual(row['archived'],[ID])
        store.pending.clear();store.set_source(PRIMARY,'archived',[ID])
        self.assertFalse(store.pending.is_set())

    def test_sync_failure_keeps_local_state_and_retries(self):
        store=self.library.settings;store.sync_command=['sync-settings']
        original=store.path.read_bytes()
        with patch('settings.subprocess.run',side_effect=subprocess.CalledProcessError(1,['sync-settings'])):
            store.sync_once()
        self.assertTrue(store.pending.is_set());self.assertIsNotNone(store.error)
        self.assertEqual(store.path.read_bytes(),original)
        with patch('settings.subprocess.run'):
            store.sync_once()
        self.assertFalse(store.pending.is_set());self.assertIsNone(store.error)
        def changed_during_push(*args,**kwargs):store.set_source(PRIMARY,'auto_download',False)
        with patch('settings.subprocess.run',side_effect=changed_during_push):store.sync_once()
        self.assertTrue(store.pending.is_set(),'Edits during a push need another sync pass')


if __name__=='__main__':unittest.main()
