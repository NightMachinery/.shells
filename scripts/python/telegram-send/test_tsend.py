"""Unit tests for tsend's rich messages, with fake Telegram clients.

Run: python3 -m pytest python/telegram-send/test_tsend.py -q -p no:cacheprovider

Nothing here connects to Telegram. The tests that build real rich-message
requests need a Telethon with rich messages (1.44+) and skip themselves
otherwise; the rest run on any version.
"""

import asyncio
import importlib.util
import json
import os
import types as pytypes
from pathlib import Path
from unittest import mock

import pytest
from telethon import errors as tl_errors
from telethon.tl import functions as tl_functions
from telethon.tl import types as tl_types

TSEND_PATH = Path(__file__).with_name("tsend.py")
RICH_TELETHON_P = hasattr(tl_types, "InputRichMessageMarkdown")
needs_rich_telethon = pytest.mark.skipif(
    not RICH_TELETHON_P, reason="this Telethon has no rich messages"
)
needs_old_telethon = pytest.mark.skipif(
    RICH_TELETHON_P, reason="this Telethon already has rich messages"
)


@pytest.fixture(scope="module")
def ts():
    #: Fake credentials, so importing never reads ~/.telegram-config, and a
    #: session path that cannot exist, so nothing can touch a real session.
    env = dict(
        TSEND_BACKEND="2",
        TSEND_TOKEN="test-token",
        TELEGRAM_API_ID="1",
        TELEGRAM_API_HASH="test-hash",
        TELEGRAM_SESSION="/nonexistent/tsend-test",
    )
    with mock.patch.dict(os.environ, env):
        spec = importlib.util.spec_from_file_location("tsend_under_test", TSEND_PATH)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    return module


@pytest.fixture
def no_backoff(ts, monkeypatch):
    calls = []

    async def handle(e, attempt, max_retries, verbosity):
        calls.append(e)

    monkeypatch.setattr(ts, "handle", handle)
    return calls


def run(coro):
    return asyncio.run(coro)


def arguments(ts, argv):
    return ts.parse_tsend(argv)


#: Parsing and checks


def test_parse_mode_rich_is_accepted_by_send_and_edit(ts):
    assert (
        arguments(ts, ["--parse-mode=rich", "--", "d", "m"])["--parse-mode"] == "rich"
    )
    edit = arguments(ts, ["edit", "--parse-mode=rich", "--", "d", "4", "m"])
    assert edit["--parse-mode"] == "rich"
    assert ts.parse_mode_rich_p("RICH")
    assert not ts.parse_mode_rich_p(None)
    assert not ts.parse_mode_rich_p("md")


def test_rich_markdown_check_trims_blank_lines_but_keeps_indentation(ts):
    assert ts.rich_markdown_check("\n  \n    code\n\n") == "    code"


def test_rich_markdown_check_rejects_empty(ts):
    with pytest.raises(SystemExit, match="cannot be empty"):
        ts.rich_markdown_check(" \n\t\n")


def test_rich_markdown_check_limit(ts):
    limit = ts.RICH_MESSAGE_MAX_CHARS
    assert len(ts.rich_markdown_check("x" * limit)) == limit
    with pytest.raises(SystemExit, match=f"{limit + 1}/{limit}.*--parse-mode=md"):
        ts.rich_markdown_check("x" * (limit + 1))


def test_telethon_rich_message_names_the_fix_when_missing(ts):
    with pytest.raises(SystemExit) as exc:
        ts.telethon_rich_message("x", tl_types=pytypes.SimpleNamespace())
    text = str(exc.value)
    assert "Telethon 1.44" in text and "TSEND_BACKEND=2" in text


@needs_rich_telethon
def test_telethon_rich_message_builds_markdown(ts):
    rich = ts.telethon_rich_message("# hi")
    assert isinstance(rich, tl_types.InputRichMessageMarkdown)
    assert rich.markdown == "# hi"


def test_record_sent_takes_bare_ids(ts):
    sent_ids = []
    ts.record_sent(sent_ids, 7)
    ts.record_sent(sent_ids, None)
    ts.record_sent(sent_ids, True)
    assert sent_ids == [7]


#: Telethon backend


def test_sent_message_id_from_short_sent_message(ts):
    result = tl_types.UpdateShortSentMessage(id=42, pts=1, pts_count=1, date=None)
    assert ts.telethon_sent_message_id(result, random_id=5) == 42


def test_sent_message_id_from_updates(ts):
    message = tl_types.Message(id=9, peer_id=tl_types.PeerUser(1), message="")
    updates = [
        tl_types.UpdateMessageID(id=8, random_id=4),
        tl_types.UpdateMessageID(id=7, random_id=5),
        tl_types.UpdateNewMessage(message=message, pts=1, pts_count=1),
    ]
    result = tl_types.Updates(updates=updates, users=[], chats=[], date=None, seq=0)
    assert ts.telethon_sent_message_id(result, random_id=5) == 7
    #: With no UpdateMessageID for our random_id, the new message decides.
    assert ts.telethon_sent_message_id(result, random_id=6) == 9
    assert ts.telethon_sent_message_id(object(), random_id=5) is None


class FakeTelethonClient:
    """Answers raw requests from `answers` (an exception is raised, anything else
    returned) and records every request it was given."""

    def __init__(self, *answers):
        self.answers = list(answers)
        self.requests = []

    async def __call__(self, request):
        self.requests.append(request)
        answer = self.answers.pop(0)
        if isinstance(answer, BaseException):
            raise answer
        return answer


@needs_rich_telethon
def test_telethon_send_rich_prints_id_from_short_answer(ts, no_backoff):
    client = FakeTelethonClient(
        tl_types.UpdateShortSentMessage(id=42, pts=1, pts_count=1, date=None)
    )
    rich = ts.telethon_rich_message("| a |\n|---|\n| 1 |")
    sent_ids = []
    message_id = run(
        ts.telethon_send_rich(client, "peer", rich_message=rich, sent_ids=sent_ids)
    )
    assert message_id == 42 and sent_ids == [42]
    (request,) = client.requests
    assert request.message == ""
    assert request.rich_message is rich
    assert request.no_webpage


@needs_rich_telethon
def test_telethon_send_rich_fails_at_once_on_rejection(ts, no_backoff):
    rejection = tl_errors.BadRequestError(request=None, message="RICH_MESSAGE_INVALID")
    client = FakeTelethonClient(rejection, rejection)
    rich = ts.telethon_rich_message("x")
    with pytest.raises(ts.SendFailed, match="RICH_MESSAGE_INVALID"):
        run(ts.telethon_send_rich(client, "peer", rich_message=rich))
    assert len(client.requests) == 1 and not no_backoff


@needs_rich_telethon
def test_telethon_send_rich_retries_transient_errors(ts, no_backoff):
    client = FakeTelethonClient(
        ConnectionError("flaky"),
        tl_types.UpdateShortSentMessage(id=3, pts=1, pts_count=1, date=None),
    )
    rich = ts.telethon_rich_message("x")
    assert run(ts.telethon_send_rich(client, "peer", rich_message=rich)) == 3
    assert len(client.requests) == 2 and len(no_backoff) == 1


@needs_rich_telethon
def test_telethon_edit_rich_sends_raw_edit(ts, no_backoff):
    client = FakeTelethonClient(
        tl_types.Updates(updates=[], users=[], chats=[], date=None, seq=0)
    )
    rich = ts.telethon_rich_message("## new")
    run(ts.telethon_edit(client, "peer", 12, "## new", rich_message=rich))
    (request,) = client.requests
    assert isinstance(request, tl_functions.messages.EditMessageRequest)
    assert (request.id, request.message, request.rich_message) == (12, "", rich)


@needs_rich_telethon
def test_telethon_edit_rich_not_modified_is_success(ts, no_backoff):
    client = FakeTelethonClient(tl_errors.MessageNotModifiedError(request=None))
    rich = ts.telethon_rich_message("same")
    assert run(ts.telethon_edit(client, "peer", 12, "same", rich_message=rich)) is None


class FakeTelethonSession:
    """Stands in for TelegramClient in tsend(): authorized, and never online."""

    instances = []

    def __init__(self, **kwargs):
        self.kwargs = kwargs
        self.requests = []
        FakeTelethonSession.instances.append(self)

    async def connect(self):
        pass

    async def get_me(self):
        return pytypes.SimpleNamespace(bot=True)

    async def __call__(self, request):
        self.requests.append(request)
        return tl_types.UpdateShortSentMessage(id=77, pts=1, pts_count=1, date=None)

    def disconnect(self):
        return None


@pytest.fixture
def fake_telethon(ts, monkeypatch):
    import telethon

    FakeTelethonSession.instances = []
    monkeypatch.setattr(telethon, "TelegramClient", FakeTelethonSession)
    monkeypatch.setattr(ts, "backend", 1)
    return FakeTelethonSession


@needs_rich_telethon
def test_tsend_telethon_rich_prints_the_id(ts, fake_telethon, capsys):
    argv = ["--parse-mode=rich", "--print-ids", "--", "123", "# Title\n\n| a |\n|---|"]
    run(ts.tsend(arguments(ts, argv)))
    (client,) = fake_telethon.instances
    (request,) = client.requests
    assert request.rich_message.markdown == "# Title\n\n| a |\n|---|"
    assert capsys.readouterr().out.split() == ["77"]


@needs_old_telethon
def test_tsend_telethon_rich_fails_before_connecting(ts, fake_telethon):
    with pytest.raises(SystemExit, match="TSEND_BACKEND=2"):
        run(ts.tsend(arguments(ts, ["--parse-mode=rich", "--", "123", "x"])))
    assert fake_telethon.instances == []


def test_tsend_rich_rejects_files_before_connecting(ts, fake_telethon):
    argv = ["--parse-mode=rich", "--file=a.png", "--", "123", "x"]
    with pytest.raises(SystemExit, match="--file"):
        run(ts.tsend(arguments(ts, argv)))
    assert fake_telethon.instances == []


def test_tsend_rich_rejects_long_text_before_connecting(ts, fake_telethon):
    text = "x" * (ts.RICH_MESSAGE_MAX_CHARS + 1)
    with pytest.raises(SystemExit, match="too long"):
        run(ts.tsend(arguments(ts, ["--parse-mode=rich", "--", "123", text])))
    assert fake_telethon.instances == []


#: Bot API backend


class FakeBot:
    """Answers PTB calls from `answers` and records each (method, kwargs)."""

    def __init__(self, *answers):
        self.answers = list(answers)
        self.calls = []

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc):
        return False

    async def _answer(self, method, kwargs):
        self.calls.append((method, kwargs))
        answer = self.answers.pop(0)
        if isinstance(answer, BaseException):
            raise answer
        return answer

    async def do_api_request(self, endpoint, api_kwargs=None, return_type=None):
        return await self._answer(endpoint, dict(api_kwargs or {}))

    async def edit_message_text(self, **kwargs):
        return await self._answer("edit_message_text", kwargs)

    async def delete_message(self, **kwargs):
        return await self._answer("delete_message", kwargs)


def bad_request(text):
    from telegram.error import BadRequest

    return BadRequest(text)


def test_ptb_send_rich_calls_send_rich_message(ts, no_backoff):
    bot = FakeBot(dict(message_id=5, chat=dict(id=1, type="private"), date=0))
    sent_ids = []
    message_id = run(ts.ptb_send_rich(bot, 1, markdown="# hi", sent_ids=sent_ids))
    assert message_id == 5 and sent_ids == [5]
    assert bot.calls == [
        (
            "sendRichMessage",
            dict(
                chat_id=1,
                rich_message=dict(markdown="# hi"),
                link_preview_options=dict(is_disabled=True),
            ),
        )
    ]


def test_ptb_send_rich_link_preview(ts, no_backoff):
    bot = FakeBot(dict(message_id=5))
    run(ts.ptb_send_rich(bot, 1, markdown="x", link_preview=True))
    assert "link_preview_options" not in bot.calls[0][1]


def test_ptb_send_rich_fails_at_once_on_bad_request(ts, no_backoff):
    bot = FakeBot(bad_request("Bad Request: too many blocks"))
    with pytest.raises(ts.SendFailed, match="(?i)too many blocks"):
        run(ts.ptb_send_rich(bot, 1, markdown="x"))
    assert len(bot.calls) == 1 and not no_backoff


def test_ptb_send_rich_retries_transient_errors(ts, no_backoff):
    from telegram.error import TimedOut

    bot = FakeBot(TimedOut(), dict(message_id=6))
    assert run(ts.ptb_send_rich(bot, 1, markdown="x")) == 6
    assert len(bot.calls) == 2 and len(no_backoff) == 1


def test_ptb_edit_rich_passes_rich_message_instead_of_text(ts, no_backoff):
    bot = FakeBot(True)
    run(ts.ptb_edit(bot, 1, 9, "## new", rich_markdown="## new"))
    ((method, kwargs),) = bot.calls
    assert method == "edit_message_text"
    assert kwargs["text"] is None
    assert kwargs["api_kwargs"] == dict(rich_message=dict(markdown="## new"))
    assert (kwargs["chat_id"], kwargs["message_id"]) == (1, 9)


def test_ptb_edit_rich_not_modified_is_success(ts, no_backoff):
    bot = FakeBot(bad_request("Bad Request: message is not modified"))
    assert run(ts.ptb_edit(bot, 1, 9, "x", rich_markdown="x")) is None


def test_ptb_edit_rich_fails_at_once_on_bad_request(ts, no_backoff):
    bot = FakeBot(bad_request("Bad Request: can't parse rich message"))
    with pytest.raises(ts.SendFailed, match="Cannot edit 9 in 1"):
        run(ts.ptb_edit(bot, 1, 9, "x", rich_markdown="x"))
    assert not no_backoff


def test_ptb_edit_classic_is_unchanged(ts, no_backoff):
    bot = FakeBot(bad_request("Bad Request: something odd"), True)
    run(ts.ptb_edit(bot, 1, 9, "text", parse_mode="HTML"))
    #: A classic edit keeps retrying errors it does not recognize.
    assert len(bot.calls) == 2 and len(no_backoff) == 1
    assert bot.calls[-1] == (
        "edit_message_text",
        dict(text="text", chat_id=1, message_id=9, parse_mode="HTML"),
    )
    bot = FakeBot(bad_request("Bad Request: message to edit not found"))
    with pytest.raises(ts.SendFailed, match="Cannot edit 9 in 1"):
        run(ts.ptb_edit(bot, 1, 9, "text"))


def test_ptb_delete_fails_at_once_on_missing_message(ts, no_backoff):
    bot = FakeBot(bad_request("Bad Request: message to delete not found"))
    with pytest.raises(ts.SendFailed, match="Cannot delete from 1"):
        run(ts.ptb_delete(bot, 1, [9]))
    assert not no_backoff


@pytest.fixture
def fake_ptb(ts, monkeypatch):
    bots = []

    def make(*answers):
        bot = FakeBot(*answers)
        bots.append(bot)

        async def from_env():
            return bot

        monkeypatch.setattr(ts, "_ptb_bot_from_env", from_env)
        monkeypatch.setattr(ts, "backend", 2)
        return bot

    return make


def test_tsend_bot_api_rich_prints_the_id(ts, fake_ptb, capsys):
    bot = fake_ptb(dict(message_id=31))
    argv = ["--parse-mode=rich", "--print-ids", "--", "123", "\n# Title\n"]
    run(ts.tsend(arguments(ts, argv)))
    ((method, kwargs),) = bot.calls
    assert method == "sendRichMessage"
    assert kwargs["chat_id"] == 123
    assert kwargs["rich_message"] == dict(markdown="# Title")
    assert capsys.readouterr().out.split() == ["31"]


def test_tsend_bot_api_rich_edit(ts, fake_ptb):
    bot = fake_ptb(True)
    run(
        ts.tsend(
            arguments(ts, ["edit", "--parse-mode=rich", "--", "123", "9", "# New"])
        )
    )
    ((method, kwargs),) = bot.calls
    assert method == "edit_message_text"
    assert kwargs["api_kwargs"] == dict(rich_message=dict(markdown="# New"))


def test_tsend_rich_edit_allows_more_than_classic_limit(ts, fake_ptb):
    bot = fake_ptb(True)
    text = "x" * 5000
    run(ts.tsend(arguments(ts, ["edit", "--parse-mode=rich", "--", "123", "9", text])))
    assert bot.calls[0][1]["api_kwargs"]["rich_message"]["markdown"] == text


class RecordingRequest:
    """A PTB request backend that records each call's endpoint and JSON
    parameters and answers with `result`, so real PTB serialization is tested
    without a network."""

    def __new__(cls, result):
        from telegram.request import BaseRequest

        class Recording(BaseRequest):
            async def initialize(self):
                pass

            async def shutdown(self):
                pass

            async def do_request(self, url, method, request_data=None, **timeouts):
                parameters = request_data.json_parameters if request_data else {}
                self.calls.append((url.rsplit("/", 1)[-1], parameters))
                return 200, json.dumps(dict(ok=True, result=result)).encode()

        request = Recording()
        request.calls = []
        return request


def real_bot(result):
    import telegram

    request = RecordingRequest(result)
    bot = telegram.Bot("123:test-token", request=request, get_updates_request=request)
    return bot, request.calls


def test_ptb_send_rich_wire_format(ts, no_backoff):
    message = dict(message_id=5, date=0, chat=dict(id=1, type="private"))
    bot, calls = real_bot(message)
    assert run(ts.ptb_send_rich(bot, 1, markdown="# hi")) == 5
    ((endpoint, parameters),) = calls
    assert endpoint == "sendRichMessage"
    assert json.loads(parameters["rich_message"]) == dict(markdown="# hi")
    assert json.loads(parameters["link_preview_options"]) == dict(is_disabled=True)
    assert parameters["chat_id"] == "1"


def test_ptb_edit_rich_wire_format(ts, no_backoff):
    message = dict(message_id=9, date=0, chat=dict(id=1, type="private"))
    bot, calls = real_bot(message)
    run(ts.ptb_edit(bot, 1, 9, "## new", rich_markdown="## new"))
    ((endpoint, parameters),) = calls
    assert endpoint == "editMessageText"
    assert "text" not in parameters and "parse_mode" not in parameters
    assert json.loads(parameters["rich_message"]) == dict(markdown="## new")
    assert (parameters["chat_id"], parameters["message_id"]) == ("1", "9")
