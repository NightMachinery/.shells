"""Redis connections that know about our ``~/.redis-auth`` secret.

Redis binds to ``127.0.0.1``, which excludes other *hosts* but not processes on
this one -- other users on a shared box, and on a single-user laptop anything
that can reach loopback: a web page that makes the browser POST to port 6379, a
sandboxed build step, a compromised dependency. So redis requires a password;
see ``docs/redis-hardening.md`` and ``h-redis-auth-ensure`` in
``zshlang/basic/redis.zsh``.

``redis.StrictRedis(host='localhost', port=6379, db=0)`` -- what these scripts
used to construct directly -- sends no password and fails with NOAUTH against a
hardened server. Use :func:`redis_client` instead.

Consumers live in ``~/scripts/python/<subdir>/``, so they must put
``~/scripts/python/`` on ``sys.path`` themselves before importing this::

    import os, sys
    sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from libs.redis_client import redis_client

That is safe despite ``~/scripts/python/redis/`` looking like a package named
``redis``: a directory with no ``__init__.py`` is only a namespace *portion*,
and the import machinery keeps scanning ``sys.path`` for a regular package, so
``import redis`` still resolves to the installed library rather than to our
directory of scripts.
"""

from __future__ import annotations

import os
from pathlib import Path

AUTH_FILE = Path.home() / ".redis-auth"


def redis_auth_get() -> str | None:
    """The redis password, or None if this host has no secret.

    ``REDISCLI_AUTH`` first -- the shell exports it, and it is what redis-cli
    reads -- then the file, so that a process which started before the secret
    existed, or inherited a stale environment, still finds it.

    Returning None rather than raising is deliberate: it reproduces the old
    passwordless behaviour exactly, so these scripts keep working on a host
    whose redis has not been hardened.
    """
    secret = os.environ.get("REDISCLI_AUTH") or ""
    if secret:
        return secret

    try:
        #: .strip() because the file may or may not end in a newline, and on
        #: some hosts a CR too; the secret itself is hex, so no legitimate
        #: character can be stripped by accident.
        secret = AUTH_FILE.read_text().strip()
    except OSError:
        return None

    return secret or None


def redis_client(host: str = "localhost", port: int = 6379, db: int = 0, **kwargs):
    """A StrictRedis authenticated with our secret, if there is one."""
    import redis

    kwargs.setdefault("password", redis_auth_get())
    return redis.StrictRedis(host=host, port=port, db=db, **kwargs)
