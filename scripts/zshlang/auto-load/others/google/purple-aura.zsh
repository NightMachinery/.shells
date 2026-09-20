#: Spend and trial status for the personal GCP project used for translation
#: and speech. The project id, billing account, trial dates and export table
#: are private and come from `~/.night-gcp/config.zsh`:
#:   gcp_pa_project, gcp_pa_billing_account, gcp_pa_trial_start (YYYY-MM-DD),
#:   gcp_pa_trial_days, gcp_pa_trial_credit_eur, gcp_pa_billing_table
#: The cost breakdown reads the Cloud Billing export in BigQuery, which the
#: console has to switch on once; until its first rows land, only the trial
#: countdown and the quota are shown.

typeset -g gcp_pa_project="${gcp_pa_project:-}"
typeset -g gcp_pa_billing_account="${gcp_pa_billing_account:-}"
typeset -g gcp_pa_trial_start="${gcp_pa_trial_start:-}"
typeset -g gcp_pa_trial_days="${gcp_pa_trial_days:-90}"
typeset -g gcp_pa_trial_credit_eur="${gcp_pa_trial_credit_eur:-}"
typeset -g gcp_pa_billing_table="${gcp_pa_billing_table:-}"

function purple-aura-spent {
    #: Usage: purple-aura-spent [--json]
    #: Shows spend by service, credit remaining, and days left in the trial.
    ensure-cmd gcloud bq python3 @RET
    [[ -n "${gcp_pa_project}" ]] || { ecerr "$0: set gcp_pa_project in ${gcp_conf_file}" ; return 1 }
    local json_p=''
    [[ "$1" == --json ]] && json_p=y

    local costs='[]'
    if [[ -n "${gcp_pa_billing_table}" ]] ; then
        costs="$(bq --project_id="${gcp_pa_project}" query --quiet --use_legacy_sql=false --format=json \
            "SELECT service.description AS service,
                    ROUND(SUM(cost), 4) AS cost,
                    ROUND(SUM(IFNULL((SELECT SUM(c.amount) FROM UNNEST(credits) c), 0)), 4) AS credits,
                    ANY_VALUE(currency) AS currency
             FROM \`${gcp_pa_billing_table}\`
             WHERE project.id = '${gcp_pa_project}'
             GROUP BY service ORDER BY cost DESC" 2>/dev/null)" || costs='[]'
    fi

    local quota
    quota="$(gcloud alpha services quota list --service=translate.googleapis.com \
        --consumer="projects/${gcp_pa_project}" --format=json 2>/dev/null)" || quota='[]'

    local rich_p=''
    if [[ -z "$json_p" ]] && isOutTty && python3 -c 'import rich' 2>/dev/null ; then
        rich_p=y
    fi

    PA_COSTS="$costs" PA_QUOTA="$quota" PA_PROJECT="${gcp_pa_project}" \
    PA_START="${gcp_pa_trial_start}" PA_DAYS="${gcp_pa_trial_days}" \
    PA_CREDIT="${gcp_pa_trial_credit_eur}" PA_JSON="${json_p}" PA_RICH="${rich_p}" \
    python3 - <<'PY'
import json, os, datetime as dt

costs = json.loads(os.environ["PA_COSTS"] or "[]")
quota = json.loads(os.environ["PA_QUOTA"] or "[]")
start = os.environ["PA_START"]
days = int(os.environ["PA_DAYS"] or 90)
credit = float(os.environ["PA_CREDIT"] or 0)

today = dt.date.today()
expiry = dt.date.fromisoformat(start) + dt.timedelta(days=days) if start else None
left = (expiry - today).days if expiry else None

spent = sum(float(c["cost"]) for c in costs)
credited = -sum(float(c["credits"]) for c in costs)   # credits are negative amounts
currency = next((c["currency"] for c in costs if c.get("currency")), "EUR")
remaining = credit - credited if credit else None

def translate_quota():
    for q in quota:
        if q.get("metric", "").endswith("/default"):
            for lim in q.get("consumerQuotaLimits", []):
                if lim.get("unit") == "1/d/{project}":
                    b = (lim.get("quotaBuckets") or [{}])[0]
                    return b.get("effectiveLimit")
    return None

summary = {
    "project": os.environ["PA_PROJECT"],
    "trial_start": start or None, "trial_expiry": expiry.isoformat() if expiry else None,
    "days_left": left, "spent": round(spent, 2), "covered_by_credits": round(credited, 2),
    "credit_remaining": round(remaining, 2) if remaining is not None else None,
    "currency": currency, "translate_chars_per_day_cap": translate_quota(),
    "by_service": [{"service": c["service"], "cost": float(c["cost"]), "credits": float(c["credits"])} for c in costs],
    "export_has_data": bool(costs),
}

if os.environ["PA_JSON"]:
    print(json.dumps(summary, indent=2)); raise SystemExit

if os.environ["PA_RICH"]:
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
    from rich.progress_bar import ProgressBar
    from rich import box
    con = Console()
    head = f"[bold]{summary['project']}[/]"
    if left is not None:
        tone = "green" if left > 30 else "yellow" if left > 7 else "red"
        head += f"   trial ends [bold {tone}]{expiry}[/] ([{tone}]{left} days left[/])"
    con.print(Panel(head, box=box.ROUNDED, expand=False))
    if credit:
        used = min(max(credited / credit, 0), 1) if credit else 0
        con.print(f"credit  {credited:.2f} of {credit:.2f} {currency} used   remaining [bold]{remaining:.2f}[/]")
        con.print(ProgressBar(total=1, completed=used, width=40), "\n")
    if costs:
        t = Table(box=box.SIMPLE_HEAD, show_edge=False)
        t.add_column("service"); t.add_column("cost", justify="right"); t.add_column("credits", justify="right")
        for c in costs:
            t.add_row(c["service"], f"{float(c['cost']):.4f}", f"{float(c['credits']):.4f}")
        t.add_row("[bold]total[/]", f"[bold]{spent:.4f}[/]", f"[bold]{-credited:.4f}[/]")
        con.print(t)
    else:
        con.print("[dim]no rows in the billing export yet (it lags a day, and must be switched on in the console once)[/]")
    cap = summary["translate_chars_per_day_cap"]
    if cap: con.print(f"\ntranslate quota  [bold]{int(cap):,}[/] characters/day")
else:
    print(f"{summary['project']}  trial ends {summary['trial_expiry']}  ({left} days left)")
    if credit: print(f"credit used {credited:.2f} of {credit:.2f} {currency}, remaining {remaining:.2f}")
    for c in costs: print(f"  {c['service']:40s} {float(c['cost']):9.4f}  credits {float(c['credits']):9.4f}")
    if not costs: print("  no rows in the billing export yet")
    if summary["translate_chars_per_day_cap"]: print(f"translate quota {int(summary['translate_chars_per_day_cap']):,} chars/day")
PY
}
