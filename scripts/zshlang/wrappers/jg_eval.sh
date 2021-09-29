#!/usr/bin/env dash

if test "$1" = '-c' ; then
   shift
fi

cmd="$*"
json_output="${jg_eval_sh_json_output}"
quote="$jg_eval_sh_quote"
session="${jg_eval_sh_session}"
kernel_name="${jg_eval_sh_kernel_name}"
kernel_session="${jg_eval_sh_kernel_session}"
kernel_id="${jg_eval_sh_kernel_id}"
nolog="${jg_eval_sh_nolog}"
endpoint="${JUPYTERGARDEN_Endpoint:-http://127.0.0.1:${JUPYTERGARDEN_PORT:-7330}}/eval/"
if test -n "$nolog" ; then
   endpoint="${endpoint}nolog/"
fi


if test -e "$cmd" ; then
   cmd="$(cat "$cmd")" || {
      echo "jg_eval: bad input"
      exit 1
   }
fi

if test -z "$quote" ; then
   req_json="{\"cmd\":\"$cmd\", \"kernel_id\":\"$kernel_id\", \"kernel_name\":\"$kernel_name\", \"kernel_session\":\"${kernel_session}\", \"session\":\"$session\", \"json_output\":\"${json_output}\", \"nolog\":\"$nolog\"}"
else
    # @todoing
    req_json="$(printf -- "%s" "$cmd" | jq --raw-input --slurp --null-input --compact-output --arg kernel_id "$kernel_id" --arg kernel_name "$kernel_name" --arg kernel_session "${kernel_session}" --arg session "$session" --arg json_output "$json_output" --arg nolog "$nolog" 'inputs as $i | {"cmd": $i, "kernel_id": $kernel_id, "kernel_name": $kernel_name, "kernel_session": $kernel_session, "session": $session, "json_output": $json_output, "nolog": $nolog}')"
fi

if test -n "$DEBUGME" ; then
   printf -- "%s" "$req_json" | jq . >&2
fi

printf -- "%s" "$req_json" | curl --fail --silent --location --header "Content-Type: application/json" --request POST --data '@-' $endpoint
