sleep-neon()  {
    mdoc 'Usage: sleep-neon <seconds>' MAGIC
    local inter="$1"
    chalk-animation neon --duration $((inter*1000)) "Sleeping for $inter seconds..."
}
