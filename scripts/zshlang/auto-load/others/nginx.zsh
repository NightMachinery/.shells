function http-static-nginx {
    reval-ec nginx -p . -c "${NIGHTDIR}/launchers/nginx_configs/serve_current_dir.conf"
}
