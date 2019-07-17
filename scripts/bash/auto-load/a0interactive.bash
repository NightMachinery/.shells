[[ $- == *i* ]] && {
eval $(thefuck --alias fu) #should be evaled before aliases

eval "$(fasd --init auto)"
}
