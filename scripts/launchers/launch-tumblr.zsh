#!/usr/bin/env zsh

typeset -a tumblr_art_misc=( 'https://sugarcoated-izmi69.tumblr.com/rss' 'https://art.tumblr.com/rss' 'https://thecollectibles.tumblr.com/rss' 'https://supersonicart.com/rss' 'https://theonlymagicleftisart.tumblr.com/rss' )

typeset -a tumblr_modern_art=( 'https://nycartscene.info/rss' 'https://contemporaryartdaily.tumblr.com/rss' )

typeset -a tumblr_illustration=( 'https://maruti-bitamin.tumblr.com/rss' 'https://zandraart.tumblr.com/rss' 'https://1000drawings.tumblr.com/rss' )

typeset -a tumblr_anime=( 'https://animefangirl00.tumblr.com/rss' 'https://wallpaperaday.tumblr.com/rss' 'https://anime-cloud.tumblr.com/rss' 'https://anime-wallpapers.tumblr.com/rss' )

typeset -a tumblr_anime_gif=( 'https://worldoro.tumblr.com/rss' 'https://akaribaby.tumblr.com/rss' 'https://fursteinger.tumblr.com/rss' 'https://0animeaesthetic0.tumblr.com/rss' )

typeset -a tumblr_anime_nsfw=( 'https://yoyoyopt.tumblr.com/rss' )

typeset -a tumblr_food=( 'https://officebento.tumblr.com/rss' 'https://www.foodfuck.net/rss' 'https://yummyfoooooood.tumblr.com/rss' 'https://everybody-loves-to-eat.tumblr.com/rss' 'https://daily-deliciousness.tumblr.com/rss' )

typeset -a tumblr_food_sweets=( 'https://honeyrolls.tumblr.com/rss' )

typeset -a tumblr_food_healthy=( 'https://the-eatjoyfully-posts.tumblr.com/rss' 'https://thefruitmarket.tumblr.com/rss' 'https://cherrysnobs.com/rss' 'https://fruit-power.tumblr.com/rss' )

typeset -a tumblr_photos=( 'https://boho-vibes.tumblr.com/rss' 'https://rosesofthetwilight.tumblr.com/rss' )

typeset -a tumblr_photos_nature=( 'https://bambuita.tumblr.com/rss' )

tmuxnewsh2 rss-tumblr rt_skip=$rt_skip rt_eud=1 rt_notel=y rt_duplicates_key=tumblr rt_e=tumblr2tlg $proxyenv rss-tsend $tumblr_art_misc[@] $tumblr_modern_art[@] $tumblr_illustration[@] $tumblr_anime[@] $tumblr_anime_gif[@] $tumblr_anime_nsfw[@] $tumblr_food[@] $tumblr_food_sweets[@] $tumblr_food_healthy[@] $tumblr_photos[@] $tumblr_photos_nature[@]

# 'https://theartofanimation.tumblr.com/rss' posts too much
