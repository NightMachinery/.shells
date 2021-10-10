function steam-to-json {
    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    eval-memoi 'full-html2' "$url" \
        | html-links-absolutify 'https://store.steampowered.com' \
        | selectors2json.py \
        title "#appHubAppName" '' \
        description_summary '.game_description_snippet' '->org' \
        description '#aboutThisGame' '->org' \
        releaseDate '.release_date .date' '' \
        metacriticScore '#game_area_metascore .score' '' \
        allReviews '#userReviews > .user_reviews_summary_row:nth-child(2) .responsive_reviewdesc' '' \
        recentReviews '#userReviews > .user_reviews_summary_row:nth-child(1) .responsive_reviewdesc' '' \
        tags '.app_tag' '' \
        developers '#developers_list' '->org' \
        publisher '.dev_row div:-soup-contains("Publish")+.summary.column' '' \
        platforms '.game_area_purchase_platform .platform_img' 'attr:class' \
        capabilities '.game_area_details_specs a.name' '' \
        minReqs '.game_area_sys_req_leftCol,.game_area_sys_req_full' '->org' \
        recommendedReqs '.game_area_sys_req_rightCol' '->org' \
        metacriticUrl '#game_area_metalink a' 'attr:href' \
        img '.game_header_image_full' 'attr:src' \
        | jq '.'

    ## old selectors:
    ## deceptive selectors:
    ##
}


function steam-to-org {
    local url="${1:-$(pbpaste)}"
    assert-args url @RET

    steam-to-json "$url" \
        | steam_json_to_org.lisp "$url" \
        | cat-copy-if-tty
}
##
