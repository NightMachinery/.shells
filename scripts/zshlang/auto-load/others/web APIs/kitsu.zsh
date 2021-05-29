function kitsu-getall() {
    local query="${*:-fullmetal alchemist}"
    local other=''
    test -n "$kitsu_f" && other=' + "\n\n" + .description '

    kitsu.js "$query" | jq '[.data[] | { 
        tlg_title: .canonicalTitle,
        # tlg_img: .posterImage.original,
        tlg_img_thumb: .posterImage.tiny,
        tlg_parsemode: "md",
        tlg_content: ("[🦚](" + .posterImage.original + ") " +
        "[" + .canonicalTitle + "](" + (.links.self | sub("/api/edge";"")) + ")\n" +
        ([.abbreviatedTitles[] | ("🦚 " + .)] | join("\n")) + # There was a bug when I didnot use join, that duplicated some entries. I guess it was because I had not converted the array into a string.
        "\n\nRating Rank: " + (.ratingRank|tostring) + 
        "\nRating: " + .averageRating + 
        "\nPop Rank: " + (.popularityRank | tostring) + 
        "\nGenres: " +([ .genres.data[] | .name ] | join(", ")) + 
        "\nEpisode Count: " + (.episodeCount | tostring) + 
        "\nStatus: " +.status + "\n" + .startDate + " to " + .endDate + "\n" + 
        .ageRatingGuide'$other'),

        org_content: (
        "[[" + (.links.self | sub("/api/edge";"")) + "][" + .canonicalTitle + "]]\n:PROPERTIES:\n" +
        ":Title: " + .canonicalTitle +
        (if (.abbreviatedTitles | length) != 0 then
            ("\n" + ([.abbreviatedTitles[] | (":Titles+: " + .)] | join("\n")))
        else
            ""
        end) +
        "\n:RatingRank: " + (.ratingRank|tostring) +
        "\n:Rating: " + .averageRating +
        "\n:PopRank: " + (.popularityRank | tostring) +
        (if (.genres.data | length) != 0 then
          ("\n:Genres: " + ([ .genres.data[] | .name ] | join(" ")))
        else
            ""
        end) +
        "\n:EpisodeCount: " + (.episodeCount | tostring) +
        "\n:Status: " + .status +
        "\n:StartDate: " + .startDate +
        (if (.endDate) then
            "\n:EndDate: " + .endDate
        else
            ""
        end) +
        (if (.ageRatingGuide) then
        "\n:AgeGuide: " + .ageRatingGuide
        else
            ""
        end) +
        "\n:END:\n#+begin_quote\n" + .description + "\n#+end_quote\n"
        )
        } ]'

        #         tlg_content: (.canonicalTitle + "\n" + (.abbreviatedTitles | join("\n")) + "\n\nRating Rank: " + (.ratingRank|tostring) + 
        # "\nRating: " + .averageRating + 
        # "\nPop Rank: " + (.popularityRank | tostring) + 
        # "\nGenres: " +([ .genres.data[] | .name ] | join(", ")) + 
        # "\nEpisode Count: " + (.episodeCount | tostring) + 
        # "\nStatus: " +.status + "\n" + .startDate + " to " + .endDate + "\n" + 
        # .ageRatingGuide + "\n" + 
        # (.links.self | sub("/api/edge";"")) + "\n\n" + .description) 
}

function kitsu-get() {
    local i="${kitsu_get_i:-0}"
    local query="${*:-fullmetal alchemist}"

    kitsu-getall "$query" | jqm ".[$i].tlg_content"
}

function ffkitsu() {
    # local d sel query
    query="${*}"
    if test -z "$query" ; then
        query='fullmetal alchemist'
    fi

    d="$(kitsu-getall "$query")" @TRET

    sel="$(ec "$d" | jq -r '.[] | .tlg_title' | fz -1)" @RET

    ec "$d" | jq -r --arg t "$sel" '.[] | select(.tlg_title == $t) | .org_content' | cat-copy @TRET
}
alias ffkitsu2org='ffkitsu'
