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
        .ageRatingGuide'$other') 
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
