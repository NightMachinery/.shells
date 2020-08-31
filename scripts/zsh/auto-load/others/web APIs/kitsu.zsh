function kitsu-getall() {
    local query="${*:-fullmetal alchemist}"

    kitsu.js "$query" | jq '[.data[] | { tlg_title: .canonicalTitle, tlg_content: (.canonicalTitle + "\n" + (.abbreviatedTitles | join("\n")) + "\n\nRating Rank: " + (.ratingRank|tostring) + "\nRating: " + .averageRating + "\nPop Rank: " + (.popularityRank | tostring) + "\nGenres: " +([ .genres.data[] | .name ] | join(", ")) + "\nEpisode Count: " + (.episodeCount | tostring) + "\nStatus: " +.status + "\n" + .startDate + " to " + .endDate + "\n" + .ageRatingGuide + "\n" + (.links.self | sub("/api/edge";"")) + "\n\n" + .description) } ]'
}
function kitsu-get() {
    local i="${kitsu_get_i:-0}"
    local query="${*:-fullmetal alchemist}"

    kitsu-getall "$query" | jqm ".[$i].tlg_content"
}
