-- Native scripting dictionaries differ even among Chromium-based browsers.
local R={version="1.0.0"}
R.browsers={
 {id="brave",name="Brave",bundleID="com.brave.Browser",kind="chromium",nativeMRU=true},
 {id="chrome",name="Chrome",bundleID="com.google.Chrome",kind="chromium"},
 {id="edge",name="Edge",bundleID="com.microsoft.edgemac",kind="chromium"},
 {id="safari",name="Safari",bundleID="com.apple.Safari",kind="safari"},
 {id="arc",name="Arc",bundleID="company.thebrowser.Browser",kind="arc"},
 {id="dia",name="Dia",bundleID="company.thebrowser.dia",kind="dia"},
 {id="firefox",name="Firefox",bundleID="org.mozilla.firefox",kind="firefox"},
}
function R.find(bundleID)
 for _,browser in ipairs(R.browsers) do if browser.bundleID==bundleID then return browser end end
end
return R
