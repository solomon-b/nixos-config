{ ... }:

{
  services.podcast-dl = {
    enable = true;
    dataDir = "/mnt/media/Podcasts/";
    podcasts = [
      "https://data.bff.fm/shows/tapecase.rss"
      "https://malicious.life/feed/podcast"
      "https://feed.podbean.com/palladiummag/feed.xml"
      "http://feeds.wnyc.org/onthemedia"
      "https://thehomelab.show/feed"
      "https://feeds.buzzsprout.com/1817535.rss"
      "https://feed.podbean.com/expandingmind/feed.xml"
      "https://lexfridman.com/feed/podcast/"
      "https://feeds.simplecast.com/bdb_rip_"
      "https://changelog.fm/rss"
      "https://changelog.com/brainscience/feed"
    ];
  };
}
