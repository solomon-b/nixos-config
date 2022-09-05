{ ... }:

{
  services.youtube-dl = {
    enable = true;
    dataDir = "/mnt/media/YouTube";
    onCalendar = "Sat, 04:00:00";
    youtubes = [
      # Math
      "https://www.youtube.com/c/3blue1brown"                    # 3Blue1Brown
      #"https://www.youtube.com/c/Mathologer"                     # Mathologer 
      #"https://www.youtube.com/c/numberphile"                    # Numberphile 
      ## Fabrication
      #"https://www.youtube.com/c/Advoko"                         # Advoko MAKES
      #"https://www.youtube.com/c/colinfurze"                     # colinfurze 
      #"https://www.youtube.com/c/FireballTool"                   # Fireball Tool
      #"https://www.youtube.com/c/frankhowarth"                   # frank howarth
      #"https://www.youtube.com/channel/UCWHni4UsUiuo-oMixymV5ow" # HOMEMADE MADNESS
      #"https://www.youtube.com/c/MadeinPolandChannel"            # Made in Poland
      #"https://www.youtube.com/c/MakerB"                         # Maker B
      #"https://www.youtube.com/channel/UCAL3JXZSzSm8AlZyD3nQdBA" # Primitive Technology
      ## Computer Science
      #"https://www.youtube.com/user/ATTTechChannel"              # AT&T Tech Channel
      #"https://www.youtube.com/user/DrBartosz"                   # Bartosz Milewski
      #"https://www.youtube.com/channel/UCCL46pxWWtfhK3TxL55ybeQ" # Bay Area Haskell
      #"https://www.youtube.com/channel/UCiGOzKde1rlvzEB7J73gvrQ" # Chalmers Functional Programming Seminar Series
      #"https://www.youtube.com/user/Computerphile"               # Computerphile 
      #"https://www.youtube.com/channel/UC1j4K875i9geeirW_kEKRCQ" # Denotational Design
      #"https://www.youtube.com/c/EdwardKmett1"                   # Edward Kmett
      #"https://www.youtube.com/c/ICFPVideo"                      # ICFP Video
      #"https://www.youtube.com/c/LukeSmithxyz"                   # Luke Smith
      #"https://www.youtube.com/channel/UCvsakfIy5ObsDHVsEkG2jMw" # Mathematically Structured Functional Programming
      #"https://www.youtube.com/c/NixCon"                         # NixCon 
      #"https://www.youtube.com/c/OPLSS"                          # OPLSS 
      #"https://www.youtube.com/user/philipwadler"                # Philip Wadler
      ## Electronics
      #"https://www.youtube.com/c/BenEater"                       # Ben Eater
      #"https://www.youtube.com/c/EevblogDave"                    # EEVblog 
      ## Economics
      #"https://www.youtube.com/c/BenFelixCSI"
      ## Gardening
      #"https://www.youtube.com/c/ChrisTrumpSoilSteward"          # Chris Trump
      #"https://www.youtube.com/c/CrimePaysButBotanyDoesnt"       # Crime Pays But Botany Doesn't
      #"https://www.youtube.com/c/DanOrganicFoodForest"           # Dan Permaculture Food Forest
      #"https://www.youtube.com/c/DiegoFooter"                    # Diego Footer
      #"https://www.youtube.com/c/edibleacres"                    # EdibleAcres 
      #"https://www.youtube.com/user/homesteadonomics"            # homesteadonomics 
      #"https://www.youtube.com/c/Jadamorganicmedia"              # JADAM Organic Farming
      #"https://www.youtube.com/c/PureKNFDrake"                   # PureKNF Dr. Drake
      ## Environmentalism
      #"https://www.youtube.com/c/ClimateTown"                    # Climate Town
      ## Misc Science
      #"https://www.youtube.com/user/theCodyReeder"               # Cody'sLab
      #"https://www.youtube.com/c/universetodayvids"              # Fraser Cain
      #"https://www.youtube.com/c/HazelChem"                      # Hazel Chem
      #"https://www.youtube.com/c/NurdRage"                       # NurdRage 
      #"https://www.youtube.com/c/pbsinfiniteseries"              # PBS Infinite Series
      #"https://www.youtube.com/c/QuantaScienceChannel"           # Quanta Magazine
      ## Politics
      #"https://www.youtube.com/c/ContraPoints"                   # ContraPoints 
      ## Media Studies
      #"https://www.youtube.com/c/FoldingIdeas"                   # Folding Ideas
      ## Technology
      #"https://www.youtube.com/channel/UCZFipeZtQM5CKUjx6grh54g" # Isaac Arthur
      #"https://www.youtube.com/motherboard"                      # Motherboard 
      ## Cooking
      #"https://www.youtube.com/c/ProHomeCooks"                   # Pro Home Cooks
    ];
    configFlags = [
      "-i"
      "-o \"%(uploader)s (%(uploader_id)s)/%(upload_date)s - %(title)s - [%(resolution)s] [%(id)s].%(ext)s\""
      
      # Uniform Format
      "--merge-output-format mkv"
      
      # Get All Subs to SRT
      "--write-subs"
      "--sub-langs all"
      "--convert-subs srt"
      
      # Get metadata
      "--add-metadata"
      "--write-description"
      "--write-thumbnail"
      # Debug
      "-v"
    ];
  };
}
